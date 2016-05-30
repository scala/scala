package scala.tools.reflect

import scala.reflect.macros.runtime.Context
import scala.collection.mutable.{ ListBuffer, Stack }
import scala.reflect.internal.util.Position
import scala.PartialFunction.cond
import scala.util.matching.Regex.Match

import java.util.Formattable

abstract class FormatInterpolator {
  val c: Context
  val global: c.universe.type = c.universe

  import c.universe.{ Match => _, _ }
  import definitions._
  import treeInfo.Applied

  @inline private def truly(body: => Unit): Boolean = { body ; true }
  @inline private def falsely(body: => Unit): Boolean = { body ; false }

  private def fail(msg: String) = c.abort(c.enclosingPosition, msg)
  private def bail(msg: String) = global.abort(msg)

  def interpolate: Tree = c.macroApplication match {
    //case q"$_(..$parts).f(..$args)" =>
    case Applied(Select(Apply(_, parts), _), _, argss) =>
      val args = argss.flatten
      def badlyInvoked = (parts.length != args.length + 1) && truly {
        def because(s: String) = s"too $s arguments for interpolated string"
        val (p, msg) =
          if (parts.length == 0) (c.prefix.tree.pos, "there are no parts")
          else if (args.length + 1 < parts.length)
            (if (args.isEmpty) c.enclosingPosition else args.last.pos, because("few"))
          else (args(parts.length-1).pos, because("many"))
        c.abort(p, msg)
      }
      if (badlyInvoked) c.macroApplication else interpolated(parts, args)
    case other =>
      bail(s"Unexpected application ${showRaw(other)}")
      other
  }

  /** Every part except the first must begin with a conversion for
   *  the arg that preceded it. If the conversion is missing, "%s"
   *  is inserted.
   *
   *  In any other position, the only permissible conversions are
   *  the literals (%% and %n) or an index reference (%1$ or %<).
   *
   *  A conversion specifier has the form:
   *
   *  [index$][flags][width][.precision]conversion
   *
   *  1) "...${smth}" => okay, equivalent to "...${smth}%s"
   *  2) "...${smth}blahblah" => okay, equivalent to "...${smth}%sblahblah"
   *  3) "...${smth}%" => error
   *  4) "...${smth}%n" => okay, equivalent to "...${smth}%s%n"
   *  5) "...${smth}%%" => okay, equivalent to "...${smth}%s%%"
   *  6) "...${smth}[%legalJavaConversion]" => okay*
   *  7) "...${smth}[%illegalJavaConversion]" => error
   *  *Legal according to [[http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Formatter.html]]
   */
  def interpolated(parts: List[Tree], args: List[Tree]) = {
    val fstring  = new StringBuilder
    val evals    = ListBuffer[ValDef]()
    val ids      = ListBuffer[Ident]()
    val argStack = Stack(args: _*)

    // create a tmp val and add it to the ids passed to format
    def defval(value: Tree, tpe: Type): Unit = {
      val freshName = TermName(c.freshName("arg$"))
      evals += ValDef(Modifiers(), freshName, TypeTree(tpe) setPos value.pos.focus, value) setPos value.pos
      ids += Ident(freshName)
    }
    // Append the nth part to the string builder, possibly prepending an omitted %s first.
    // Sanity-check the % fields in this part.
    def copyPart(part: Tree, n: Int): Unit = {
      import SpecifierGroups.{ Spec, Index }
      val s0 = part match {
        case Literal(Constant(x: String)) => x
        case _ => throw new IllegalArgumentException("internal error: argument parts must be a list of string literals")
      }
      def escapeHatch: PartialFunction[Throwable, String] = {
        // trailing backslash, octal escape, or other
        case e: StringContext.InvalidEscapeException =>
          def errPoint = part.pos withPoint (part.pos.point + e.index)
          def octalOf(c: Char) = Character.digit(c, 8)
          def alt = {
            def altOf(i: Int) = i match {
              case '\b' => "\\b"
              case '\t' => "\\t"
              case '\n' => "\\n"
              case '\f' => "\\f"
              case '\r' => "\\r"
              case '\"' => "${'\"'}" /* avoid lint warn */ +
                " or a triple-quoted literal \"\"\"with embedded \" or \\u0022\"\"\""  // $" in future
              case '\'' => "'"
              case '\\' => """\\"""
              case x    => "\\u%04x" format x
            }
            val suggest = {
              val r = "([0-7]{1,3}).*".r
              (s0 drop e.index + 1) match {
                case r(n) => altOf { (0 /: n) { case (a, o) => (8 * a) + (o - '0') } }
                case _    => ""
              }
            }
            val txt =
              if ("" == suggest) ""
              else s", use $suggest instead"
            txt
          }
          def badOctal = {
            def msg(what: String) = s"Octal escape literals are $what$alt."
            if (settings.future) {
              c.error(errPoint, msg("unsupported"))
              s0
            } else {
              currentRun.reporting.deprecationWarning(errPoint, msg("deprecated"), "2.11.0")
              try StringContext.treatEscapes(s0) catch escapeHatch
            }
          }
          if (e.index == s0.length - 1) {
            c.error(errPoint, """Trailing '\' escapes nothing.""")
            s0
          } else if (octalOf(s0(e.index + 1)) >= 0) {
            badOctal
          } else {
            c.error(errPoint, e.getMessage)
            s0
          }
      }
      val s  = try StringContext.processEscapes(s0) catch escapeHatch
      val ms = fpat findAllMatchIn s

      def errorLeading(op: Conversion) = op.errorAt(Spec, s"conversions must follow a splice; ${Conversion.literalHelp}")

      def first = n == 0
      // a conversion for the arg is required
      if (!first) {
        val arg = argStack.pop()
        def s_%() = {
          fstring append "%s"
          defval(arg, AnyTpe)
        }
        def accept(op: Conversion) = {
          if (!op.isLeading) errorLeading(op)
          op.accepts(arg) match {
            case Some(tpe) => defval(arg, tpe)
            case None      =>
          }
        }
        if (ms.hasNext) {
          Conversion(ms.next, part.pos, args.size) match {
            case Some(op) if op.isLiteral => s_%()
            case Some(op) if op.indexed =>
              if (op.index map (_ == n) getOrElse true) accept(op)
              else {
                // either some other arg num, or '<'
                c.warning(op.groupPos(Index), "Index is not this arg")
                s_%()
              }
            case Some(op) => accept(op)
            case None     =>
          }
        } else s_%()
      }
      // any remaining conversions must be either literals or indexed
      while (ms.hasNext) {
        Conversion(ms.next, part.pos, args.size) match {
          case Some(op) if first && op.hasFlag('<')   => op.badFlag('<', "No last arg")
          case Some(op) if op.isLiteral || op.indexed => // OK
          case Some(op) => errorLeading(op)
          case None     =>
        }
      }
      fstring append s
    }

    parts.zipWithIndex foreach {
      case (part, n) => copyPart(part, n)
    }

    //q"{..$evals; new StringOps(${fstring.toString}).format(..$ids)}"
    val format = fstring.toString
    if (ids.isEmpty && !format.contains("%")) Literal(Constant(format))
    else {
      val scalaPackage = Select(Ident(nme.ROOTPKG), TermName("scala"))
      val newStringOps = Select(
        New(Select(Select(Select(scalaPackage,
          TermName("collection")), TermName("immutable")), TypeName("StringOps"))),
        termNames.CONSTRUCTOR
      )
      val expr =
        Apply(
          Select(
            Apply(
              newStringOps,
              List(Literal(Constant(format)))),
            TermName("format")),
          ids.toList
        )
      val p = c.macroApplication.pos
      Block(evals.toList, atPos(p.focus)(expr)) setPos p.makeTransparent
    }
  }

  val fpat = """%(?:(\d+)\$)?([-#+ 0,(\<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r
  object SpecifierGroups extends Enumeration { val Spec, Index, Flags, Width, Precision, CC = Value }

  val stdContextTags = new { val tc: c.type = c } with StdContextTags
  import stdContextTags._
  val tagOfFormattable = typeTag[Formattable]

  /** A conversion specifier matched by `m` in the string part at `pos`,
   *  with `argc` arguments to interpolate.
   */
  sealed trait Conversion {
    def m: Match
    def pos: Position
    def argc: Int

    import SpecifierGroups.{ Value => SpecGroup, _ }
    private def maybeStr(g: SpecGroup) = Option(m group g.id)
    private def maybeInt(g: SpecGroup) = maybeStr(g) map (_.toInt)
    val index: Option[Int]     = maybeInt(Index)
    val flags: Option[String]  = maybeStr(Flags)
    val width: Option[Int]     = maybeInt(Width)
    val precision: Option[Int] = maybeStr(Precision) map (_.drop(1).toInt)
    val op: String             = maybeStr(CC) getOrElse ""

    def cc: Char = if ("tT" contains op(0)) op(1) else op(0)

    def indexed:   Boolean = index.nonEmpty || hasFlag('<')
    def isLiteral: Boolean = false
    def isLeading: Boolean = m.start(0) == 0
    def verify:    Boolean = goodFlags && goodIndex
    def accepts(arg: Tree): Option[Type]

    val allFlags = "-#+ 0,(<"
    def hasFlag(f: Char) = (flags getOrElse "") contains f
    def hasAnyFlag(fs: String) = fs exists (hasFlag)

    def badFlag(f: Char, msg: String) = {
      val i = flags map (_.indexOf(f)) filter (_ >= 0) getOrElse 0
      errorAtOffset(Flags, i, msg)
    }
    def groupPos(g: SpecGroup) = groupPosAt(g, 0)
    def groupPosAt(g: SpecGroup, i: Int) = pos withPoint (pos.point + m.start(g.id) + i)
    def errorAt(g: SpecGroup, msg: String) = c.error(groupPos(g), msg)
    def errorAtOffset(g: SpecGroup, i: Int, msg: String) = c.error(groupPosAt(g, i), msg)

    def noFlags = flags.isEmpty || falsely { errorAt(Flags, "flags not allowed") }
    def noWidth = width.isEmpty || falsely { errorAt(Width, "width not allowed") }
    def noPrecision = precision.isEmpty || falsely { errorAt(Precision, "precision not allowed") }
    def only_-(msg: String) = {
      val badFlags = (flags getOrElse "") filterNot { case '-' | '<' => true case _ => false }
      badFlags.isEmpty || falsely { badFlag(badFlags(0), s"Only '-' allowed for $msg") }
    }
    protected def okFlags: String = allFlags
    def goodFlags = {
      val badFlags = flags map (_ filterNot (okFlags contains _))
      for (bf <- badFlags; f <- bf) badFlag(f, s"Illegal flag '$f'")
      badFlags.getOrElse("").isEmpty
    }
    def goodIndex = {
      if (index.nonEmpty && hasFlag('<'))
        c.warning(groupPos(Index), "Argument index ignored if '<' flag is present")
      val okRange = index map (i => i > 0 && i <= argc) getOrElse true
      okRange || hasFlag('<') || falsely { errorAt(Index, "Argument index out of range") }
    }
    /** Pick the type of an arg to format from among the variants
     *  supported by a conversion.  This is the type of the temporary,
     *  so failure results in an erroneous assignment to the first variant.
     *  A more complete message would be nice.
     */
    def pickAcceptable(arg: Tree, variants: Type*): Option[Type] =
      variants find (arg.tpe <:< _) orElse (
        variants find (c.inferImplicitView(arg, arg.tpe, _) != EmptyTree)
      ) orElse Some(variants(0))
  }
  object Conversion {
    import SpecifierGroups.{ Spec, CC }
    def apply(m: Match, p: Position, n: Int): Option[Conversion] = {
      def badCC(msg: String) = {
        val dk = new ErrorXn(m, p)
        val at = if (dk.op.isEmpty) Spec else CC
        dk.errorAt(at, msg)
      }
      def cv(cc: Char) = cc match {
        case 'b' | 'B' | 'h' | 'H' | 's' | 'S' =>
          new GeneralXn(m, p, n)
        case 'c' | 'C' =>
          new CharacterXn(m, p, n)
        case 'd' | 'o' | 'x' | 'X' =>
          new IntegralXn(m, p, n)
        case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' =>
          new FloatingPointXn(m, p, n)
        case 't' | 'T' =>
          new DateTimeXn(m, p, n)
        case '%' | 'n' =>
          new LiteralXn(m, p, n)
        case _ =>
          badCC(s"illegal conversion character '$cc'")
          null
      }
      Option(m group CC.id) map (cc => cv(cc(0))) match {
        case Some(x) => Option(x) filter (_.verify)
        case None    =>
          badCC(s"Missing conversion operator in '${m.matched}'; $literalHelp")
          None
      }
    }
    val literalHelp = "use %% for literal %, %n for newline"
  }
  class GeneralXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    def accepts(arg: Tree) = cc match {
      case 's' | 'S' if hasFlag('#') => pickAcceptable(arg, tagOfFormattable.tpe)
      case 'b' | 'B' => if (arg.tpe <:< NullTpe) Some(NullTpe) else Some(BooleanTpe)
      case _         => Some(AnyTpe)
    }
    override protected def okFlags = cc match {
      case 's' | 'S' => "-#<"
      case _         => "-<"
    }
  }
  class LiteralXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    import SpecifierGroups.Width
    override val isLiteral = true
    override def verify = op match {
      case "%" => super.verify && noPrecision && truly(width foreach (_ => c.warning(groupPos(Width), "width ignored on literal")))
      case "n" => noFlags && noWidth && noPrecision
    }
    override protected val okFlags = "-"
    def accepts(arg: Tree) = None
  }
  class CharacterXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = super.verify && noPrecision && only_-("c conversion")
    def accepts(arg: Tree) = pickAcceptable(arg, CharTpe, ByteTpe, ShortTpe, IntTpe)
  }
  class IntegralXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = {
      def d_# = (cc == 'd' && hasFlag('#') &&
        truly { badFlag('#', "# not allowed for d conversion") }
      )
      def x_comma = (cc != 'd' && hasFlag(',') &&
        truly { badFlag(',', "',' only allowed for d conversion of integral types") }
      )
      super.verify && noPrecision && !d_# && !x_comma
    }
    override def accepts(arg: Tree) = {
      def isBigInt = arg.tpe <:< tagOfBigInt.tpe
      val maybeOK = "+ ("
      def bad_+ = cond(cc) {
        case 'o' | 'x' | 'X' if hasAnyFlag(maybeOK) && !isBigInt =>
          maybeOK filter hasFlag foreach (badf =>
            badFlag(badf, s"only use '$badf' for BigInt conversions to o, x, X"))
          true
      }
      if (bad_+) None else pickAcceptable(arg, IntTpe, LongTpe, ByteTpe, ShortTpe, tagOfBigInt.tpe)
    }
  }
  class FloatingPointXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = super.verify && (cc match {
      case 'a' | 'A' =>
        val badFlags = ",(" filter hasFlag
        noPrecision && badFlags.isEmpty || falsely {
          badFlags foreach (badf => badFlag(badf, s"'$badf' not allowed for a, A"))
        }
      case _ => true
    })
    def accepts(arg: Tree) = pickAcceptable(arg, DoubleTpe, FloatTpe, tagOfBigDecimal.tpe)
  }
  class DateTimeXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    import SpecifierGroups.CC
    def hasCC = (op.length == 2 ||
      falsely { errorAt(CC, "Date/time conversion must have two characters") })
    def goodCC = ("HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc" contains cc) ||
      falsely { errorAtOffset(CC, 1, s"'$cc' doesn't seem to be a date or time conversion") }
    override def verify = super.verify && hasCC && goodCC && noPrecision && only_-("date/time conversions")
    def accepts(arg: Tree) = pickAcceptable(arg, LongTpe, tagOfCalendar.tpe, tagOfDate.tpe)
  }
  class ErrorXn(val m: Match, val pos: Position) extends Conversion {
    val argc = 0
    override def verify = false
    def accepts(arg: Tree) = None
  }
}
