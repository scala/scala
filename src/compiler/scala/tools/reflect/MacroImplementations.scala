package scala.tools.reflect

import scala.reflect.macros.contexts.Context
import scala.collection.mutable.{ ListBuffer, Stack }
import scala.reflect.internal.util.Position
import scala.PartialFunction.cond
import scala.util.matching.Regex.Match

import java.util.{ Formatter, Formattable, IllegalFormatException }

abstract class MacroImplementations {
  val c: Context

  import c.universe.{ Match => _, _ }
  import definitions._

  @inline private def truly(body: => Unit): Boolean = { body ; true }
  @inline private def falsely(body: => Unit): Boolean = { body ; false }

  private def fail(msg: String) = c.abort(c.enclosingPosition, msg)

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
   */
  def macro_StringInterpolation_f(parts: List[Tree], args: List[Tree], origApplyPos: c.universe.Position): Tree = {

    val fpat = """%(?:(\d+)\$)?([-#+ 0,(\<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r
    object SpecifierGroups extends Enumeration { val Spec, Index, Flags, Width, Precision, CC = Value }

    // None if subtypes none
    def checkType0(arg: Tree, variants: Type*): Option[Type] =
      variants find (arg.tpe <:< _)
    // None if conforming to none
    def checkType1(arg: Tree, variants: Type*): Option[Type] =
      checkType0(arg, variants: _*) orElse (
        variants find (c.inferImplicitView(arg, arg.tpe, _) != EmptyTree)
      )
    // require variants.nonEmpty
    def checkType(arg: Tree, variants: Type*): Option[Type] =
      checkType1(arg, variants: _*) orElse Some(variants(0))

    val stdContextTags = new { val tc: c.type = c } with StdContextTags
    import stdContextTags._
    val tagOfFormattable = typeTag[Formattable]

    abstract class Conversion(m: Match, pos: Position) {
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
        val okRange = index map (i => i > 0 && i <= args.size) getOrElse true
        okRange || hasFlag('<') ||
          falsely { errorAt(Index, "Argument index out of range") }
      }
    }
    object Conversion {
      import SpecifierGroups.{ Spec, CC, Width }
      def apply(m: Match, p: Position): Option[Conversion] = {
        def badCC(msg: String) = {
          val dk = new Conversion(m, p) {
            override def verify = false
            def accepts(arg: Tree) = None
          }
          val at = if (dk.op.isEmpty) Spec else CC
          dk.errorAt(at, msg)
        }
        def cv(cc: Char) = cc match {
          case 'b' | 'B' | 'h' | 'H' | 's' | 'S' =>
            new Conversion(m, p) with GeneralXn
          case 'c' | 'C' =>
            new Conversion(m, p) with CharacterXn
          case 'd' | 'o' | 'x' | 'X' =>
            new Conversion(m, p) with IntegralXn
          case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' =>
            new Conversion(m, p) with FloatingPointXn
          case 't' | 'T' =>
            new Conversion(m, p) with DateTimeXn
          case '%' | 'n' =>
            new Conversion(m, p) with LiteralXn
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
      trait GeneralXn extends Conversion {
        def accepts(arg: Tree) = cc match {
          case 's' | 'S' if hasFlag('#') => checkType(arg, tagOfFormattable.tpe)
          case 'b' | 'B' => if (arg.tpe <:< NullTpe) Some(NullTpe) else Some(BooleanTpe)
          case _         => Some(AnyTpe)
        }
        override protected def okFlags = cc match {
          case 's' | 'S' => "-#<"
          case _         => "-<"
        }
      }
      trait LiteralXn extends Conversion {
        override val isLiteral = true
        override def verify = op match {
          case "%" => super.verify && noPrecision && truly(width foreach (_ => c.warning(groupPos(Width), "width ignored on literal")))
          case "n" => noFlags && noWidth && noPrecision
        }
        override protected val okFlags = "-"
        def accepts(arg: Tree) = None
      }
      trait CharacterXn extends Conversion {
        override def verify = super.verify && noPrecision && only_-("c conversion")
        def accepts(arg: Tree) = checkType(arg, CharTpe, ByteTpe, ShortTpe, IntTpe)
      }
      trait IntegralXn extends Conversion {
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
          def isBigInt = checkType0(arg, tagOfBigInt.tpe).nonEmpty
          val maybeOK = "+ ("
          def bad_+ = cond(cc) {
            case 'o' | 'x' | 'X' if hasAnyFlag(maybeOK) && !isBigInt =>
              maybeOK filter hasFlag foreach (badf =>
                badFlag(badf, s"only use '$badf' for BigInt conversions to o, x, X"))
              true
          }
          if (bad_+) None else checkType(arg, IntTpe, LongTpe, ByteTpe, ShortTpe, tagOfBigInt.tpe)
        }
      }
      trait FloatingPointXn extends Conversion {
        override def verify = super.verify && (cc match {
          case 'a' | 'A' =>
            val badFlags = ",(" filter hasFlag
            noPrecision && badFlags.isEmpty || falsely {
              badFlags foreach (badf => badFlag(badf, s"'$badf' not allowed for a, A"))
            }
          case _ => true
        })
        def accepts(arg: Tree) = checkType(arg, DoubleTpe, FloatTpe, tagOfBigDecimal.tpe)
      }
      trait DateTimeXn extends Conversion {
        def hasCC = (op.length == 2 ||
          falsely { errorAt(CC, "Date/time conversion must have two characters") })
        def goodCC = ("HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc" contains cc) ||
          falsely { errorAtOffset(CC, 1, s"'$cc' doesn't seem to be a date or time conversion") }
        override def verify = super.verify && hasCC && goodCC && noPrecision && only_-("date/time conversions")
        def accepts(arg: Tree) = checkType(arg, LongTpe, tagOfCalendar.tpe, tagOfDate.tpe)
      }
      val literalHelp = "use %% for literal %, %n for newline"
    }
    def badlyInvoked = (parts.length != args.length + 1) && truly {
      def because(s: String) = s"too $s arguments for interpolated string"
      val (p, msg) =
        if (parts.length == 0) (c.prefix.tree.pos, "there are no parts")
        else if (args.length + 1 < parts.length)
          (if (args.isEmpty) c.enclosingPosition else args.last.pos, because("few"))
        else (args(parts.length-1).pos, because("many"))
      c.abort(p, msg)
    }
    def interpolated = {
      val bldr     = new StringBuilder
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
        val s  = StringContext.treatEscapes(s0)
        val ms = fpat findAllMatchIn s
        def first = n == 0
        // a conversion for the arg is required
        if (!first) {
          val arg = argStack.pop()
          def s_%(): Unit = {
            bldr append "%s"
            defval(arg, AnyTpe)
          }
          def accept(op: Conversion) = op.accepts(arg) match {
            case Some(tpe) => defval(arg, tpe)
            case None =>
          }
          if (ms.hasNext) {
            val m = ms.next
            Conversion(m, part.pos) match {
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
          val m = ms.next
          Conversion(m, part.pos) match {
            case Some(op) if first && op.hasFlag('<')   => op.badFlag('<', "No last arg")
            case Some(op) if op.isLiteral || op.indexed => // OK
            case Some(op) => op.errorAt(Spec, s"conversions must follow a splice; ${Conversion.literalHelp}")
            case None     =>
          }
        }
        bldr append s
      }

      parts.zipWithIndex foreach {
        case (part, n) => copyPart(part, n)
      }

      val fstring = bldr.toString
      val expr =
        Apply(
          Select(
            Literal(Constant(fstring)),
            TermName("format")),
          List(ids: _* )
        )
      Block(evals.toList, atPos(origApplyPos.focus)(expr)) setPos origApplyPos.makeTransparent
      //q"..evals, ${Literal(Constant(fstring))}.format(..$ids)"
    }
    if (badlyInvoked) EmptyTree else interpolated
  }
}
