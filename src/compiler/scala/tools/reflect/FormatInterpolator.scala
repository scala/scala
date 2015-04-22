package scala.tools.reflect

import scala.reflect.macros.runtime.Context
import scala.collection.mutable.{ ListBuffer, Stack }
import scala.reflect.internal.util.Position
import scala.PartialFunction.{ cond => when }
import scala.math.ScalaNumber
import scala.util.matching.Regex.Match
import scala.util.{ Either, Left, Right }
import scala.util.Properties.{ lineSeparator => EOL }

import java.util.{ Formatter, Formattable, IllegalFormatException }

abstract class FormatInterpolator {
  val c: Context
  val global: c.universe.type = c.universe

  import c.universe.{ Match => _, Try => _, _ }
  import definitions._
  import treeInfo.Applied

  val stdContextTags = new { val tc: c.type = c } with StdContextTags
  import stdContextTags._
  val tagOfScalaNumber = typeTag[ScalaNumber]
  val tagOfJBigInt     = typeTag[java.math.BigInteger]
  val tagOfJBigDecimal = typeTag[java.math.BigDecimal]
  val tagOfFormattable = typeTag[java.util.Formattable]
  //val tagOfTemporalAccessor = typeTag[java.time.TemporalAccessor]

  @inline private def truly  (body: Unit): Boolean = true
  @inline private def falsely(body: Unit): Boolean = false

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
      if (badlyInvoked) EmptyTree else interpolated(parts, args)
    case other =>
      bail(s"Unexpected application ${showRaw(other)}")
      other
  }

  // extra help on escape errors
  private def escapeHatch(s0: String, pos: Position): PartialFunction[Throwable, String] = {
    // trailing backslash, octal escape, or other
    case e: StringContext.InvalidEscapeException =>
      def errPoint = pos withPoint (pos.point + e.index)
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
          currentRun.reporting.deprecationWarning(errPoint, msg("deprecated"))
          try StringContext.treatEscapes(s0) catch escapeHatch(s0, pos)
        }
      }
      def control(ctl: Char, i: Int, name: String) = {
        c.error(errPoint, s"\\$ctl is not supported, but for $name use \\u${"%04x" format i};${EOL}${e.getMessage}")   
        s0
      }
      if (e.index == s0.length - 1) {
        c.error(errPoint, """Trailing '\' escapes nothing.""")
        s0
      } else s0(e.index + 1) match {
        case i if octalOf(i) >= 0 => badOctal
        case 'a' => control('a', 0x7, "alert or BEL")
        case 'v' => control('v', 0xB, "vertical tab")
        case 'e' => control('e', 0x1B, "escape")
        case _   => c.error(errPoint, e.getMessage) ; s0
      }
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

    // the idiom to produce a tree suitable for c.eval
    def untyped(t: Tree) = c.untypecheck(t.duplicate)

    def coerce(t: Tree, s: TypeSymbol): Tree = Typed(t, global.gen.mkUnattributedRef(TypeName(s.fullName)))

    // a typed arg and the type required for conversion
    case class ArgExpr(arg: Tree, tpe: Type) {
      def boxed(restpe: Type = tpe, rhstpe: Type = NoType, rhs: Tree = untyped(arg)) =
        BoxedArgExpr(arg, restpe, rhstpe, rhs)
    }
    // the arg is assigned a tmp of the form val $x1: restpe = (rhs: rhstpe)
    // the rhs is an untyped tree. rhstpe coercion is omitted for NoType.
    case class BoxedArgExpr(arg: Tree, restpe: Type, rhstpe: Type, rhs: Tree) {
      def indexed(index: Int)    = IndexedArgExpr(this, Left(index))
      def literal(value: AnyRef) = IndexedArgExpr(this, Right(value))
    }
    // after inlining constants, each arg has either a %3$ index or a constant value
    case class IndexedArgExpr(expr: BoxedArgExpr, ref: Either[Int, AnyRef]) {
      def isLiteral = ref.isRight
      def literal   = ref.right.get
      def index     = ref.left.get
    }

    // if true, sanity check the index later
    var sawIndexedSpecifier = false

    // build the format string and determine required types for args
    def buildFormat(): (String, List[ArgExpr]) = {
      val fstring = new StringBuilder
      val evals   = ListBuffer[ArgExpr]()

      // Append the nth part to the string builder, possibly prepending an omitted %s first.
      // Sanity-check the % fields in this part.
      def copyPart(part: Tree, n: Int): Unit = {
        import SpecifierGroups.{ Spec, Index }
        def first = n == 0

        val s  = part match {
          case Literal(Constant(x: String)) => try StringContext.processEscapes(x) catch escapeHatch(x, part.pos)
          case _ => throw new IllegalArgumentException("internal error: argument parts must be a list of string literals")
        }

        // find all conversions in the part
        val ms = fpat findAllMatchIn s

        def errorLeading(op: Conversion) = op.errorAt(Spec, s"conversions must follow a splice; ${Conversion.literalHelp}")

        // a conversion for the arg is required at the beginning of the next part
        def checkLeading(): Unit = {
          val arg = args(n - 1)
          // an arg with the type that it must bear when handed to format
          def defval(value: Tree, tpe: Type): Unit = {
            val t = if (AnyRefTpe <:< tpe && !(arg.tpe =:= AnyTpe)) arg.tpe else tpe  // keep narrower type than anyref
            evals += ArgExpr(value, t)
          }
          def s_%() = {
            fstring append "%s"
            defval(arg, AnyRefTpe)
          }
          def onIndex(op: Conversion) = {
            if (op.isLeading) c.warning(op.groupPos(Index), "Index is not this arg")
            sawIndexedSpecifier = true
            s_%()
          }
          def accept(op: Conversion) = {
            if (!op.isLeading) errorLeading(op)
            op.accepts(arg) match {
              case Some(tpe) => defval(arg, tpe)
              case None      => // you've come to the right place, but there was an error
            }
          }
          if (ms.hasNext) {
            Conversion(ms.next, part.pos, args.size) match {
              case Some(op) if op.isLiteral => s_%()
              case Some(op) if op.indexed   => onIndex(op)
              case Some(op)                 => accept(op)
              case None                     => // e.g., bad or missing conversion character
            }
          } else s_%()
        }
        // any remaining conversions in the part must be either literals or indexed
        def checkRemaining(): Unit = {
          var hasIndex = false
          def onIndex(op: Conversion) = {
            if (!op.hasFlag('<')) hasIndex = true
            else if (first && !hasIndex) op.badFlag('<', "No previous arg")
            sawIndexedSpecifier = true
          }
          while (ms.hasNext) {
            Conversion(ms.next, part.pos, args.size) match {
              case Some(op) if op.isLiteral => // OK
              case Some(op) if op.indexed   => onIndex(op)
              case Some(op)                 => errorLeading(op)
              case None                     => // e.g., bad or missing conversion character
            }
          }
        }

        if (!first) checkLeading()
        checkRemaining()

        fstring append s
      }
      parts.zipWithIndex foreach {
        case (part, n) => copyPart(part, n)
      }
      (fstring.toString, evals.toList)
    }

    // unwrap ScalaNumber or box primitives
    def unwrapOrBox(argExpr: ArgExpr): BoxedArgExpr = argExpr match {
      case ArgExpr(arg, reqtpe) =>
        def boxUp(typSym: TypeSymbol) = typSym match {
          case BooleanClass => typeOf[java.lang.Boolean]
          case ByteClass    => typeOf[java.lang.Byte]
          case CharClass    => typeOf[java.lang.Character]
          case ShortClass   => typeOf[java.lang.Short]
          case IntClass     => typeOf[java.lang.Integer]
          case LongClass    => typeOf[java.lang.Long]
          case FloatClass   => typeOf[java.lang.Float]
          case DoubleClass  => typeOf[java.lang.Double]
          case _            => AnyRefTpe
        }
        def primSym(s: Symbol) = s.isClass && s.asClass.isPrimitive
        def botSym(s: Symbol)  = s.isClass && s.asClass.isBottomClass

        // ScalaNumber (BigDecimal, BigInt) must be unwrapped to n.underlying (see StringOps.format).
        // Only unwrap if the tmp assignment typechecks; otherwise, let it error.
        if (!botSym(arg.tpe.typeSymbol) && arg.tpe <:< tagOfScalaNumber.tpe && arg.tpe <:< reqtpe) {
          val underlying = TermName("underlying")
          val tpt = arg.tpe.member(underlying).typeSignature.resultType
          val rhs = Select(untyped(arg), underlying)
          argExpr.boxed(restpe = tpt, rhs = rhs)
        }
        // primitives must be boxed, so val x$1: Int = v becomes val x$1: java.lang.Integer = (v: Int)
        else if (primSym(reqtpe.typeSymbol.asType))
          argExpr.boxed(restpe = boxUp(reqtpe.typeSymbol.asType), rhstpe = reqtpe)
        // or val x$1: AnyRef = v where (v: Int) becomes val x$1: java.lang.Integer = v
        else if (primSym(arg.tpe.typeSymbol.asType))
          argExpr.boxed(restpe = boxUp(arg.tpe.typeSymbol.asType))
        else
          argExpr.boxed()
    }

    // For each arg, either its index (after inlining literals) or a literal value to inline.
    def indexed(args: List[BoxedArgExpr]): List[IndexedArgExpr] = {
      var ai = ListBuffer[IndexedArgExpr]()
      var index = 0
      def nextLiteral(b: BoxedArgExpr, x: AnyRef) = ai += b.literal(x)
      def nextArg(b: BoxedArgExpr) = {
        index += 1
        ai += b.indexed(index)
      }
      // if the rhs is a constant expression, extract the value
      args foreach { case boxed @ BoxedArgExpr(arg, restpe, rhstpe, rhs) =>
        val target = rhstpe orElse restpe
        rhs match {
          case Literal(Constant(s: Symbol)) => nextLiteral(boxed, s.name)
          case Literal(Constant(t: Type))   => nextLiteral(boxed, t.typeSymbol.fullName)
          case Literal(k @ Constant(_)) =>
            if (arg.tpe weak_<:< target) {
              k convertTo target match {
                case Constant(x) => nextLiteral(boxed, x.asInstanceOf[AnyRef])
                case _           => nextArg(boxed)
              }
            } else nextArg(boxed)
          case _ =>
            if (arg.tpe weak_<:< target) {
              // see if it's a foldable constant op
              global.constfold(rhs) match {
                case Literal(k @ Constant(_)) =>
                  k convertTo target match {
                    case Constant(x) => nextLiteral(boxed, x.asInstanceOf[AnyRef])
                    case _           => nextArg(boxed)
                  }
                case _ => nextArg(boxed)
              }
            } else nextArg(boxed)
        }
      }
      ai.toList
    }

    // eliminate constants from the args, updating arg indexes.
    // inline constants in the parts, including index refs.
    // sanity check conversion types of index refs: e.g., f"$v%f %1\$d" should not compile.
    def inlined(format: String, indexes: List[IndexedArgExpr]): String = {
      var argi  = 0                                                      // the current arg index
      var lasti = 0                                                      // the previous index, used by "%<s"
      def argAt(i: Int)     = indexes(i - 1)
      def isLiteral(i: Int) = argAt(i).isLiteral
      def literal(i: Int)   = argAt(i).literal
      def indexAt(i: Int)   = argAt(i).index
      def tmptpes(i: Int)   = argAt(i).expr.rhstpe orElse argAt(i).expr.restpe

      def check(m: Match, cc: Conversion): Option[String] = {
        def specifier: String  = cc.m.matched
        // emit modified index, stripping relative index < if present
        def atNewIndex(i: Int) = {
          def orEmpty(s: String) = if (s == null) "" else s
          val fpat(index, flags, width, precision, cc) = m
          val nindex = if (i > 0) s"$i$$" else ""
          val nflags = orEmpty(flags).replace("<", "")
          s"%${nindex}${nflags}${orEmpty(width)}${orEmpty(precision)}${cc}"
        }
        def unindexed(i: Int)  = atNewIndex(0)                                            // strip index
        def replaceAt(f: String, i: Int) = Some(String.format(f, literal(i)))             // this format should always succeed
        def noncompat()        = { c.error(cc.pos, s"Specifier '$specifier' not compatible with indexed arg") ; None }

        if (cc.isLiteral) None                                           // can't do anything with %%, %n
        else if (cc.indexed) {
          val index = cc.index getOrElse lasti                           // explicit index %3$s or relative %<s
          lasti = index
          val ok = cc.isCompatible(tmptpes(index))                       // check that specifier is compatible with indexed arg
          if (!ok) noncompat()
          else if (isLiteral(index)) replaceAt(unindexed(index), index)  // format using %tm instead of %<tm or %1$tm
          else cc.index filter (i => indexAt(i) != i) map atNewIndex     // translate %3$tm if necessary
        }
        else {
          argi += 1                                                      // advance to next vararg
          lasti = argi
          if (isLiteral(argi)) replaceAt(specifier, argi) else None
        }
      }
      // parsing specifiers must succeed since all specs have been parsed (but indexed specs not verified yet)
      val replacer = (m: Match) => Conversion(m, c.enclosingPosition, args.size) flatMap (check(m, _))

      fpat replaceSomeIn (format, replacer)
    }

    //q"{..$evals; java.lang.String.format($fstring, ..$ids)}"
    def emit(format: String, tmps: List[Tree], vargs: List[Tree]): Tree = vargs match {
      case Nil if (format indexOf '%') < 0 => Literal(Constant(format))
      case _ =>
        val javaPackage = Select(Ident(nme.ROOTPKG), TermName("java"))
        val formatter   = Select(Select(Select(javaPackage,
            TermName("lang")), TermName("String")), TermName("format")
        )
        val expr =
          Apply(
            formatter,
            Literal(Constant(format)) :: vargs
          )
        val p = c.macroApplication.pos
        //if (settings.debug) Console println s"Format '$format' with ${tmps.size} tmps $tmps and ${vargs.size} args $vargs"
        Block(tmps, atPos(p.focus)(expr)) setPos p.makeTransparent
    }

    // build a java format string with args to supply in evals
    val (format, argExprs) = buildFormat()

    // can I get this boxed, please?
    val boxedArgs = argExprs map unwrapOrBox

    // identify literals and adjust arg indexes
    val indexedArgs = indexed(boxedArgs)

    //if (settings.debug) Console println s"argExprs: $argExprs ; boxed: $boxedArgs"

    def mkTmp(b: BoxedArgExpr): ValDef = {
      val n = TermName(c.freshName("arg$"))
      val x =
        if (b.rhstpe != NoType) coerce(b.rhs, b.rhstpe.typeSymbol.asType)
        else if (b.arg.tpe =:= AnyTpe && b.restpe =:= AnyRefTpe)
          TypeApply(Select(b.rhs, TermName("asInstanceOf")), List(Ident(TypeName("AnyRef"))))
        else b.rhs
      ValDef(Modifiers(), n, TypeTree(b.restpe), x)
    }
    def emitOptimized(): Tree = {
      val reducedExprs = indexedArgs filterNot (_.isLiteral)
      val reducedTmps  = reducedExprs map (i => mkTmp(i.expr))
      val reducedIDs   = reducedTmps map (v => Ident(v.name))
      emit(inlined(format, indexedArgs), tmps = reducedTmps, vargs = reducedIDs)
    }
    def emitAll(): Tree = {
      val tmps = indexedArgs map (i => mkTmp(i.expr))
      val ids  = tmps map (v => Ident(v.name))
      emit(format, tmps = tmps, vargs = ids)
    }

    val optimizing = !c.hasErrors && (sawIndexedSpecifier || indexedArgs.exists(_.isLiteral))
    if (optimizing) emitOptimized() else emitAll()
  }

  val fpat = """%(?:(\d+)\$)?([-#+ 0,(\<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r   // specifier
  object SpecifierGroups extends Enumeration { val Spec, Index, Flags, Width, Precision, CC = Value }

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

    // for a given arg, pick a target type for the specifier from among oktypes
    def oktypes: List[Type]
    def accepts(arg: Tree): Option[Type] = acceptsBottom(arg) orElse pickAcceptable(arg, oktypes: _*)
    def isCompatible(tpe: Type): Boolean = oktypes.exists(tpe <:< _) || tpe.typeSymbol.isBottomClass
    protected def acceptsBottom(arg: Tree): Option[Type] =
      if (arg.tpe.typeSymbol.isBottomClass) Some(arg.tpe) else None

    val allFlags = "-#+ 0,(<"
    def hasFlag(f: Char) = (flags getOrElse "") contains f
    def hasFlags(f1: Char, f2: Char) = hasFlag(f1) && hasFlag(f2)
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
      for (ff <- flags; dupe <- (ff diff ff.distinct).distinct; i = ff lastIndexOf dupe)
        errorAtOffset(Flags, i, s"Duplicate flag '$dupe'")
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
    /** Pick the type of the tmp val to format from among the variants
     *  supported by a conversion, defaulting to the first variant.
     *  On an erroneous assignment, the first variant determines the error message.
     */
    final def pickAcceptable(arg: Tree, variants: Type*): Option[Type] =
      variants find (arg.tpe <:< _) orElse (
        variants find (c.inferImplicitView(arg, arg.tpe, _) != EmptyTree)
      ) orElse variants.headOption

    override def toString = s"Conversion $getClass(${m.matched})"
  }
  object Conversion {
    import SpecifierGroups.{ Spec, CC, Width }
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
    import SpecifierGroups.CC
    val oktypes = Nil
    def isFormattable(tpe: Type) = !tpe.typeSymbol.isBottomClass && tpe <:< tagOfFormattable.tpe
    override def verify = {
      def missingWidth = hasFlag('-') && width.isEmpty && truly {
        badFlag('-', "Field width is required with '-' left adjustment")
      }
      super.verify && !missingWidth
    }
    override def accepts(arg: Tree) = cc match {
      case 's' | 'S' if hasFlag('#') =>
        // "%#s".format(null) is ConversionMismatch
        if (arg.tpe.typeSymbol.isBottomClass) {
          errorAt(CC, "Requires an instance of java.util.Formattable")
          None
        } else
          Some(tagOfFormattable.tpe)
      case 'b' | 'B' => super.accepts(arg) orElse Some(BooleanTpe)
      case _         => super.accepts(arg) orElse Some(AnyRefTpe)
    }
    override def isCompatible(tpe: Type) = cc match {
      case 's' | 'S' if hasFlag('#') => isFormattable(tpe)
      case 'b' | 'B' => tpe <:< BooleanTpe || super.isCompatible(tpe)
      case _         => true
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
      case "%" => super.verify && noPrecision && truly {
        width foreach (_ => c.warning(groupPos(Width), "width ignored on literal"))
      }
      case "n" => noFlags && noWidth && noPrecision
    }
    override protected val okFlags = "-"
    val oktypes = Nil
    override def accepts(arg: Tree) = None
    override def isCompatible(tpe: Type) = false
  }
  class CharacterXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = super.verify && noPrecision && only_-("c conversion")
    val oktypes = List(CharTpe, ByteTpe, ShortTpe, IntTpe)
  }
  class IntegralXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = {
      def d_# = cc == 'd' && hasFlag('#') && truly {
        badFlag('#', "# not allowed for d conversion")
      }
      def x_comma = cc != 'd' && hasFlag(',') && truly {
        badFlag(',', "',' only allowed for d conversion of integral types")
      }
      def leftAdjustWithPad = hasFlags('-', '0') &&
        truly(badFlag('0', "Can't combine '0' padding with '-' left adjustment"))
      def missingWidth = hasAnyFlag("-0") && width.isEmpty && truly {
        val f = if (hasFlag('-')) '-' else '0'
        badFlag(f, "Field width is required with '0' padding or '-' left adjustment")
      }
      super.verify && noPrecision && !d_# && !x_comma && !leftAdjustWithPad && !missingWidth
    }
    val oktypes = List(IntTpe, LongTpe, ByteTpe, ShortTpe, tagOfJBigInt.tpe, tagOfBigInt.tpe)
    override def accepts(arg: Tree) = {
      def isBigInt = arg.tpe <:< tagOfBigInt.tpe
      val maybeOK = "+ ("
      def bad_+ = when(cc) {
        case 'o' | 'x' | 'X' if hasAnyFlag(maybeOK) && !isBigInt => truly {
          maybeOK filter hasFlag foreach (badf => badFlag(badf, s"only use '$badf' for BigInt conversions to o, x, X"))
        }
      }
      if (bad_+) None else super.accepts(arg)
    }
  }
  class FloatingPointXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    override def verify = {
      def restrictedForA = when(cc) {
        case 'a' | 'A' =>
          val badFlags = ",(" filter hasFlag
          !noPrecision || (!badFlags.isEmpty && truly {
            badFlags foreach (badf => badFlag(badf, s"'$badf' not allowed for a, A"))
          })
      }
      def leftAdjustWithPad = hasFlags('-', '0') &&
        truly(badFlag('0', "Can't combine '0' padding with '-' left adjustment"))
      def missingWidth = hasAnyFlag("-0") && width.isEmpty && truly {
        val f = if (hasFlag('-')) '-' else '0'
        badFlag(f, "Field width is required with '0' padding or '-' left adjustment")
      }
      super.verify && !leftAdjustWithPad && !restrictedForA && !missingWidth
    }
    val oktypes = List(DoubleTpe, FloatTpe, tagOfJBigDecimal.tpe, tagOfBigDecimal.tpe)
  }
  class DateTimeXn(val m: Match, val pos: Position, val argc: Int) extends Conversion {
    import SpecifierGroups.CC
    def hasCC = op.length == 2 || falsely {
      errorAt(CC, "Date/time conversion must have two characters")
    }
    def goodCC = ("HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc" contains cc) || falsely {
      errorAtOffset(CC, 1, s"'$cc' doesn't seem to be a date or time conversion")
    }
    override def verify = super.verify && hasCC && goodCC && noPrecision && only_-("date/time conversions")
    val oktypes = List(LongTpe, tagOfCalendar.tpe, tagOfDate.tpe) // todo TemporalAccessor
  }
  class ErrorXn(val m: Match, val pos: Position) extends Conversion {
    val argc = 0
    override def verify = false
    val oktypes = Nil
    override def accepts(arg: Tree) = None
    override def isCompatible(tpe: Type) = false
  }
}
