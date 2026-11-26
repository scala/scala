/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.reflect

import java.util.Formattable

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.runtime.Context
import scala.tools.nsc.Reporting.WarningCategory, WarningCategory.WFlagTostringInterpolated

/** The `f` interpolator, which turns `f"$x%d items"` into `"%d items".format(x)`.
 *
 *  It works in phases, run by `Interpolation.translate`. Parsing, validating and resolving a format are
 *  pure and live in the companion:
 *   1. parse each part into text and specifiers,
 *   2. validate each specifier on its own,
 *   3. resolve: insert `%s` where an arg has no conversion, and select the arg of each specifier,
 *      as `java.util.Formatter` does.
 *  The compiler is needed only to
 *   4. type check each arg against its conversions, and
 *   5. generate the formatting, a concatenation if the format is plain `%s`, or a constant.
 *  Each phase reports its problems, and nothing is generated if there are errors.
 */
abstract class FormatInterpolator {
  import FormatInterpolator._

  val c: Context
  val global: c.universe.type = c.universe

  import c.universe._
  import definitions._
  import treeInfo.Applied

  protected var linting = settings.warnToString.value

  protected final def withoutLinting[A](body: => A): A = {
    val linted = linting
    linting = false
    try body finally linting = linted
  }

  def concatenate(parts: List[Tree], args: List[Tree]): Tree

  def interpolateF: Tree = c.macroApplication match {
    //case q"$_(..$parts).f(..$args)" =>
    case Applied(Select(Apply(_, parts), _), _, argss) =>
      val args = argss.flatten
      def because(s: String) = s"too $s arguments for interpolated string"
      if (parts.isEmpty) c.abort(c.prefix.tree.pos, "there are no parts")
      if (parts.lengthIs > args.length + 1) c.abort(if (args.isEmpty) c.enclosingPosition else args.last.pos, because("few"))
      if (parts.lengthIs < args.length + 1) c.abort(args(parts.length - 1).pos, because("many"))
      new Interpolation(parts, args).translate()
    case other =>
      global.abort(s"Unexpected application ${showRaw(other)}")
  }

  /** The translation of one interpolation, with the trees of its parts and args. */
  private class Interpolation(partTrees: List[Tree], args: List[Tree]) {
    def translate(): Tree = {
      val parts   = partTrees.map(parsePart)
      val invalid = validateSpecs(parts)
      val program = resolve(parts.map(_.pieces), args.length, !invalid(_))
      program.problems.foreach { case (n, p) => report(parts(n), p) }
      val actuals = typeCheck(parts, program, invalid)
      if (hasErrors) constant("") // a tree that does not need expanding again
      else generate(parts, program, actuals)
    }

    private var hasErrors = false

    private def error(pos: Position, msg: String): Unit = { c.error(pos, msg); hasErrors = true }
    private def warning(pos: Position, msg: String, cat: WarningCategory = WarningCategory.Other): Unit =
      c.callsiteTyper.context.warning(pos, msg, cat, Nil)

    private def report(part: Part, problem: Problem): Unit =
      if (problem.error) error(part.posAt(problem.offset), problem.msg)
      else warning(part.posAt(problem.offset), problem.msg)

    // 1. parse

    /** A part, with its text after escapes are processed, and the position of each offset in that text. */
    private final class Part(val tree: Tree, val text: String, val pieces: List[Piece], val posAt: Int => Position)

    private def parsePart(tree: Tree): Part = tree match {
      case Literal(Constant(raw: String)) =>
        var escaped = true
        val text =
          try StringContext.processEscapes(raw)
          catch escapeHatch(raw, sourcePosAt(tree.pos, raw, escapes = false)).andThen { s => escaped = false; s }
        new Part(tree, text, parse(text), sourcePosAt(tree.pos, text, escapes = escaped))
      case _ =>
        error(tree.pos, "parts of an f-interpolation must be string literals")
        new Part(tree, "", Nil, _ => tree.pos)
    }

    // 2. validate

    /** The invalid specifiers, after reporting their problems. */
    private def validateSpecs(parts: List[Part]): Set[Spec] = {
      val bad = Set.newBuilder[Spec]
      for (part <- parts; spec <- part.pieces.collect { case s: Spec => s }) {
        val (err, warnings) = validate(spec)
        warnings.foreach(report(part, _))
        err.foreach { e => report(part, e); bad += spec }
      }
      bad.result()
    }

    // 4. type check

    /** The actual of each arg, as its own conversion decides. A reference checks the actual it refers to,
     *  so references are checked after all own conversions.
     */
    private def typeCheck(parts: List[Part], program: Program, invalid: Set[Spec]): List[Tree] = {
      val actuals = new Array[Tree](args.length)
      for (use <- program.uses if use.own) {
        val argi = use.arg - 1
        val arg = args(argi)
        val spec = use.spec.getOrElse(PlainS)
        val valid = use.spec.forall(!invalid(_))
        actuals(argi) =
          if (!valid && spec.kind == Kind.Unknown) arg
          else typeOwn(arg, spec) match {
            case Some((actual, resolved)) =>
              if (valid) checkResolved(parts, use, spec, resolved, arg.tpe)
              actual
            case None => arg
          }
      }
      for (use <- program.uses if !use.own; spec <- use.spec if !invalid(spec))
        typeReference(parts, use, spec, actuals(use.arg - 1))
      actuals.toList
    }

    // the actual for `arg` converted to the first accepted type it conforms or converts to, and that type
    private def typeOwn(arg: Tree, spec: Spec): Option[(Tree, Type)] = {
      val acc = accepted(spec)
      val tpe = arg.tpe
      def converted(target: Type): Option[(Tree, Type)] =
        c.inferImplicitView(arg, tpe, target) match {
          case EmptyTree => None
          case _ =>
            // let the compiler figure out how to apply the conversion
            val freshName = TermName(c.freshName("arg$"))
            val ValDef(_, _, _, rhs) = c.typecheck(ValDef(Modifiers(), freshName, TypeTree(target).setPos(arg.pos.focus), arg).setPos(arg.pos)): @unchecked
            Some((rhs, target))
        }
      val found =
        if (isNullForFormattable(spec, tpe)) None
        else acc.exact.find(tpe <:< _).map(t => (arg, t))
          .orElse(acc.exact.iterator.map(converted).collectFirst { case Some(r) => r })
          .orElse(acc.others.find(tpe <:< _).map(t => (arg, t)))
      if (found.isEmpty) error(arg.pos, mismatchMessage(tpe, acc.all.filter(_ != AnyTpe)))
      found
    }

    // The actual is not converted again. A type that may hold a value that the formatter takes is
    // accepted: one of an accepted type, or a supertype of one, such as a Serializable for a LocalDate.
    // Checks that depend on the type are done only if it is an accepted one.
    private def typeReference(parts: List[Part], use: Use, spec: Spec, actual: Tree): Unit = {
      val acc = accepted(spec)
      val tpe = actual.tpe.dealiasWiden.upperBound
      def mismatch() = error(specPos(parts, use, _.conversionAt), mismatchMessage(actual.tpe, acc.all))
      if (isNullForFormattable(spec, tpe)) mismatch()
      else acc.all.find(v => acc.runtimeTypes(v).exists(tpe <:< _)) match {
        case Some(v) => checkResolved(parts, use, spec, v, actual.tpe)
        case None    => if (!acc.all.exists(v => acc.runtimeTypes(v).exists(_ <:< tpe))) mismatch()
      }
    }

    // the formatter rejects the '#' flag for null
    private def isNullForFormattable(spec: Spec, tpe: Type) =
      spec.kind == Kind.String && spec.hasFlag('#') && tpe <:< NullTpe && !(tpe <:< NothingTpe)

    // checks of a valid specifier that depend on the type of the arg it formats
    private def checkResolved(parts: List[Part], use: Use, spec: Spec, resolved: Type, argTpe: Type): Unit = {
      def at(offset: Spec => Int) = specPos(parts, use, offset)
      spec.kind match {
        case Kind.Boolean if !(resolved =:= BooleanTpe) =>
          warning(at(_.conversionAt), "Boolean format is null test for non-Boolean")
        case Kind.DateTime =>
          // A java.time class has only some of the fields; find out which with a sample value. An own
          // conversion passes the arg itself only if it is not converted.
          val passed = if (use.own && !(resolved =:= TemporalAccessorTpe)) resolved else argTpe
          val cls = passed.dealiasWiden.typeSymbol.fullName
          for (sample <- javaTimeSamples.get(cls))
            try String.format(s"%t${spec.cc}", sample())
            catch { case _: java.util.IllegalFormatException => error(at(_.conversionAt + 1), s"'${spec.cc}' is not a field of $cls") }
        case Kind.Integral if spec.cc != 'd' && !(resolved =:= BigIntTpe) =>
          "+ (".find(spec.hasFlag).foreach { f =>
            error(at(_.flagAt(f)), s"only use '$f' for BigInt conversions to o, x, X")
          }
        case Kind.String =>
          // at the arg if the conversion is inserted, as for the s interpolator
          def lintAt = if (use.spec.isEmpty) args(use.arg - 1).pos else at(_.conversionAt)
          if (linting && !(argTpe =:= StringTpe))
            if (argTpe.typeSymbol eq UnitClass)
              warning(lintAt, "interpolated Unit value", WFlagTostringInterpolated)
            else if (!isPrimitiveValueType(argTpe))
              warning(lintAt, "interpolation uses toString", WFlagTostringInterpolated)
        case _ =>
      }
    }

    // the position of a field of a use's specifier, or of its part if the `%s` is inserted
    private def specPos(parts: List[Part], use: Use, offset: Spec => Int): Position =
      parts(use.part).posAt(use.spec.fold(0)(offset))

    // 5. generate

    private def generate(parts: List[Part], program: Program, actuals: List[Tree]): Tree =
      if (actuals.forall(treeInfo.isLiteralString) && program.isFoldable)
        constant(String.format(java.util.Locale.ROOT, program.format, actuals.map(_.asInstanceOf[Literal].value.stringValue): _*))
      else if (program.isPlain) {
        val lits = program.plainParts.zip(parts).map { case (s, part) => atPos(part.tree.pos)(constant(s)) }
        withoutLinting { // already warned
          concatenate(lits, actuals.map(asString))
        }
      }
      else {
        val scalaPackage = Select(Ident(nme.ROOTPKG), TermName("scala"))
        val newStringOps = Select(
          New(Select(Select(Select(scalaPackage,
            TermName("collection")), TermName("immutable")), TypeName("StringOps"))),
          termNames.CONSTRUCTOR
        )
        val expr = Apply(Select(Apply(newStringOps, List(Literal(Constant(program.format)))), TermName("format")), actuals)
        expr.setPos(c.macroApplication.pos.makeTransparent)
      }

    // `%s` formats a Formattable with `formatTo`, and any other value as concatenating it does. The value's
    // class decides, so check it at run time unless the type tells: a value of a final class that is not a
    // Formattable cannot be one, nor can an Option, but a subclass of a case class can. The test is of an
    // Any, as testing a sealed type compiled in this run would later warn that it is fruitless.
    private def asString(actual: Tree): Tree = {
      val tpe = actual.tpe.dealiasWiden
      def formatted(t: Tree) = q"new _root_.scala.collection.immutable.StringOps(${"%s"}).format($t)"
      if (tpe <:< FormattableTpe) c.typecheck(atPos(actual.pos)(formatted(actual)))
      else if (isPrimitiveValueType(tpe) || tpe <:< NullTpe || global.analyzer.neverMatches(tpe, FormattableTpe)) actual
      else {
        val x = TermName(c.freshName("arg$"))
        c.typecheck(atPos(actual.pos)(q"""{
          val $x = $actual
          if (($x: _root_.scala.Any).isInstanceOf[_root_.java.util.Formattable]) ${formatted(q"$x")} else $x
        }"""))
      }
    }
  }

  // The types of arg that conversions take

  private lazy val BigDecimalTpe       = typeTag[BigDecimal].tpe
  private lazy val BigIntTpe           = typeTag[BigInt].tpe
  private lazy val JavaBigDecimalTpe   = typeTag[java.math.BigDecimal].tpe
  private lazy val JavaBigIntegerTpe   = typeTag[java.math.BigInteger].tpe
  private lazy val CalendarTpe         = typeTag[java.util.Calendar].tpe
  private lazy val DateTpe             = typeTag[java.util.Date].tpe
  private lazy val FormattableTpe      = typeTag[Formattable].tpe
  private lazy val TemporalAccessorTpe = rootMirror.requiredClass[java.time.temporal.TemporalAccessor].tpe
  private lazy val javaTimeTypes = javaTimeSamples.keys.toList.map(rootMirror.getClassIfDefined(_)).filter(_ != NoSymbol).map(_.tpe)

  /** The types of arg that a conversion takes. `exact` ones are tried first, also by implicit conversion,
   *  then `others` by conformance only.
   */
  private final case class Accepted(exact: List[Type], others: List[Type]) {
    def all = exact ::: others
    // the types of the values that the formatter takes for one of `t`
    def runtimeTypes(t: Type): List[Type] = t :: boxedClass.get(t.typeSymbol).map(_.tpe).toList ::: (
      if (t =:= BigIntTpe) JavaBigIntegerTpe :: Nil
      else if (t =:= BigDecimalTpe) JavaBigDecimalTpe :: Nil
      else if (t =:= DateTpe) TemporalAccessorTpe :: javaTimeTypes
      else if (t =:= TemporalAccessorTpe) javaTimeTypes
      else Nil)
  }

  private def accepted(spec: Spec): Accepted = spec.kind match {
    case Kind.String if spec.hasFlag('#') => Accepted(FormattableTpe :: Nil, Nil)
    case Kind.String                      => Accepted(Nil, AnyTpe :: Nil)
    case Kind.Boolean                     => Accepted(BooleanTpe :: NullTpe :: Nil, AnyTpe :: Nil)
    case Kind.Hash                        => Accepted(Nil, AnyTpe :: Nil)
    case Kind.Character                   => Accepted(CharTpe :: ByteTpe :: ShortTpe :: IntTpe :: Nil, Nil)
    case Kind.Integral                    => Accepted(IntTpe :: LongTpe :: ByteTpe :: ShortTpe :: BigIntTpe :: Nil, Nil)
    case Kind.Float if spec.isHexFloat    => Accepted(DoubleTpe :: FloatTpe :: Nil, Nil) // not BigDecimal
    case Kind.Float                       => Accepted(DoubleTpe :: FloatTpe :: BigDecimalTpe :: Nil, Nil)
    case Kind.DateTime                    => Accepted(LongTpe :: CalendarTpe :: DateTpe :: Nil, TemporalAccessorTpe :: Nil)
    case _                                => Accepted(Nil, Nil)
  }

  private def mismatchMessage(tpe: Type, types: List[Type]): String = "type mismatch" + {
    val req = raw"required: (.*)".r.unanchored
    val all = types.map(req => global.analyzer.foundReqMsg(tpe, req))
    if (all.lengthIs <= 1) all.mkString
    else all.head + all.tail.map { case req(what) => what case _ => "?" }.mkString(", ", ", ", "")
  }

  private def constant(s: String): Tree = Literal(Constant(s)).setType(ConstantType(Constant(s)))

  // Positions and escapes

  /** The position of the char at an offset in `text`, the value of a part whose source starts at `pos`.
   *  In the source, `$$` and `$"` are one char, and so is an escape if `text` has escapes processed.
   */
  private def sourcePosAt(pos: Position, text: String, escapes: Boolean): Int => Position = {
    // the offset in the source of each char of `text`, if the source matches
    lazy val sourceOffsets: Option[Array[Int]] = {
      val src = pos.source.content
      val start = pos.point
      val offsets = new Array[Int](text.length + 1)
      var i = start
      var k = 0
      var consistent = true
      while (consistent && k < text.length) {
        offsets(k) = i - start
        def next(n: Int) = i + n < src.length && src(i + n) == text(k)
        if (i >= src.length) consistent = false
        else if (src(i) == '$' && next(1)) i += 2
        else if (escapes && src(i) == '\\' && i + 1 < src.length) {
          if (src(i + 1) == 'u') { i += 1; while (i < src.length && src(i) == 'u') i += 1; i += 4 }
          else i += 2
        }
        else if (src(i) == text(k)) i += 1
        else consistent = false
        k += 1
      }
      offsets(text.length) = i - start
      if (consistent) Some(offsets) else None
    }
    offset =>
      if (!pos.isDefined) pos // a tree built by a toolbox, for example
      else sourceOffsets match {
        case Some(offsets) if offset >= 0 => pos.withPoint(pos.point + offsets(offset min text.length) + (offset - text.length max 0))
        case _                            => pos.withPoint(pos.point + offset)
      }
  }

  private def escapeHatch(s0: String, posAt: Int => Position): PartialFunction[Throwable, String] = {
    // trailing backslash, octal escape, or other
    case e: StringContext.InvalidEscapeException =>
      def errPoint = posAt(e.index)
      def octalOf(c: Char) = Character.digit(c, 8)
      def alt = {
        def altOf(i: Int) = i match {
          case '\b' => "\\b"
          case '\t' => "\\t"
          case '\n' => "\\n"
          case '\f' => "\\f"
          case '\r' => "\\r"
          case '\"' => "$" /* avoid lint warn */ +
            "{'\"'} or a triple-quoted literal \"\"\"with embedded \" or \\u0022\"\"\""
          case '\'' => "'"
          case '\\' => """\\"""
          case x    => "\\u%04x" format x
        }
        val suggest = {
          val r = "([0-7]{1,3}).*".r
          s0.drop(e.index + 1) match {
            case r(n) => altOf(n.foldLeft(0) { case (a, o) => (8 * a) + (o - '0') })
            case _    => ""
          }
        }
        if (suggest.isEmpty) ""
        else s"use $suggest instead"
      }
      def control(ctl: Char, i: Int, name: String) =
        c.error(errPoint, s"\\$ctl is not supported, but for $name use \\u${f"$i%04x"};\n${e.getMessage}")
      if (e.index == s0.length - 1) c.error(errPoint, """Trailing '\' escapes nothing.""")
      else s0(e.index + 1) match {
        case 'a' => control('a', 0x7, "alert or BEL")
        case 'v' => control('v', 0xB, "vertical tab")
        case 'e' => control('e', 0x1B, "escape")
        case i if octalOf(i) >= 0 => c.error(errPoint, s"octal escape literals are unsupported: $alt")
        case _   => c.error(errPoint, e.getMessage)
      }
      s0
    case e: StringContext.InvalidUnicodeEscapeException =>
      c.error(posAt(e.index), e.getMessage)
      s0
  }
}

/** The pure phases of the f-interpolator: parse, validate and resolve a format. Also the samples of
 *  java.time classes that type checking uses.
 *
 *  The rules follow `java.util.Formatter` on JDK 17 and later. Scala.js's clean-room implementation of it
 *  (https://github.com/scala-js/scala-js/blob/main/javalib/src/main/scala/java/util/Formatter.scala) is a
 *  readable reference for the arg selection and the flags that each conversion rejects.
 */
private[reflect] object FormatInterpolator {
  // 1. parse


  // `%[index$][flags][width][.precision]conversion`; the conversion is missing if the specifier is malformed
  private val formatPattern = """%(?:(\d+)\$)?([-#+ 0,(<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r

  /** The value of a numeric field of a specifier that does not fit in an Int. */
  final val TooLarge = -1

  sealed trait Piece
  final case class Text(text: String) extends Piece

  /** A specifier `%[index$][flags][width][.precision]conversion` at offset `start` in its part. The offsets of
   *  its fields are those in the part, or -1 if absent. `conversion` is empty if missing.
   */
  final case class Spec(text: String, start: Int,
                        index: Option[Int], indexAt: Int,
                        flags: String, flagsAt: Int,
                        width: Option[Int], widthAt: Int,
                        precision: Option[Int], precisionAt: Int,
                        conversion: String, conversionAt: Int) extends Piece {
    def hasFlag(f: Char): Boolean = flags.indexOf(f) >= 0
    def flagAt(f: Char): Int = flagsAt + (flags.indexOf(f) max 0)
    /** The conversion char, or for date/time conversions the one after `t` or `T`. */
    def cc: Char = conversion.length match { case 0 => '?' case 1 => conversion(0) case _ => conversion(1) }
    def kind: Kind = if (conversion.isEmpty) Kind.Unknown else kindOf(conversion(0))
    def isLiteral: Boolean = kind == Kind.Percent || kind == Kind.Newline
    def isHexFloat: Boolean = conversion == "a" || conversion == "A"
    /** Whether it refers to an arg by index or to the previous one by '<'. */
    def isReference: Boolean = index.nonEmpty || hasFlag('<')
    def withoutIndex: String = if (index.isEmpty) text else "%" + text.substring(text.indexOf('$') + 1)
    /** A plain `%s`, which formats like `String.valueOf` except for a Formattable. */
    def isPlainS: Boolean = conversion == "s" && flags.isEmpty && width.isEmpty && precision.isEmpty
    def isPlainPercent: Boolean = conversion == "%" && index.isEmpty && flags.isEmpty && width.isEmpty && precision.isEmpty
  }

  /** What kind of value a conversion formats. */
  sealed abstract class Kind
  object Kind {
    case object String    extends Kind
    case object Boolean   extends Kind
    case object Hash      extends Kind
    case object Character extends Kind
    case object Integral  extends Kind
    case object Float     extends Kind
    case object DateTime  extends Kind
    case object Percent   extends Kind
    case object Newline   extends Kind
    case object Unknown   extends Kind
  }

  def kindOf(c: Char): Kind = c match {
    case 's' | 'S'                         => Kind.String
    case 'b' | 'B'                         => Kind.Boolean
    case 'h' | 'H'                         => Kind.Hash
    case 'c' | 'C'                         => Kind.Character
    case 'd' | 'o' | 'x' | 'X'             => Kind.Integral
    case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' => Kind.Float
    case 't' | 'T'                         => Kind.DateTime
    case '%'                               => Kind.Percent
    case 'n'                               => Kind.Newline
    case _                                 => Kind.Unknown
  }

  /** The `%s` inserted for an arg without a conversion. */
  val PlainS: Spec = parse("%s").head.asInstanceOf[Spec]

  /** The text and specifiers of a part, after escapes are processed. */
  def parse(part: String): List[Piece] = {
    val pieces = ListBuffer.empty[Piece]
    var from = 0
    for (m <- formatPattern.findAllMatchIn(part)) {
      if (m.start > from) pieces += Text(part.substring(from, m.start))
      def int(g: Int, skip: Int = 0) = Option(m.group(g)).map(_.drop(skip).toIntOption.getOrElse(TooLarge))
      def str(g: Int) = Option(m.group(g)).getOrElse("")
      pieces += Spec(m.matched, m.start,
        int(1), m.start(1), str(2), m.start(2), int(3), m.start(3), int(4, skip = 1), m.start(4), str(5), m.start(5))
      from = m.end
    }
    if (from < part.length) pieces += Text(part.substring(from))
    pieces.toList
  }

  // 2. validate

  /** A problem at an offset in a part. */
  final case class Problem(offset: Int, msg: String, error: Boolean = true)

  val literalHelp = "use %% for literal %, %n for newline"

  /** The first error of a specifier on its own, if any, and its warnings. The order of the checks puts the
   *  more specific problems first.
   */
  def validate(spec: Spec): (Option[Problem], List[Problem]) = {
    import spec._
    val warnings = ListBuffer.empty[Problem]
    def err(offset: Int, msg: String) = Some(Problem(offset, msg))
    def badFlag(f: Char, msg: String) = err(flagAt(f), msg)
    def firstFlagNotIn(ok: String) = flags.find(f => !ok.contains(f))

    if (index.nonEmpty && hasFlag('<')) warnings += Problem(indexAt, "Argument index ignored if '<' flag is present", error = false)

    val dupe = flags.indices.find(i => flags.lastIndexOf(flags(i)) != i).map(i => flags.lastIndexOf(flags(i)))
    def requiresWidth(f: Char) = if (hasFlag(f) && width.isEmpty) badFlag(f, s"Flag '$f' requires a width") else None
    def exclusive(f: Char, g: Char) =
      if (hasFlag(f) && hasFlag(g)) err(flagsAt + (flags.indexOf(f) max flags.indexOf(g)), s"Illegal combination of flags '$f' and '$g'") else None
    def numeric = exclusive('+', ' ').orElse(exclusive('-', '0')).orElse(requiresWidth('-')).orElse(requiresWidth('0'))
    def padded = requiresWidth('-')
    def noPrecision = if (precision.nonEmpty) err(precisionAt, "precision not allowed") else None
    // a conversion without a precision reports one as not allowed, also if it is too large
    def takesPrecision = kind match {
      case Kind.String | Kind.Boolean | Kind.Hash | Kind.Float => true
      case _                                                    => false
    }
    def onlyDash(what: String) = firstFlagNotIn("-<").flatMap(f => badFlag(f, s"Only '-' allowed for $what"))
    def illegalFlags(ok: String) = firstFlagNotIn(ok).flatMap(f => badFlag(f, s"Illegal flag '$f'"))
    def notAllowed(bad: String, what: String) = bad.find(hasFlag).flatMap(f => badFlag(f, s"'$f' not allowed for $what"))

    val error: Option[Problem] =
      if (conversion.isEmpty) err(start, s"Missing conversion operator in '$text'; $literalHelp")
      else if (index.exists(_ <= 0)) err(indexAt, "Argument index out of range") // also TooLarge
      else if (dupe.nonEmpty) err(flagsAt + dupe.get, s"Duplicate flag '${flags(dupe.get)}'")
      else if (width.contains(TooLarge)) err(widthAt, "Width out of range")
      else if (precision.contains(TooLarge) && takesPrecision) err(precisionAt + 1, "Precision out of range")
      else kind match {
        case Kind.Unknown  => err(conversionAt, s"illegal conversion character '$cc'")
        case Kind.Newline  =>
          if (flags.nonEmpty) err(flagsAt, "flags not allowed")
          else if (width.nonEmpty) err(widthAt, "width not allowed")
          else noPrecision
        case Kind.Percent  => noPrecision.orElse(illegalFlags("-")).orElse(padded)
        case Kind.String   => illegalFlags("-#<").orElse(padded)
        case Kind.Boolean | Kind.Hash => illegalFlags("-<").orElse(padded)
        case Kind.Character => noPrecision.orElse(onlyDash("c conversion")).orElse(padded)
        case Kind.Integral =>
          noPrecision
            .orElse(if (cc == 'd' && hasFlag('#')) badFlag('#', "# not allowed for d conversion") else None)
            .orElse(if (cc != 'd' && hasFlag(',')) badFlag(',', "',' only allowed for d conversion of integral types") else None)
            .orElse(numeric)
        case Kind.Float =>
          (cc match {
            case 'a' | 'A' => notAllowed(",(", "a, A")
            case 'e' | 'E' => notAllowed(",", "e, E")
            case 'g' | 'G' => notAllowed("#", "g, G")
            case _         => None
          }).orElse(numeric)
        case Kind.DateTime =>
          if (conversion.length != 2) err(conversionAt, "Date/time conversion must have two characters")
          else if (!"HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc".contains(cc)) err(conversionAt + 1, s"'$cc' doesn't seem to be a date or time conversion")
          else noPrecision.orElse(onlyDash("date/time conversions")).orElse(padded)
      }
    (error, warnings.toList)
  }

  // 3. resolve

  /** Text, a literal specifier, or the use of an arg. `spec` is `None` for an inserted `%s`. */
  sealed trait Item
  final case class TextItem(text: String) extends Item
  final case class LiteralItem(part: Int, spec: Spec) extends Item
  /** A specifier that uses `arg` (1-based). `own` if it is the conversion right after the arg's splice. */
  final case class Use(part: Int, spec: Option[Spec], arg: Int, own: Boolean) extends Item

  /** The resolved format, with the problems found resolving it, by part. */
  final case class Program(items: List[Item], problems: List[(Int, Problem)]) {
    def uses: List[Use] = items.collect { case u: Use => u }

    /** The format string. An own conversion is written without an index, as it is the next arg of the
     *  formatter's own counter, and a reference as it is.
     */
    def format: String = items.map {
      case TextItem(t)                           => t
      case LiteralItem(_, spec)                  => spec.text
      case Use(_, None, _, _)                    => "%s"
      case Use(_, Some(spec), _, true)           => spec.withoutIndex
      case Use(_, Some(spec), _, false)          => spec.text
    }.mkString

    /** Whether the format only appends each arg, in order, as by `String.valueOf`. */
    def isPlain: Boolean = items.forall {
      case TextItem(_)           => true
      case LiteralItem(_, spec)  => spec.isPlainPercent
      case Use(_, spec, _, own)  => own && spec.forall(_.isPlainS)
    }

    /** Whether formatting string args gives the same result on every platform: the line separator, the
     *  locale and the JDK do not matter.
     */
    def isFoldable: Boolean = items.forall {
      case TextItem(_)                 => true
      case LiteralItem(_, spec)        => spec.isPlainPercent
      case Use(_, None, _, _)          => true
      case Use(_, Some(spec), _, _)    => spec.conversion == "s" && !spec.hasFlag('#')
    }

    /** For a plain format, the text before the first arg, between args, and after the last. */
    def plainParts: List[String] = {
      val parts = ListBuffer.empty[String]
      val current = new StringBuilder
      items.foreach {
        case TextItem(t)       => current ++= t
        case LiteralItem(_, _) => current += '%'
        case _: Use            => parts += current.result(); current.clear()
      }
      (parts += current.result()).toList
    }
  }

  /** Insert `%s` where an arg has no conversion, and select the arg of each specifier as the formatter does:
   *  `<` takes the previous one, an index that one, and any other specifier the next of its own counter.
   *  An invalid specifier right after a splice is taken as the arg's conversion, so that it causes no
   *  further problems.
   */
  def resolve(parts: List[List[Piece]], argc: Int, valid: Spec => Boolean): Program = {
    val items = ListBuffer.empty[Item]
    val problems = ListBuffer.empty[(Int, Problem)]
    var last = 0 // the arg of the previous specifier that takes one
    def problem(n: Int, offset: Int, msg: String, error: Boolean = true) = problems += ((n, Problem(offset, msg, error)))

    def reference(n: Int, spec: Spec): Unit =
      if (spec.hasFlag('<')) {
        if (last == 0) problem(n, spec.flagAt('<'), "No last arg")
        else items += Use(n, Some(spec), last, own = false)
      }
      else {
        val i = spec.index.get
        if (i > argc) problem(n, spec.indexAt, "Argument index out of range")
        else { items += Use(n, Some(spec), i, own = false); last = i }
      }

    for ((pieces, n) <- parts.zipWithIndex) {
      val rest = pieces match {
        case (spec: Spec) :: rest if n > 0 && spec.start == 0 && !spec.isLiteral && (!valid(spec) || !spec.isReference || (!spec.hasFlag('<') && spec.index.contains(n))) =>
          items += Use(n, Some(spec), n, own = true)
          last = n
          rest
        case _ if n > 0 =>
          pieces match {
            case (spec: Spec) :: _ if spec.start == 0 && valid(spec) && spec.isReference && !spec.isLiteral && !(spec.index.nonEmpty && spec.hasFlag('<')) =>
              // "$x$y%1$s" where "%1$s" right after `$y` does not format it
              problem(n, if (spec.hasFlag('<')) spec.flagAt('<') else spec.indexAt, "Index is not this arg", error = false)
            case _ =>
          }
          items += Use(n, None, n, own = true)
          last = n
          pieces
        case _ => pieces
      }
      rest.foreach {
        case Text(t) => items += TextItem(t)
        case spec: Spec if !valid(spec) => items += LiteralItem(n, spec) // not used, there are errors
        case spec: Spec if spec.isLiteral => items += LiteralItem(n, spec)
        case spec: Spec if spec.isReference => reference(n, spec)
        case spec: Spec => problem(n, spec.start, s"conversions must follow a splice; $literalHelp")
      }
    }
    Program(items.toList, problems.toList)
  }

  // 4. type check

  /** A value of each of java.time's final date and time classes, by class name. */
  val javaTimeSamples: Map[String, () => AnyRef] = {
    import java.time._
    def dateTime = LocalDateTime.of(2000, 1, 2, 3, 4, 5)
    Map(
      "java.time.Instant"        -> (() => Instant.EPOCH),
      "java.time.LocalDate"      -> (() => dateTime.toLocalDate),
      "java.time.LocalTime"      -> (() => dateTime.toLocalTime),
      "java.time.LocalDateTime"  -> (() => dateTime),
      "java.time.OffsetDateTime" -> (() => dateTime.atOffset(ZoneOffset.UTC)),
      "java.time.OffsetTime"     -> (() => dateTime.toLocalTime.atOffset(ZoneOffset.UTC)),
      "java.time.ZonedDateTime"  -> (() => dateTime.atZone(ZoneId.of("UTC"))),
      "java.time.Year"           -> (() => Year.of(2000)),
      "java.time.YearMonth"      -> (() => YearMonth.of(2000, 1)),
      "java.time.MonthDay"       -> (() => MonthDay.of(1, 2)),
    )
  }
}
