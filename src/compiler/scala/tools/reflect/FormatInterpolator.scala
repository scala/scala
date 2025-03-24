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

import scala.PartialFunction.cond
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.runtime.Context
import scala.tools.nsc.Reporting.WarningCategory, WarningCategory.WFlagTostringInterpolated
import scala.util.matching.Regex.Match
import scala.util.chaining._

import java.util.Formattable

abstract class FormatInterpolator {
  import FormatInterpolator._
  import SpecifierGroups.{Value => SpecGroup, _}

  val c: Context
  val global: c.universe.type = c.universe

  import c.universe.{Match => _, _}
  import definitions._
  import treeInfo.Applied

  protected var linting = settings.warnToString.value

  protected final def withoutLinting[A](body: => A): A = {
    val linted = linting
    linting = false
    try body finally linting = linted
  }

  private def bail(msg: String) = global.abort(msg)

  def concatenate(parts: List[Tree], args: List[Tree]): Tree

  def interpolateF: Tree = c.macroApplication match {
    //case q"$_(..$parts).f(..$args)" =>
    case Applied(Select(Apply(_, parts), _), _, argss) =>
      val args = argss.flatten
      def badlyInvoked = parts.lengthIs != args.length + 1 and {
        def because(s: String) = s"too $s arguments for interpolated string"
        val (p, msg) =
          if (parts.isEmpty) (c.prefix.tree.pos, "there are no parts")
          else if (parts.lengthIs > (args.length + 1))
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
   *  the literals (%% and %n) or an index reference (%1\$ or %<).
   *
   *  A conversion specifier has the form:
   *
   *  [index\$][flags][width][.precision]conversion
   *
   *  1) "...\${smth}" => okay, equivalent to "...\${smth}%s"
   *  2) "...\${smth}blahblah" => okay, equivalent to "...\${smth}%sblahblah"
   *  3) "...\${smth}%" => error
   *  4) "...\${smth}%n" => okay, equivalent to "...\${smth}%s%n"
   *  5) "...\${smth}%%" => okay, equivalent to "...\${smth}%s%%"
   *  6) "...\${smth}[%legalJavaConversion]" => okay*
   *  7) "...\${smth}[%illegalJavaConversion]" => error
   *  *Legal according to [[java.util.Formatter]]
   */
  def interpolated(parts: List[Tree], args: List[Tree]): Tree = {
    val argTypes = args.map(_.tpe)
    val argc = argTypes.length
    // amended parts and actual args to use, in amended.mkString.format(actuals)
    val amended = ListBuffer.empty[String]
    val actuals = ListBuffer.empty[Tree]
    val convert = ListBuffer.empty[Conversion]

    // whether this format does more than concatenate strings
    var formatting = false

    def argType(argi: Int, types: Type*): Type = {
      val tpe = argTypes(argi)
      types.find(t => t != AnyTpe && argConformsTo(argi, tpe, t))
        .orElse(types.find(t => t != AnyTpe && argConvertsTo(argi, tpe, t)))
        .orElse(types.find(t => t == AnyTpe && argConformsTo(argi, tpe, t)))
        .getOrElse {
          val msg = "type mismatch" + {
            val req = raw"required: (.*)".r.unanchored
            val all = types.map(req => global.analyzer.foundReqMsg(tpe, req))
            if (all.isEmpty) ""
            else if (all.length == 1) all.head
            else all.head + all.tail.map { case req(what) => what case _ => "?" }.mkString(", ", ", ", "")
          }
          c.error(args(argi).pos, msg)
          reported = true
          actuals += args(argi)
          types.head
        }
    }
    def argConformsTo(argi: Int, arg: Type, target: Type): Boolean = (arg <:< target).tap(if (_) actuals += args(argi))
    def argConvertsTo(argi: Int, arg: Type, target: Type): Boolean =
      c.inferImplicitView(args(argi), arg, target) match {
        case EmptyTree  => false
        case _ =>
          // let the compiler figure out how to apply the conversion
          val freshName = TermName(c.freshName("arg$"))
          val value = args(argi)
          val ValDef(_, _, _, rhs) = c.typecheck(ValDef(Modifiers(), freshName, TypeTree(target).setPos(value.pos.focus), value).setPos(value.pos)): @unchecked
          actuals += rhs
          true
      }

    // Append the nth part to the string builder, possibly prepending an omitted %s first.
    // Check the % fields in this part.
    def loop(remaining: List[Tree], n: Int): Unit =
      remaining match {
        case part0 :: remaining =>
          val part1 = part0 match {
            case Literal(Constant(x: String)) => x
            case _ => throw new IllegalArgumentException("internal error: argument parts must be a list of string literals")
          }
          val part = try StringContext.processEscapes(part1) catch escapeHatch(c)(part1, part0.pos)
          val matches = formatPattern.findAllMatchIn(part)

          def insertStringConversion(): Unit = {
            amended += "%s" + part
            val cv = Conversion(part0.pos, argc)
            cv.accepts(argType(n-1, AnyTpe))
            convert += cv
            cv.lintToString(argTypes(n-1))
          }
          def errorLeading(op: Conversion) = op.errorAt(Spec)(s"conversions must follow a splice; ${Conversion.literalHelp}")
          def accept(op: Conversion): Unit = {
            if (!op.isLeading) errorLeading(op)
            op.accepts(argType(n-1, op.acceptableVariants: _*))
            amended += part
            op.lintToString(argTypes(n-1))
          }

          if (n == 0) amended += part
          else if (!matches.hasNext) insertStringConversion()
          else {
            val cv = Conversion(matches.next(), part0.pos, argc)
            if (cv.kind != Kind.StringXn || cv.cc.isUpper || cv.width.nonEmpty || cv.flags.nonEmpty)
              formatting = true
            if (cv.isLiteral) insertStringConversion()
            else if (cv.isIndexed) {
              if (cv.index.getOrElse(-1) == n) accept(cv)
              else {
                // "$x$y%1" where "%1" follows a splice but does not apply to it
                c.warning(cv.groupPosAt(Index, 0), "Index is not this arg")
                insertStringConversion()
              }
            }
            else if (!cv.isError) accept(cv)
          }
          // any remaining conversions in this part must be either literals or indexed
          while (matches.hasNext) {
            val cv = Conversion(matches.next(), part0.pos, argc)
            if (n == 0 && cv.hasFlag('<')) cv.badFlag('<', "No last arg")
            else if (!cv.isLiteral && !cv.isIndexed) errorLeading(cv)
            formatting = true
          }
          loop(remaining, n = n + 1)
        case Nil =>
      }
    loop(parts, n = 0)

    def constantly(s: String) = {
      val k = Constant(s)
      Literal(k).setType(ConstantType(k))
    }

    //q"{..$evals; new StringOps(${fstring.toString}).format(..$ids)}"
    val format = amended.mkString
    if (actuals.isEmpty && !formatting) constantly(format)
    else if (!reported && actuals.forall(treeInfo.isLiteralString)) constantly(format.format(actuals.map(_.asInstanceOf[Literal].value.value).toIndexedSeq: _*))
    else if (!formatting) {
      withoutLinting { // already warned
        concatenate(amended.map(p => constantly(p.stripPrefix("%s"))).toList, actuals.toList)
      }
    }
    else {
      val scalaPackage = Select(Ident(nme.ROOTPKG), TermName("scala"))
      val newStringOps = Select(
        New(Select(Select(Select(scalaPackage,
          TermName("collection")), TermName("immutable")), TypeName("StringOps"))),
        termNames.CONSTRUCTOR
      )
      val expr = Apply(Select(Apply(newStringOps, List(Literal(Constant(format)))), TermName("format")), actuals.toList)
      val p = c.macroApplication.pos
      expr.setPos(p.makeTransparent)
    }
  }

  val BigDecimalTpe = typeTag[BigDecimal].tpe
  val BigIntTpe = typeTag[BigInt].tpe
  val CalendarTpe = typeTag[java.util.Calendar].tpe
  val DateTpe = typeTag[java.util.Date].tpe
  val FormattableTpe = typeTag[Formattable].tpe

  object Kind extends Enumeration { val StringXn, HashXn, BooleanXn, CharacterXn, IntegralXn, FloatingPointXn, DateTimeXn, LiteralXn, ErrorXn = Value }
  import Kind.{Value => KindOf, _}

  /** A conversion specifier matched in the argi'th string part, with `argc` arguments to interpolate.
   */
  final class Conversion(val descriptor: Match, pos: Position, val kind: KindOf, argc: Int) {
    // the descriptor fields
    val index: Option[Int]     = descriptor.intOf(Index)
    val flags: String          = descriptor.stringOf(Flags)
    val width: Option[Int]     = descriptor.intOf(Width)
    val precision: Option[Int] = descriptor.group(Precision).map(_.drop(1).toInt)
    val op: String             = descriptor.stringOf(CC)

    // the conversion char is the head of the op string (but see DateTimeXn)
    val cc: Char =
      kind match {
        case ErrorXn if op.isEmpty       => '?'
        case ErrorXn                     => op(0)
        case DateTimeXn if op.length > 1 => op(1)
        case DateTimeXn                  => '?'
        case StringXn if op.isEmpty      => 's' // accommodate the default %s
        case _ => op(0)
      }

    def isIndexed: Boolean = index.nonEmpty || hasFlag('<')
    def isError: Boolean   = kind == ErrorXn
    def isLiteral: Boolean = kind == LiteralXn

    // descriptor is at index 0 of the part string
    def isLeading: Boolean = descriptor.at(Spec) == 0

    // true if passes.
    def verify: Boolean = {
      // various assertions
      def goodies = goodFlags && goodIndex
      def noFlags = flags.isEmpty or errorAt(Flags)("flags not allowed")
      def noWidth = width.isEmpty or errorAt(Width)("width not allowed")
      def noPrecision = precision.isEmpty or errorAt(Precision)("precision not allowed")
      def only_-(msg: String) = {
        val badFlags = flags.filterNot { case '-' | '<' => true case _ => false }
        badFlags.isEmpty or badFlag(badFlags(0), s"Only '-' allowed for $msg")
      }
      def goodFlags = flags.isEmpty || {
        for (dupe <- flags.diff(flags.distinct).distinct) errorAt(Flags, flags.lastIndexOf(dupe))(s"Duplicate flag '$dupe'")
        val badFlags = flags.filterNot(okFlags.contains(_))
        for (f <- badFlags) badFlag(f, s"Illegal flag '$f'")
        badFlags.isEmpty
      }
      def goodIndex = !isIndexed || {
        if (index.nonEmpty && hasFlag('<')) warningAt(Index)("Argument index ignored if '<' flag is present")
        val okRange = index.map(i => i > 0 && i <= argc).getOrElse(true)
        okRange || hasFlag('<') or errorAt(Index)("Argument index out of range")
      }
      // begin verify
      kind match {
        case StringXn        => goodies
        case BooleanXn       => goodies
        case HashXn          => goodies
        case CharacterXn     => goodies && noPrecision && only_-("c conversion")
        case IntegralXn      =>
          def d_# = cc == 'd' && hasFlag('#') and badFlag('#', "# not allowed for d conversion")
          def x_comma = cc != 'd' && hasFlag(',') and badFlag(',', "',' only allowed for d conversion of integral types")
          goodies && noPrecision && !d_# && !x_comma
        case FloatingPointXn =>
          goodies && (cc match {
            case 'a' | 'A' =>
              val badFlags = ",(".filter(hasFlag)
              noPrecision && badFlags.isEmpty or badFlags.foreach(badf => badFlag(badf, s"'$badf' not allowed for a, A"))
            case _ => true
          })
        case DateTimeXn      =>
          def hasCC = op.length == 2 or errorAt(CC)("Date/time conversion must have two characters")
          def goodCC = "HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc".contains(cc) or errorAt(CC, 1)(s"'$cc' doesn't seem to be a date or time conversion")
          goodies && hasCC && goodCC && noPrecision && only_-("date/time conversions")
        case LiteralXn       =>
          op match {
            case "%" => goodies && noPrecision and width.foreach(_ => warningAt(Width)("width ignored on literal"))
            case "n" => noFlags && noWidth && noPrecision
          }
        case ErrorXn         =>
          errorAt(CC)(s"illegal conversion character '$cc'")
          false
        case _               =>
          errorAt(CC)(s"bad conversion '$kind' for '$cc'")
          false
      }
    }

    // is the specifier OK with the given arg
    def accepts(arg: Type): Boolean =
      kind match {
        case BooleanXn  => arg == BooleanTpe orElse warningAt(CC)("Boolean format is null test for non-Boolean")
        case IntegralXn =>
          arg == BigIntTpe || !cond(cc) {
            case 'o' | 'x' | 'X' if hasAnyFlag("+ (") => "+ (".filter(hasFlag).foreach(bad => badFlag(bad, s"only use '$bad' for BigInt conversions to o, x, X")) ; true
          }
        case _ => true
      }
    def lintToString(arg: Type): Unit =
      if (linting && kind == StringXn && !(arg =:= StringTpe))
        if (arg.typeSymbol eq UnitClass)
          warningAt(CC)("interpolated Unit value", WFlagTostringInterpolated)
        else if (!definitions.isPrimitiveValueType(arg))
          warningAt(CC)("interpolation uses toString", WFlagTostringInterpolated)

    // what arg type if any does the conversion accept
    def acceptableVariants: List[Type] =
      kind match {
        case StringXn if hasFlag('#') => FormattableTpe :: Nil
        case StringXn                 => AnyTpe :: Nil
        case BooleanXn                => BooleanTpe :: NullTpe :: AnyTpe :: Nil // warn if not boolean
        case HashXn                   => AnyTpe :: Nil
        case CharacterXn              => CharTpe :: ByteTpe :: ShortTpe :: IntTpe :: Nil
        case IntegralXn               => IntTpe :: LongTpe :: ByteTpe :: ShortTpe :: BigIntTpe :: Nil
        case FloatingPointXn          => DoubleTpe :: FloatTpe :: BigDecimalTpe :: Nil
        case DateTimeXn               => LongTpe :: CalendarTpe :: DateTpe :: Nil
        case LiteralXn                => Nil
        case ErrorXn                  => Nil
        case _                        => errorAt(CC)(s"bad conversion '$kind' for '$cc'") ; Nil
      }

    // what flags does the conversion accept?
    private def okFlags: String =
      kind match {
        case StringXn           => "-#<"
        case BooleanXn | HashXn => "-<"
        case LiteralXn          => "-"
        case _                  => "-#+ 0,(<"
      }

    def hasFlag(f: Char) = flags.contains(f)
    def hasAnyFlag(fs: String) = fs.exists(hasFlag)

    def badFlag(f: Char, msg: String) = {
      val i = flags.indexOf(f) match { case -1 => 0 case j => j }
      errorAt(Flags, i)(msg)
    }

    def groupPosAt(g: SpecGroup, i: Int) = pos.withPoint(pos.point + descriptor.offset(g, i))
    def errorAt(g: SpecGroup, i: Int = 0)(msg: String)   = c.error(groupPosAt(g, i), msg).tap(_ => reported = true)
    def warningAt(g: SpecGroup, i: Int = 0)(msg: String, cat: WarningCategory = WarningCategory.Other) = c.callsiteTyper.context.warning(groupPosAt(g, i), msg, cat, Nil)
  }

  object Conversion {
    def apply(m: Match, p: Position, argc: Int): Conversion = {
      def kindOf(cc: Char) = cc match {
        case 's' | 'S' => StringXn
        case 'h' | 'H' => HashXn
        case 'b' | 'B' => BooleanXn
        case 'c' | 'C' => CharacterXn
        case 'd' | 'o' |
             'x' | 'X' => IntegralXn
        case 'e' | 'E' |
             'f' |
             'g' | 'G' |
             'a' | 'A' => FloatingPointXn
        case 't' | 'T' => DateTimeXn
        case '%' | 'n' => LiteralXn
        case _         => ErrorXn
      }
      m.group(CC) match {
        case Some(cc) => new Conversion(m, p, kindOf(cc(0)), argc).tap(_.verify)
        case None     => new Conversion(m, p, ErrorXn, argc).tap(_.errorAt(Spec)(s"Missing conversion operator in '${m.matched}'; $literalHelp"))
      }
    }
    // construct a default %s conversion
    def apply(p: Position, argc: Int): Conversion =
      new Conversion(formatPattern.findAllMatchIn("%").next(), p, StringXn, argc)
    val literalHelp = "use %% for literal %, %n for newline"
  }

  var reported = false
}
object FormatInterpolator {
  // match a conversion specifier
  private val formatPattern = """%(?:(\d+)\$)?([-#+ 0,(<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r
  // ordinal is the regex group index in the format pattern
  private object SpecifierGroups extends Enumeration { val Spec, Index, Flags, Width, Precision, CC = Value }
  import SpecifierGroups.{Value => SpecGroup}
  private implicit class `enumlike`(val value: SpecGroup) extends AnyVal {
    def ordinal = value.id
  }

  private implicit class `boolean whimsy`(val value: Boolean) extends AnyVal {
    def or(body: => Unit): Boolean     = value || { body ; false }
    def orElse(body: => Unit): Boolean = value || { body ; true }
    def and(body: => Unit): Boolean    = value && { body ; true }
    def but(body: => Unit): Boolean    = value && { body ; false }
  }
  private implicit class `match game`(val descriptor: Match) extends AnyVal {
    def at(g: SpecGroup): Int                 = descriptor.start(g.ordinal)
    def offset(g: SpecGroup, i: Int = 0): Int = at(g) + i
    def group(g: SpecGroup): Option[String]   = Option(descriptor.group(g.ordinal))
    def stringOf(g: SpecGroup): String        = group(g).getOrElse("")
    def intOf(g: SpecGroup): Option[Int]      = group(g).map(_.toInt)
  }
  private def escapeHatch(c: Context)(s0: String, pos: c.universe.Position): PartialFunction[Throwable, String] = {
    // trailing backslash, octal escape, or other
    case e: StringContext.InvalidEscapeException =>
      def errPoint = pos.withPoint(pos.point + e.index)
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
  }
}
