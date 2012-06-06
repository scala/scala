/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.cmd
package gen

/** Code generation of the AnyVal types and their companions.
 */
trait AnyValReps {
  self: AnyVals =>

  sealed abstract class AnyValNum(name: String, repr: Option[String], javaEquiv: String) extends AnyValRep(name,repr,javaEquiv) {

    case class Op(val op : String, val doc : String)
    
    private def companionCoercions(tos: AnyValRep*) = {
      tos.toList map (to =>
        """implicit def @javaequiv@2%s(x: @name@): %s = x.to%s""".format(to.javaEquiv, to.name, to.name)
      )
    }
    def coercionCommentExtra = ""
    def coercionComment = """
  /** Language mandated coercions from @name@ to "wider" types.%s
   */""".format(coercionCommentExtra)
    
    def implicitCoercions: List[String] = {
      val coercions = this match {
        case B     => companionCoercions(S, I, L, F, D)
        case S | C => companionCoercions(I, L, F, D)
        case I     => companionCoercions(L, F, D)
        case L     => companionCoercions(F, D)
        case F     => companionCoercions(D)
        case _     => Nil
      }
      if (coercions.isEmpty) Nil
      else coercionComment :: coercions
    }

    def isCardinal: Boolean = isIntegerType(this)
    def unaryOps = {
      val ops = List(
        Op("+", "/**\n" +
                " * Returns this value, unmodified.\n" +
                " */"),
        Op("-", "/**\n" +
                " * Returns the negation of this value.\n" +
                " */"))

      if(isCardinal)
        Op("~", "/**\n" +
                " * Returns the bitwise negation of this value.\n" +
                " * @example {{{\n" +
                " * ~5 == -6\n" +
                " * // in binary: ~00000101 ==\n" +
                " * //             11111010\n" +
                " * }}}\n" +
                " */") :: ops
      else ops
    }

    def bitwiseOps =
      if (isCardinal)
        List(
          Op("|", "/**\n" +
                     "  * Returns the bitwise OR of this value and `x`.\n" +
                     "  * @example {{{\n" +
                     "  * (0xf0 | 0xaa) == 0xfa\n" +
                     "  * // in binary:   11110000\n" +
                     "  * //            | 10101010\n" +
                     "  * //              --------\n" +
                     "  * //              11111010\n" +
                     "  * }}}\n" +
                     "  */"),
          Op("&", "/**\n" +
                     "  * Returns the bitwise AND of this value and `x`.\n" +
                     "  * @example {{{\n" +
                     "  * (0xf0 & 0xaa) == 0xa0\n" +
                     "  * // in binary:   11110000\n" +
                     "  * //            & 10101010\n" +
                     "  * //              --------\n" +
                     "  * //              10100000\n" +
                     "  * }}}\n" +
                     "  */"),
          Op("^", "/**\n" +
                     "  * Returns the bitwise XOR of this value and `x`.\n" +
                     "  * @example {{{\n" +
                     "  * (0xf0 ^ 0xaa) == 0x5a\n" +
                     "  * // in binary:   11110000\n" +
                     "  * //            ^ 10101010\n" +
                     "  * //              --------\n" +
                     "  * //              01011010\n" +
                     "  * }}}\n" +
                     "  */"))
      else Nil

    def shiftOps            =
      if (isCardinal)
        List(
          Op("<<",  "/**\n" +
                       "  * Returns this value bit-shifted left by the specified number of bits,\n" +
                       "  *         filling in the new right bits with zeroes.\n" +
                       "  * @example {{{ 6 << 3 == 48 // in binary: 0110 << 3 == 0110000 }}}\n" +
                       "  */"),

          Op(">>>", "/**\n" +
                       "  * Returns this value bit-shifted right by the specified number of bits,\n" +
                       "  *         filling the new left bits with zeroes.\n" +
                       "  * @example {{{ 21 >>> 3 == 2 // in binary: 010101 >>> 3 == 010 }}}\n" +
                       "  * @example {{{\n" +
                       "  * -21 >>> 3 == 536870909\n" +
                       "  * // in binary: 11111111 11111111 11111111 11101011 >>> 3 ==\n" +
                       "  * //            00011111 11111111 11111111 11111101\n" +
                       "  * }}}\n" +
                       "  */"),

          Op(">>",  "/**\n" +
                       "  * Returns this value bit-shifted left by the specified number of bits,\n" +
                       "  *         filling in the right bits with the same value as the left-most bit of this.\n" +
                       "  *         The effect of this is to retain the sign of the value.\n" +
                       "  * @example {{{\n" +
                       "  * -21 >> 3 == -3\n" +
                       "  * // in binary: 11111111 11111111 11111111 11101011 >> 3 ==\n" +
                       "  * //            11111111 11111111 11111111 11111101\n" +
                       "  * }}}\n" +
                       "  */"))
      else Nil

    def comparisonOps       = List(
      Op("==", "/**\n  * Returns `true` if this value is equal to x, `false` otherwise.\n  */"),
      Op("!=", "/**\n  * Returns `true` if this value is not equal to x, `false` otherwise.\n  */"),
      Op("<",  "/**\n  * Returns `true` if this value is less than x, `false` otherwise.\n  */"),
      Op("<=", "/**\n  * Returns `true` if this value is less than or equal to x, `false` otherwise.\n  */"),
      Op(">",  "/**\n  * Returns `true` if this value is greater than x, `false` otherwise.\n  */"),
      Op(">=", "/**\n  * Returns `true` if this value is greater than or equal to x, `false` otherwise.\n  */"))

    def otherOps = List(
      Op("+", "/**\n  * Returns the sum of this value and `x`.\n  */"),
      Op("-", "/**\n  * Returns the difference of this value and `x`.\n  */"),
      Op("*", "/**\n  * Returns the product of this value and `x`.\n  */"),
      Op("/", "/**\n  * Returns the quotient of this value and `x`.\n  */"),
      Op("%", "/**\n  * Returns the remainder of the division of this value by `x`.\n  */"))

    // Given two numeric value types S and T , the operation type of S and T is defined as follows:
    // If both S and T are subrange types then the operation type of S and T is Int.
    // Otherwise the operation type of S and T is the larger of the two types wrt ranking.
    // Given two numeric values v and w the operation type of v and w is the operation type
    // of their run-time types.
    def opType(that: AnyValNum): AnyValNum = {
      val rank = IndexedSeq(I, L, F, D)
      (rank indexOf this, rank indexOf that) match {
        case (-1, -1)   => I
        case (r1, r2)   => rank apply (r1 max r2)
      }
    }

    def mkCoercions = numeric map (x => "def to%s: %s".format(x, x))
    def mkUnaryOps  = unaryOps map (x => "%s\n  def unary_%s : %s".format(x.doc, x.op, this opType I))
    def mkStringOps = List("def +(x: String): String")
    def mkShiftOps  = (
      for (op <- shiftOps ; arg <- List(I, L)) yield
        "%s\n  def %s(x: %s): %s".format(op.doc, op.op, arg, this opType I)
    )

    def clumps: List[List[String]] = {
      val xs1 = List(mkCoercions, mkUnaryOps, mkStringOps, mkShiftOps) map (xs => if (xs.isEmpty) xs else xs :+ "")
      val xs2 = List(
        mkBinOpsGroup(comparisonOps, numeric, _ => Z),
        mkBinOpsGroup(bitwiseOps, cardinal, this opType _),
        mkBinOpsGroup(otherOps, numeric, this opType _)
      )
      xs1 ++ xs2
    }
    def classLines = (clumps :+ commonClassLines).foldLeft(List[String]()) {
      case (res, Nil)   => res
      case (res, lines) =>
        val xs = lines map {
          case ""   => ""
          case s    => interpolate(s)
        }
        res ++ xs
    }
    def objectLines = {
      val comp = if (isCardinal) cardinalCompanion else floatingCompanion
      (comp + allCompanions + "\n" + nonUnitCompanions).trim.lines.toList ++ implicitCoercions map interpolate
    }

    /** Makes a set of binary operations based on the given set of ops, args, and resultFn.
     *
     *  @param    ops       list of function names e.g. List(">>", "%")
     *  @param    args      list of types which should appear as arguments
     *  @param    resultFn  function which calculates return type based on arg type
     *  @return             list of function definitions
     */
    def mkBinOpsGroup(ops: List[Op], args: List[AnyValNum], resultFn: AnyValNum => AnyValRep): List[String] = (
      ops flatMap (op =>
        args.map(arg =>
          "%s\n  def %s(x: %s): %s".format(op.doc, op.op, arg, resultFn(arg))) :+ ""
      )
    ).toList
  }

  sealed abstract class AnyValRep(val name: String, val repr: Option[String], val javaEquiv: String) {
    def classLines: List[String]
    def objectLines: List[String]
    def commonClassLines = List(
      "override def getClass(): Class[@name@] = null"
    )

    def lcname = name.toLowerCase
    def boxedName = this match {
      case U => "scala.runtime.BoxedUnit"
      case C => "java.lang.Character"
      case I => "java.lang.Integer"
      case _ => "java.lang." + name
    }
    def zeroRep = this match {
      case L => "0L"
      case F => "0.0f"
      case D => "0.0d"
      case _ => "0"
    }

    def representation = repr.map(", a " + _).getOrElse("")

    def indent(s: String)  = if (s == "") "" else "  " + s
    def indentN(s: String) = s.lines map indent mkString "\n"

    def boxUnboxImpls = Map(
      "@boxImpl@"   -> "%s.valueOf(x)".format(boxedName),
      "@unboxImpl@" -> "x.asInstanceOf[%s].%sValue()".format(boxedName, lcname),
      "@unboxDoc@"  -> "the %s resulting from calling %sValue() on `x`".format(name, lcname)
    )
    def interpolations = Map(
      "@name@"      -> name,
      "@representation@" -> representation,
      "@javaequiv@" -> javaEquiv,
      "@boxed@"     -> boxedName,
      "@lcname@"    -> lcname,
      "@zero@"      -> zeroRep
    ) ++ boxUnboxImpls

    def interpolate(s: String): String = interpolations.foldLeft(s) {
      case (str, (key, value)) => str.replaceAll(key, value)
    }
    def classDoc  = interpolate(classDocTemplate)
    def objectDoc = ""
    def mkImports = ""
    
    def mkClass       = assemble("final abstract class " + name + " private extends AnyVal", classLines)
    def mkObject      = assemble("object " + name + " extends AnyValCompanion", objectLines)
    def make()    = List[String](
      headerTemplate,
      mkImports,
      classDoc,
      mkClass,
      objectDoc,
      mkObject
    ) mkString ""

    def assemble(decl: String, lines: List[String]): String = {
      val body = if (lines.isEmpty) " { }\n\n" else lines map indent mkString (" {\n", "\n", "\n}\n")

      decl + body + "\n"
    }
    override def toString = name
  }
}

trait AnyValTemplates {
  def headerTemplate = ("""
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

%s
package scala

import language.implicitConversions

""".trim.format(timestampString) + "\n\n")

  def classDocTemplate = ("""
/** `@name@`@representation@ (equivalent to Java's `@javaequiv@` primitive type) is a
 *  subtype of [[scala.AnyVal]]. Instances of `@name@` are not
 *  represented by an object in the underlying runtime system.
 *
 *  There is an implicit conversion from [[scala.@name@]] => [[scala.runtime.Rich@name@]]
 *  which provides useful non-primitive operations.
 */
""".trim + "\n")

  def timestampString = "// DO NOT EDIT, CHANGES WILL BE LOST.\n"

  def allCompanions = """
/** Transform a value type into a boxed reference type.
 *
 *  @param  x   the @name@ to be boxed
 *  @return     a @boxed@ offering `x` as its underlying value.
 */
def box(x: @name@): @boxed@ = @boxImpl@

/** Transform a boxed type into a value type.  Note that this
 *  method is not typesafe: it accepts any Object, but will throw
 *  an exception if the argument is not a @boxed@.
 *
 *  @param  x   the @boxed@ to be unboxed.
 *  @throws     ClassCastException  if the argument is not a @boxed@
 *  @return     @unboxDoc@
 */
def unbox(x: java.lang.Object): @name@ = @unboxImpl@

/** The String representation of the scala.@name@ companion object.
 */
override def toString = "object scala.@name@"
"""

  def nonUnitCompanions = ""  // todo

  def cardinalCompanion = """
/** The smallest value representable as a @name@.
 */
final val MinValue = @boxed@.MIN_VALUE

/** The largest value representable as a @name@.
 */
final val MaxValue = @boxed@.MAX_VALUE
"""

  def floatingCompanion = """
/** The smallest positive value greater than @zero@ which is
 *  representable as a @name@.
 */
final val MinPositiveValue = @boxed@.MIN_VALUE
final val NaN              = @boxed@.NaN
final val PositiveInfinity = @boxed@.POSITIVE_INFINITY
final val NegativeInfinity = @boxed@.NEGATIVE_INFINITY

@deprecated("use @name@.MinPositiveValue instead", "2.9.0")
final val Epsilon  = MinPositiveValue

/** The negative number with the greatest (finite) absolute value which is representable
 *  by a @name@.  Note that it differs from [[java.lang.@name@.MIN_VALUE]], which
 *  is the smallest positive value representable by a @name@.  In Scala that number
 *  is called @name@.MinPositiveValue.
 */
final val MinValue = -@boxed@.MAX_VALUE

/** The largest finite positive number representable as a @name@. */
final val MaxValue = @boxed@.MAX_VALUE
"""
}

class AnyVals extends AnyValReps with AnyValTemplates {
  object B extends AnyValNum("Byte",    Some("8-bit signed integer"),                  "byte")
  object S extends AnyValNum("Short",   Some("16-bit signed integer"),                 "short")
  object C extends AnyValNum("Char",    Some("16-bit unsigned integer"),               "char")
  object I extends AnyValNum("Int",     Some("32-bit signed integer"),                 "int")
  object L extends AnyValNum("Long",    Some("64-bit signed integer"),                 "long")
  object F extends AnyValNum("Float",   Some("32-bit IEEE-754 floating point number"), "float")
  object D extends AnyValNum("Double",  Some("64-bit IEEE-754 floating point number"), "double")
  object Z extends AnyValRep("Boolean", None,                                          "boolean") {
    def classLines = """
/**
 * Negates a Boolean expression.
 *
 * - `!a` results in `false` if and only if `a` evaluates to `true` and
 * - `!a` results in `true` if and only if `a` evaluates to `false`.
 *
 * @return the negated expression
 */
def unary_! : Boolean

/**
  * Compares two Boolean expressions and returns `true` if they evaluate to the same value.
  *
  * `a == b` returns `true` if and only if
  *  - `a` and `b` are `true` or
  *  - `a` and `b` are `false`.
  */
def ==(x: Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
  *
  * `a != b` returns `true` if and only if
  *  - `a` is `true` and `b` is `false` or
  *  - `a` is `false` and `b` is `true`.
  */
def !=(x: Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
  *
  * `a || b` returns `true` if and only if
  *  - `a` is `true` or
  *  - `b` is `true` or
  *  - `a` and `b` are `true`.
  *
  * @note This method uses 'short-circuit' evaluation and
  *       behaves as if it was declared as `def ||(x: => Boolean): Boolean`.
  *       If `a` evaluates to `true`, `true` is returned without evaluating `b`.
  */
def ||(x: Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
  *
  * `a && b` returns `true` if and only if
  *  - `a` and `b` are `true`.
  *
  * @note This method uses 'short-circuit' evaluation and
  *       behaves as if it was declared as `def &&(x: => Boolean): Boolean`.
  *       If `a` evaluates to `false`, `false` is returned without evaluating `b`.
  */
def &&(x: Boolean): Boolean

// Compiler won't build with these seemingly more accurate signatures
// def ||(x: => Boolean): Boolean
// def &&(x: => Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
  *
  * `a | b` returns `true` if and only if
  *  - `a` is `true` or
  *  - `b` is `true` or
  *  - `a` and `b` are `true`.
  *
  * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
  */
def |(x: Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
  *
  * `a & b` returns `true` if and only if
  *  - `a` and `b` are `true`.
  *
  * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
  */
def &(x: Boolean): Boolean

/**
  * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
  *
  * `a ^ b` returns `true` if and only if
  *  - `a` is `true` and `b` is `false` or
  *  - `a` is `false` and `b` is `true`.
  */
def ^(x: Boolean): Boolean

override def getClass(): Class[Boolean] = null
    """.trim.lines.toList

    def objectLines = interpolate(allCompanions + "\n" + nonUnitCompanions).lines.toList
  }
  object U extends AnyValRep("Unit", None, "void") {
    override def classDoc = """
/** `Unit` is a subtype of [[scala.AnyVal]]. There is only one value of type
 *  `Unit`, `()`, and it is not represented by any object in the underlying
 *  runtime system. A method with return type `Unit` is analogous to a Java
 *  method which is declared `void`.
 */
"""
    def classLines  = List(
      """override def getClass(): Class[Unit] = null"""
    )
    def objectLines = interpolate(allCompanions).lines.toList

    override def boxUnboxImpls = Map(
      "@boxImpl@"   -> "scala.runtime.BoxedUnit.UNIT",
      "@unboxImpl@" -> "()",
      "@unboxDoc@"  -> "the Unit value ()"
    )
  }

  def isSubrangeType = Set(B, S, C)
  def isIntegerType  = Set(B, S, C, I, L)
  def isFloatingType = Set(F, D)
  def isWideType     = Set(L, D)

  def cardinal = numeric filter isIntegerType
  def numeric  = List(B, S, C, I, L, F, D)
  def values   = List(U, Z) ++ numeric

  def make() = values map (x => (x.name, x.make()))
}

object AnyVals extends AnyVals { }
