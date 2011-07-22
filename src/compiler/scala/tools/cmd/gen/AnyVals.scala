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

  sealed abstract class AnyValNum(name: String) extends AnyValRep(name) {
    def isCardinal: Boolean = isIntegerType(this)
    def unaryOps            = if (isCardinal) List("+", "-", "~") else List("+", "-")
    def bitwiseOps          = if (isCardinal) List("|", "&", "^") else Nil
    def shiftOps            = if (isCardinal) List("<<", ">>>", ">>") else Nil
    def comparisonOps       = List("==", "!=", "<", "<=", ">", ">=")
    def otherOps            = List("+", "-" ,"*", "/", "%")

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
    def mkUnaryOps  = unaryOps map (x => "def unary_%s : %s".format(x, this opType I))
    def mkStringOps = List("def +(x: String): String")
    def mkShiftOps  = (
      for (op <- shiftOps ; arg <- List(I, L)) yield
        "def %s(x: %s): %s".format(op, arg, this opType I)
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
          case s    => interpolate(s) + " = " + stub
        }
        res ++ xs
    }
    def objectLines = {
      val comp = if (isCardinal) cardinalCompanion else floatingCompanion
      (comp + allCompanions).trim.lines map interpolate toList
    }

    /** Makes a set of binary operations based on the given set of ops, args, and resultFn.
     *
     *  @param    ops       list of function names e.g. List(">>", "%")
     *  @param    args      list of types which should appear as arguments
     *  @param    resultFn  function which calculates return type based on arg type
     *  @return             list of function definitions
     */
    def mkBinOpsGroup(ops: List[String], args: List[AnyValNum], resultFn: AnyValNum => AnyValRep): List[String] = (
      ops flatMap (op =>
        args.map(arg => "def %s(x: %s): %s".format(op, arg, resultFn(arg))) :+ ""
      )
    ).toList
  }

  sealed abstract class AnyValRep(val name: String) {
    def classLines: List[String]
    def objectLines: List[String]
    def commonClassLines = List(
      "def getClass(): Class[@name@]"
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

    def indent(s: String)  = if (s == "") "" else "  " + s
    def indentN(s: String) = s.lines map indent mkString "\n"

    def boxUnboxImpls = Map(
      "@boxImpl@"   -> "%s.valueOf(x)".format(boxedName),
      "@unboxImpl@" -> "x.asInstanceOf[%s].%sValue()".format(boxedName, lcname),
      "@unboxDoc@"  -> "the %s resulting from calling %sValue() on `x`".format(name, lcname)
    )
    def interpolations = Map(
      "@name@"      -> name,
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
    def mkClass   = assemble("final class", "AnyVal", classLines) + "\n"
    def mkObject  = assemble("object", "AnyValCompanion", objectLines) + "\n"
    def make()    = List[String](
      headerTemplate,
      mkImports,
      classDoc,
      mkClass,
      objectDoc,
      mkObject
    ) mkString ""

    def assemble(what: String, parent: String, lines: List[String]): String = {
      val decl = "%s %s extends %s ".format(what, name, parent)
      val body = if (lines.isEmpty) "{ }\n\n" else lines map indent mkString ("{\n", "\n", "\n}\n")

      decl + body
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

""".trim.format(timestampString) + "\n\n")

  def classDocTemplate = ("""
/** `@name@` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
 *
 *  There is an implicit conversion from [[scala.@name@]] => [[scala.runtime.Rich@name@]]
 *  which provides useful non-primitive operations.
 */
""".trim + "\n")

  def timestampString = "// DO NOT EDIT, CHANGES WILL BE LOST.\n"
  def stub            = """sys.error("stub")"""

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
  object B extends AnyValNum("Byte")
  object S extends AnyValNum("Short")
  object C extends AnyValNum("Char")
  object I extends AnyValNum("Int")
  object L extends AnyValNum("Long")
  object F extends AnyValNum("Float")
  object D extends AnyValNum("Double")
  object Z extends AnyValRep("Boolean") {
    def classLines = """
def unary_! : Boolean = sys.error("stub")

def ==(x: Boolean): Boolean = sys.error("stub")
def !=(x: Boolean): Boolean = sys.error("stub")
def ||(x: Boolean): Boolean = sys.error("stub")
def &&(x: Boolean): Boolean = sys.error("stub")
// Compiler won't build with these seemingly more accurate signatures
// def ||(x: => Boolean): Boolean = sys.error("stub")
// def &&(x: => Boolean): Boolean = sys.error("stub")
def |(x: Boolean): Boolean  = sys.error("stub")
def &(x: Boolean): Boolean  = sys.error("stub")
def ^(x: Boolean): Boolean  = sys.error("stub")

def getClass(): Class[Boolean] = sys.error("stub")
    """.trim.lines.toList

    def objectLines = interpolate(allCompanions).lines.toList
  }
  object U extends AnyValRep("Unit") {
    override def classDoc = """
/** Unit is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.  There is
 *  only one value of type Unit: `()`.
 */
"""
    def classLines  = List(
      """def getClass(): Class[Unit] = sys.error("stub")"""
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

