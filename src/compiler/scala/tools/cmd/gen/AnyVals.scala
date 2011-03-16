/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.cmd
package gen

trait AnyValTemplates {
  def timestampString = ""
  def template = ("""
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

%s
package scala

import java.{ lang => jl }
  """.trim.format(timestampString) + "\n\n"
  )

  def booleanBody = """
final class Boolean extends AnyVal {
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
}

object Boolean extends AnyValCompanion {
  override def toString = "object scala.Boolean"
  def box(x: Boolean): jl.Boolean = jl.Boolean.valueOf(x)
  def unbox(x: jl.Object): Boolean = x.asInstanceOf[jl.Boolean].booleanValue()
}
  """.trim

  def unitBody = """
import runtime.BoxedUnit

final class Unit extends AnyVal { }

object Unit extends AnyValCompanion {
  override def toString = "object scala.Unit"
  def box(x: Unit): BoxedUnit = BoxedUnit.UNIT
  def unbox(x: jl.Object): Unit = ()
}
  """.trim

  def cardinalCompanion = """
final val MinValue = @type@.MIN_VALUE
final val MaxValue = @type@.MAX_VALUE

def box(x: @name@): @type@ = @type@.valueOf(x)
def unbox(x: jl.Object): @name@ = x.asInstanceOf[@type@].@lcname@Value()
override def toString = "object scala.@name@"
  """.trim.lines

  def floatingCompanion = """
/** The smallest positive value greater than @zero@.*/
final val MinPositiveValue = @type@.MIN_VALUE
final val NaN              = @type@.NaN
final val PositiveInfinity = @type@.POSITIVE_INFINITY
final val NegativeInfinity = @type@.NEGATIVE_INFINITY

@deprecated("use @name@.MinPositiveValue instead")
final val Epsilon  = MinPositiveValue

/** The negative number with the greatest (finite) absolute value which is representable
 *  by a @name@.  Note that it differs from [[java.lang.@name@.MIN_VALUE]], which
 *  is the smallest positive value representable by a @name@.  In Scala that number
 *  is called @name@.MinPositiveValue.
 */
final val MinValue = -@type@.MAX_VALUE

/** The largest finite positive number representable as a @name@. */
final val MaxValue = @type@.MAX_VALUE

def box(x: @name@): @type@ = @type@.valueOf(x)
def unbox(x: jl.Object): @name@ = x.asInstanceOf[@type@].@lcname@Value()
override def toString = "object scala.@name@"
  """.trim.lines
}

class AnyVals extends AnyValTemplates {
  val B = "Byte"
  val S = "Short"
  val C = "Char"
  val I = "Int"
  val L = "Long"
  val F = "Float"
  val D = "Double"

  lazy val cardinal = List(B, S, C, I, L)
  lazy val floating = List(F, D)
  lazy val numeric  = cardinal ++ floating

  def javaType(primType: String) = "jl." + (primType match {
    case C => "Character"
    case I => "Integer"
    case t => t
  })

  def make() =
    (numeric zip (numeric map (name => new AnyValOps(name).make()))) ++ List(
      ("Boolean", template + booleanBody),
      ("Unit", template + unitBody)
    )

  class AnyValOps(name: String) {
    val isCardinal = cardinal contains name
    val restype    = if ("LFD" contains name.head) name else I
    val tpe        = javaType(name)
    val zero       = name.head match {
      case 'L' => "0L"
      case 'F' => "0.0f"
      case 'D' => "0.0d"
      case _   => "0"
    }
    val interpolations = Map(
      "@restype@"  -> restype,
      "@name@"     -> name,
      "@type@"     -> tpe,
      "@lcname@"   -> name.toLowerCase,
      "@zero@"     -> zero
    )

    def mkCoercions = numeric map (x => "def to%s: %s".format(x, x))
    def mkUnaryOps  = unaryops map (op => "def unary_%s : @restype@".format(op))
    def mkCommon    = List(
      "def +(x: String): String"
    )
    def mkShiftOps = (
      for (op <- shiftops ; tpe <- List(I, L)) yield
        "def %s(x: %s): @restype@".format(op, tpe)
    )

    def clumps: List[List[String]] = {
      val xs1 = List(mkCoercions, mkUnaryOps, mkCommon, mkShiftOps) map (xs => if (xs.isEmpty) xs else xs :+ "")
      val xs2 = List(
        mkBinOpsGroup(boolBinops, numeric, _ => "Boolean"),
        mkBinOpsGroup(bitwiseops, cardinal, resultTypeForArg),
        mkBinOpsGroup(otherBinops, numeric, resultTypeForArg)
      )
      xs1 ++ xs2
    }

    def defImplementation = "sys.error(\"stub\")"
    def indent(s: String) = if (s == "") "" else "  " + s
    def mkClass = {
      val lines = clumps.foldLeft(List[String]()) {
        case (res, Nil)   => res
        case (res, lines) =>
          val xs = lines map {
            case ""   => ""
            case s    => interpolate(s) + " = " + defImplementation
          }
          res ++ xs
      }
      assemble("final class", "AnyVal", lines)
    }
    def mkObject = assemble("object", "AnyValCompanion", companionBody map interpolate toList)

    def assemble(what: String, parent: String, lines: List[String]): String = (
      List(what, name, "extends", parent, "{").mkString(" ") +:
      (lines map indent) :+
      "}"
    ).mkString("\n", "\n", "\n")

    def make() = template + mkClass + "\n" + mkObject

    def interpolate(s: String): String = interpolations.foldLeft(s) {
      case (str, (key, value)) => str.replaceAll(key, value)
    }

    /** Makes a set of binary operations based on the given set of ops, args, and resultFn.
     *
     *  @param    ops       list of function names e.g. List(">>", "%")
     *  @param    args      list of types which should appear as arguments
     *  @param    resultFn  function which calculates return type based on arg type
     *  @return             list of function definitions
     */
    def mkBinOpsGroup(ops: List[String], args: List[String], resultFn: String => String): List[String] = (
      ops flatMap { op =>
        args.map(arg => "def %s(x: %s): ".format(op, arg) + resultFn(arg)) :+ ""
      }
    )

    def resultTypeForArg(arg: String): String = arg match {
      case L if isCardinal  => L
      case F                => if (name == D) D else F
      case D                => D
      case _                => restype
    }

    def unaryops    = if (isCardinal) List("+", "-", "~") else List("+", "-")
    def bitwiseops  = if (isCardinal) List("|", "&", "^") else Nil
    def shiftops    = if (isCardinal) List("<<", ">>>", ">>") else Nil
    def boolBinops  = List("==", "!=", "<", "<=", ">", ">=")
    def otherBinops = List("+", "-" ,"*", "/", "%")

    def companionBody =
      if (isCardinal) cardinalCompanion
      else floatingCompanion
  }
}

object AnyVals extends AnyVals { }