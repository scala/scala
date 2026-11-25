package scala.tools.reflect

import org.junit.Assert.fail
import org.junit.Test

import scala.reflect.ClassTag
import scala.tools.reflect.FormatInterpolatorTesting.{Outcome, run}

/** The f-interpolator's checks, translation and results. A case gives the errors and warnings that
 *  compiling the expression must report, as (column in the expression, message), and what running it
 *  must give if it compiles. The expressions can refer to the values in `FormatInterpolatorTesting.Decls`.
 */
class FormatInterpolatorTest {
  import FormatInterpolatorTest._

  @Test def arguments(): Unit = check(
    err("new StringContext().f()", 0 -> "there are no parts"),
    err("new StringContext(\"\", \" is \", \"%2d years old\").f(s)", 49 -> "too few arguments for interpolated string"),
    err("new StringContext(\"\", \" is \", \"%2d years old\").f(s, d, d)", 55 -> "too many arguments for interpolated string"),
    err("new StringContext(\"\", \"\").f()", 27 -> "too few arguments for interpolated string"),
  )

  @Test def typeMismatches(): Unit = check(
    compiles("f\"$s%b\"").warns(5 -> "Boolean format is null test for non-Boolean"),
    err("f\"$s%c\"", 3 -> "type mismatch;\n found   : String\n required: Char, Byte, Short, Int"),
    err("f\"$f%c\"", 3 -> "type mismatch;\n found   : Double\n required: Char, Byte, Short, Int"),
    err("f\"$s%x\"", 3 -> "type mismatch;\n found   : String\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"$b%d\"", 3 -> "type mismatch;\n found   : Boolean\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"$s%d\"", 3 -> "type mismatch;\n found   : String\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"$f%o\"", 3 -> "type mismatch;\n found   : Double\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"$s%e\"", 3 -> "type mismatch;\n found   : String\n required: Double, Float, BigDecimal"),
    err("f\"$b%f\"", 3 -> "type mismatch;\n found   : Boolean\n required: Double, Float, BigDecimal"),
    err("{ implicit val strToInt1 = (s: String) => 1; implicit val strToInt2 = (s: String) => 2; f\"$s%d\" }", 91 -> "type mismatch;\n found   : String\n required: Int\nNote that implicit conversions are not applicable because they are ambiguous:\n both value strToInt2 of type String => Int\n and value strToInt1 of type String => Int\n are possible conversion functions from String to Int"),
    err("f\"$s%i\"", 5 -> "illegal conversion character 'i'"),
  )

  @Test def flagMismatches(): Unit = check(
    err("f\"$s%+ 0,(s\"", 5 -> "Illegal flag '+'", 6 -> "Illegal flag ' '", 7 -> "Illegal flag '0'", 8 -> "Illegal flag ','", 9 -> "Illegal flag '('"),
    err("f\"$c%#+ 0,(c\"", 5 -> "Only '-' allowed for c conversion"),
    err("f\"$d%#d\"", 5 -> "# not allowed for d conversion"),
    err("f\"$d%,x\"", 5 -> "',' only allowed for d conversion of integral types"),
    err("f\"$d%+ (x\"", 5 -> "only use '+' for BigInt conversions to o, x, X", 6 -> "only use ' ' for BigInt conversions to o, x, X", 7 -> "only use '(' for BigInt conversions to o, x, X"),
    err("f\"$f%,(a\"", 5 -> "',' not allowed for a, A", 6 -> "'(' not allowed for a, A"),
    err("f\"$t%#+ 0,(tT\"", 5 -> "Only '-' allowed for date/time conversions"),
    err("f\"$d%,,d\"", 6 -> "Duplicate flag ','"),
  )

  @Test def badPrecisions(): Unit = check(
    err("f\"$c%.2c\"", 5 -> "precision not allowed"),
    err("f\"$d%.2d\"", 5 -> "precision not allowed"),
    err("f\"%.2%\"", 3 -> "precision not allowed"),
    err("f\"%.2n\"", 3 -> "precision not allowed"),
    err("f\"$f%.2a\"", 5 -> "precision not allowed"),
    err("f\"$t%.2tT\"", 5 -> "precision not allowed"),
  )

  @Test def badIndexes(): Unit = check(
    err("f\"%<s\"", 3 -> "No last arg"),
    err("f\"%<c\"", 3 -> "No last arg"),
    err("f\"%<tT\"", 3 -> "No last arg"),
    err("f\"${8}%d ${9}%d%3$$d\"", 16 -> "Argument index out of range"),
    err("f\"${8}%d ${9}%d%0$$d\"", 16 -> "Argument index out of range"),
  )

  @Test def indexWarnings(): Unit = check(
    compiles("f\"${8}%d ${9}%1$$d\"").warns(14 -> "Index is not this arg"),
    compiles("f\"$s%s $s%s %1$$<s\"").warns(13 -> "Argument index ignored if '<' flag is present"),
    compiles("f\"$s%s $s%1$$s\"").warns(10 -> "Index is not this arg"),
  )

  @Test def badArgTypes(): Unit = check(
    err("f\"$s%#s\"", 3 -> "type mismatch;\n found   : String\n required: java.util.Formattable"),
    compiles("f\"$f%f %<d\""),
    compiles("f\"%1$$d $f%f\""),
    compiles("f\"${null}%s %<#s\""),
  )

  @Test def misunderstoodConversions(): Unit = check(
    err("f\"$t%tG\"", 6 -> "'G' doesn't seem to be a date or time conversion"),
    err("f\"$t%t\"", 5 -> "Date/time conversion must have two characters"),
    err("f\"$s%10.5\"", 4 -> "Missing conversion operator in '%10.5'; use %% for literal %, %n for newline"),
  )

  @Test def junkBeforeConversion(): Unit = check(
    err("f\"${d}random-leading-junk%d\"", 25 -> "conversions must follow a splice; use %% for literal %, %n for newline"),
  )

}

object FormatInterpolatorTest {
  type Located = (Int, String)

  final case class Case(expr: String, result: Option[Either[Class[_], String]], errors: List[Located], warnings: List[Located]) {
    def warns(ws: Located*): Case = copy(warnings = ws.toList)

    def mismatches(o: Outcome): List[String] = {
      def located(ms: List[FormatInterpolatorTesting.Message]) = ms.map(m => (m.column, m.text))
      val errs  = if (located(o.errors) == errors) Nil else List(s"errors: expected $errors, got ${located(o.errors)}")
      val warns = if (located(o.warnings) == warnings) Nil else List(s"warnings: expected $warnings, got ${located(o.warnings)}")
      val res = (result, o.result) match {
        case (None, _)                                                    => Nil
        case (Some(Right(r)), Some(Right(got))) if r == got               => Nil
        case (Some(Left(t)), Some(Left(got))) if t.isInstance(got)        => Nil
        case (Some(expected), got)                                        => List(s"result: expected $expected, got $got")
      }
      errs ++ warns ++ res
    }
  }

  /** It compiles with no errors and no warnings unless given by `warns`; its result is not checked. */
  def compiles(expr: String): Case = Case(expr, None, Nil, Nil)
  /** It compiles, and evaluates to `result`. */
  def ok(expr: String, result: String): Case = Case(expr, Some(Right(result)), Nil, Nil)
  /** It compiles, and throws a `T` when run. */
  def throws[T <: Throwable](expr: String)(implicit t: ClassTag[T]): Case = Case(expr, Some(Left(t.runtimeClass)), Nil, Nil)
  /** It reports exactly these errors. */
  def err(expr: String, errors: Located*): Case = Case(expr, None, errors.toList, Nil)

  def check(cases: Case*): Unit = {
    val failures = for {
      (c, o) <- cases.zip(run(cases.map(_.expr)))
      problems = c.mismatches(o)
      if problems.nonEmpty
    } yield problems.mkString(s"${c.expr}\n  ", "\n  ", "")
    if (failures.nonEmpty) fail(failures.mkString(s"${failures.length} of ${cases.length} cases failed:\n", "\n", ""))
  }
}
