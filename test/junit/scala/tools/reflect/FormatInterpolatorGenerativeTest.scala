package scala.tools.reflect

import java.util.IllegalFormatException

import org.junit.Assert.fail
import org.junit.Assume.assumeTrue
import org.junit.Test

import scala.tools.reflect.FormatInterpolatorTesting.run
import scala.util.Properties.isJavaAtLeast

/** Systematically built f-interpolations, compared with what `java.util.Formatter` does with each conversion.
 *
 *  An interpolation compiles if and only if formatting does not throw, and then gives the formatter's result.
 *  The formatter of JDK 17 and later is the reference: earlier ones accept some specifiers that the
 *  interpolator rejects, such as an index of 0.
 */
class FormatInterpolatorGenerativeTest {
  import FormatInterpolatorGenerativeTest._

  /** A value of an arg, with the name it has in `FormatInterpolatorTesting.Decls`-like declarations. */
  final case class Arg(name: String, tpe: String, decl: String, value: Any)

  val args = List(
    Arg("int", "Int", "42", 42),
    Arg("neg", "Int", "-42", -42),
    Arg("long", "Long", "7L", 7L),
    Arg("short", "Short", "3.toShort", 3.toShort),
    Arg("dbl", "Double", "3.25", 3.25),
    Arg("flt", "Float", "1.5f", 1.5f),
    Arg("bi", "BigInt", "BigInt(-12)", BigInt(-12)),
    Arg("bd", "BigDecimal", "BigDecimal(\"1.5\")", BigDecimal("1.5")),
    Arg("jbi", "java.math.BigInteger", "java.math.BigInteger.TEN", java.math.BigInteger.TEN),
    Arg("str", "String", "\"Scala\"", "Scala"),
    Arg("chr", "Char", "'c'", 'c'),
    Arg("bool", "Boolean", "true", true),
    Arg("date", "java.util.Date", "new java.util.Date(0L)", new java.util.Date(0L)),
    Arg("cal", "java.util.Calendar", "{ val c = java.util.Calendar.getInstance; c.setTimeInMillis(0L); c }",
      { val c = java.util.Calendar.getInstance; c.setTimeInMillis(0L); c }),
    Arg("inst", "java.time.Instant", "java.time.Instant.EPOCH", java.time.Instant.EPOCH),
    Arg("ld", "java.time.LocalDate", "java.time.LocalDate.of(2012, 5, 26)", java.time.LocalDate.of(2012, 5, 26)),
    Arg("lt", "java.time.LocalTime", "java.time.LocalTime.of(13, 14, 15)", java.time.LocalTime.of(13, 14, 15)),
    Arg("zdt", "java.time.ZonedDateTime", "java.time.ZonedDateTime.of(2012, 5, 26, 13, 14, 15, 0, java.time.ZoneId.of(\"UTC\"))",
      java.time.ZonedDateTime.of(2012, 5, 26, 13, 14, 15, 0, java.time.ZoneId.of("UTC"))),
  )
  val decls = args.map(a => s"val ${a.name}: ${a.tpe} = ${a.decl}").mkString("\n")
  def value(name: String) = args.find(_.name == name).get.value

  val conversions = List("s", "S", "b", "B", "h", "c", "d", "o", "x", "X", "e", "E", "f", "g", "a", "A", "tY", "TB", "tH", "tZ", "ts", "tc", "tq", "q")
  val flagSets: List[String] = "" :: "-#+ 0,(".map(_.toString).toList ::: List("-0", "+ ", "+(", "-#", "0,", "#(")
  val sizes = List("", "5", ".2", "7.3")

  /** The formatter's result for one specifier and value, as the interpolation formats it. */
  def formatted(spec: String, values: Any*): Either[String, String] =
    try Right(spec.format(values: _*))
    catch { case e: IllegalFormatException => Left(e.getClass.getSimpleName) }

  /** The value that the conversion after a splice formats: Scala's numeric widening applies first. */
  def converted(cv: String, v: Any): Any = {
    def wide: Option[Long] = v match {
      case b: Byte => Some(b.toLong) case s: Short => Some(s.toLong) case c: Char => Some(c.toLong)
      case i: Int => Some(i.toLong) case _ => None
    }
    cv.head match {
      case 'd' | 'o' | 'x' | 'X' => v match { case c: Char => c.toInt case b: java.math.BigInteger => BigInt(b) case _ => v }
      case 'e' | 'E' | 'f' | 'g' | 'G' | 'a' | 'A' => v match {
        case _: Double | _: Float | _: BigDecimal => v
        case l: Long => l.toDouble
        case _ => wide.map(_.toDouble).getOrElse(v)
      }
      case 't' | 'T' => v match { case _: Long | _: java.util.Calendar | _: java.util.Date => v case _ => wide.getOrElse(v) }
      case _ => v
    }
  }

  def compare(cases: Seq[(String, Either[String, String])]): Unit = {
    assumeTrue("the reference is the formatter of JDK 17 and later", isJavaAtLeast("17"))
    val outcomes = run(cases.map(_._1), decls = decls)
    val problems = for (((expr, expected), o) <- cases.zip(outcomes); p <- problem(expected, o.errors.map(_.text), o.result)) yield s"$expr: $p"
    if (problems.nonEmpty) fail(problems.take(50).mkString(s"${problems.length} of ${cases.length} cases:\n", "\n", ""))
  }

  /** The conversion after a splice: `f"$x%<spec>"`. */
  @Test def ownConversion(): Unit = {
    val cases = for {
      a <- args; cv <- conversions; fl <- flagSets; sz <- sizes
      spec = s"%$fl$sz$cv"
    } yield (s"""f"$$${a.name}$spec"""", formatted(spec, converted(cv, a.value)))
    compare(cases)
  }

  /** A reference to an arg after its own conversion `%s`: `f"$x%s %<spec>"` and `f"$x%s %1$$<spec>"`. */
  @Test def references(): Unit = {
    val cases = for {
      a <- args; cv <- conversions; fl <- flagSets.take(9); sz <- sizes.take(2); ref <- List("<", "1$$")
      spec = s"%$ref$fl$sz$cv"
    } yield (s"""f"$$${a.name}%s $spec"""", formatted(s"%s ${spec.replace("$$", "$")}", a.value))
    compare(cases)
  }

  /** Arg selection across several args, for each kind of specifier in each position. */
  @Test def selection(): Unit = {
    val specs = List("%s", "%d", "%1$$s", "%2$$s", "%3$$s", "%<s", "%1$$<s", "%n", "%%", "%5$$%", "%2$$n", "")
    val cases = for {
      s1 <- specs; s2 <- specs; s3 <- List("", " %<s", " %1$$s", " %s")
      expr = s"""f"$$int$s1 $$str$s2$s3""""
    } yield (expr, intended(List(("int", s1), ("str", s2)), s3))
    compare(cases)
  }
}

object FormatInterpolatorGenerativeTest {
  /** Whether an expected outcome and an actual one agree, or what is wrong. */
  def problem(expected: Either[String, String], errors: List[String], result: Option[Either[Throwable, String]]): Option[String] =
    (expected, result) match {
      case (Right(e), Some(Right(r))) if e == r => None
      case (Right(e), Some(r))                  => Some(s"expected \"$e\", got $r")
      case (Right(e), None)                     => Some(s"expected \"$e\", but it does not compile: ${errors.mkString("; ")}")
      case (Left(_), None)                      => None
      case (Left("IllegalFormatCodePointException"), Some(Left(_))) => None
      case (Left(e), Some(r))                   => Some(s"the formatter throws $e, but it compiles, and gives $r")
    }

  /** What `f"$a<spec a>$b<spec b>..."` means, computed with the formatter: each arg is formatted by the
   *  specifier after it, or by `%s` if there is none or it does not apply to the arg, and references
   *  select args as the formatter does.
   */
  def intended(parts: List[(String, String)], tail: String): Either[String, String] = {
    val gen = new FormatInterpolatorGenerativeTest
    val values = parts.map { case (name, _) => gen.value(name) }
    val own = """%(\d+\$)?([-#+ 0,(]*)(\d+)?(\.\d+)?([a-zA-Z]|[tT][a-zA-Z])(.*)""".r
    var last = 0
    val out = new StringBuilder
    def reference(spec: String): Either[String, Unit] = {
      val sp = spec.replace("$$", "$")
      val idx = """%(\d+)\$.*""".r
      val arg =
        if (sp.contains("<")) last
        else sp match { case idx(i) => i.toInt case _ => 0 }
      if (sp == "%%" || sp == "%n" || sp.matches("""%\d+\$[%n]""")) gen.formatted(sp).map(out ++= _)
      else if (arg < 1 || arg > values.length) Left("MissingFormatArgumentException")
      else { last = arg; gen.formatted(sp.replaceFirst("""^%\d+\$""", "%").replace("<", ""), values(arg - 1)).map(out ++= _) }
    }
    val steps = parts.zipWithIndex.iterator.map { case ((_, spec), i) =>
      val n = i + 1
      val sp = spec.replace("$$", "$")
      out ++= (if (i == 0) "" else " ")
      last = n
      sp match {
        case own(index, _, _, _, cv, _) if cv != "n" && !sp.contains("<") && (index == null || index == s"$n$$") =>
          gen.formatted(sp.replaceFirst("""^%\d+\$""", "%"), values(i)).map(out ++= _)
        case _ =>
          out ++= String.valueOf(values(i))
          if (sp.isEmpty) Right(()) else reference(spec)
      }
    }.toList
    val tailStep = if (tail.isEmpty) Right(()) else { out ++= " "; reference(tail.trim) }
    (steps :+ tailStep).collectFirst { case Left(e) => Left(e) }.getOrElse(Right(out.result()))
  }
}
