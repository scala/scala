package scala.tools.reflect

import org.junit.Assert.{assertEquals, fail}
import org.junit.Test

import scala.reflect.ClassTag
import scala.util.chaining._
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
    err("f\"$s%+ 0,(s\"", 5 -> "Illegal flag '+'"),
    err("f\"$c%#+ 0,(c\"", 5 -> "Only '-' allowed for c conversion"),
    err("f\"$d%#d\"", 5 -> "# not allowed for d conversion"),
    err("f\"$d%,x\"", 5 -> "',' only allowed for d conversion of integral types"),
    err("f\"$d%+ (x\"", 6 -> "Illegal combination of flags '+' and ' '"),
    err("f\"$d%+(x\"", 5 -> "only use '+' for BigInt conversions to o, x, X"),
    ok("f\"${BigInt(-1)}%+(x\"", "(1)"),
    err("f\"$f%,(a\"", 5 -> "',' not allowed for a, A"),
    err("f\"$t%#+ 0,(tT\"", 5 -> "Only '-' allowed for date/time conversions"),
    err("f\"$d%,,d\"", 6 -> "Duplicate flag ','"),
  )

  @Test def badPrecisions(): Unit = check(
    err("f\"$c%.2c\"", 5 -> "precision not allowed"),
    err("f\"$d%.2d\"", 5 -> "precision not allowed"),
    err("f\"%.2%\"", 3 -> "precision not allowed"),
    err("f\"%.2n\"", 3 -> "precision not allowed"),
    err("f\"$f%.2147483648a\"", 6 -> "Precision out of range"),
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
    err("f\"$f%f %<d\"", 9 -> "type mismatch;\n found   : Double\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"%1$$d $f%f\"", 6 -> "type mismatch;\n found   : Double\n required: Int, Long, Byte, Short, BigInt"),
    errAt("f\"${null}%s %<#s\"", "s\"" -> "type mismatch;\n found   : Null(null)\n required: java.util.Formattable"),
  )

  @Test def misunderstoodConversions(): Unit = check(
    err("f\"$t%tG\"", 6 -> "'G' doesn't seem to be a date or time conversion"),
    err("f\"$t%t\"", 5 -> "Date/time conversion must have two characters"),
    err("f\"$s%10.5\"", 4 -> "Missing conversion operator in '%10.5'; use %% for literal %, %n for newline"),
  )

  @Test def junkBeforeConversion(): Unit = check(
    err("f\"${d}random-leading-junk%d\"", 25 -> "conversions must follow a splice; use %% for literal %, %n for newline"),
  )


  // from pr11411
  @Test def malformedSpecifiers(): Unit = check(
    err("f\"$d%1$$\"", 4 -> "Missing conversion operator in '%1$'; use %% for literal %, %n for newline"),
    err("f\"$d%2147483648d\"", 5 -> "Width out of range"),
    err("f\"$d%99999999999999999999d\"", 5 -> "Width out of range"),
    err("f\"$f%.2147483648f\"", 6 -> "Precision out of range"),
    err("f\"$d%2147483648$$d\"", 5 -> "Argument index out of range"),
    err("f\"$d\\u\"", 5 -> "invalid unicode escape at index 1 of \\u"),
    err("f\"$d\\u00\"", 8 -> "invalid unicode escape at index 4 of \\u00"),
    err("f\"a\\u00zz\"", 7 -> "invalid unicode escape at index 5 of a\\u00zz"),
    err("f\"$d%d%2147483648$$n\"", 7 -> "Argument index out of range"),
    err("f\"$d%d%2147483648$$<d\"", 7 -> "Argument index out of range"),
    err("f\"$d%d%0$$n\"", 7 -> "Argument index out of range"),
    err("f\"$d%d%0$$<d\"", 7 -> "Argument index out of range"),
    err("f\"%0$$%\"", 3 -> "Argument index out of range"),
    err("f\"$s%s %0$$<s\"", 8 -> "Argument index out of range"),
    err("f\"$d%.2147483648d\"", 5 -> "precision not allowed"),
  )

  @Test def rejectedFlags(): Unit = check(
    err("f\"$d%-d\"", 5 -> "Flag '-' requires a width"),
    err("f\"$d%0d\"", 5 -> "Flag '0' requires a width"),
    err("f\"$d%-0 5d\"", 6 -> "Illegal combination of flags '-' and '0'"),
    err("f\"$d%+ d\"", 6 -> "Illegal combination of flags '+' and ' '"),
    err("f\"$f%-015f\"", 6 -> "Illegal combination of flags '-' and '0'"),
    err("f\"$f%0f\"", 5 -> "Flag '0' requires a width"),
    err("f\"$s%-s\"", 5 -> "Flag '-' requires a width"),
    err("f\"$c%-c\"", 5 -> "Flag '-' requires a width"),
    err("f\"$t%-tT\"", 5 -> "Flag '-' requires a width"),
    err("f\"$d%d%1$$-d\"", 10 -> "Flag '-' requires a width"),
    err("f\"$d%d%-%\"", 7 -> "Flag '-' requires a width"),
    err("f\"$f%,e\"", 5 -> "',' not allowed for e, E"),
    err("f\"$f%,E\"", 5 -> "',' not allowed for e, E"),
    err("f\"$f%#g\"", 5 -> "'#' not allowed for g, G"),
    err("f\"$f%#G\"", 5 -> "'#' not allowed for g, G"),
  )

  @Test def rejectedArg(): Unit = check(
    err("f\"${BigDecimal(1)}%a\"", 14 -> "type mismatch;\n found   : scala.math.BigDecimal\n required: Double, Float"),
  )

  @Test def referencesAreTypeChecked(): Unit = check(
    err("f\"${\"a\"}%<d\"", 10 -> "type mismatch;\n found   : String(\"a\")\n required: Int, Long, Byte, Short, BigInt").warns(9 -> "Index is not this arg"),
    err("f\"${\"a\"}%s %1$$d\"", 15 -> "type mismatch;\n found   : String(\"a\")\n required: Int, Long, Byte, Short, BigInt"),
    err("f\"$d%<+x\"", 6 -> "only use '+' for BigInt conversions to o, x, X").warns(5 -> "Index is not this arg"),
    err("f\"${BigDecimal(1)}%<a\"", 20 -> "type mismatch;\n found   : scala.math.BigDecimal\n required: Double, Float").warns(19 -> "Index is not this arg"),
    err("f\"${BigDecimal(1)}%e %1$$A\"", 25 -> "type mismatch;\n found   : scala.math.BigDecimal\n required: Double, Float"),
  )

  @Test def positionsAfterEscapes(): Unit = check(
    err("f\"$d%d\\t\\u0041$\"%-%\"", 17 -> "Flag '-' requires a width"),
  )

  @Test def moreChecks(): Unit = check(
    err("f\"$s%.2147483648s\"", 6 -> "Precision out of range"),
    err("f\"$s%-b\"", 5 -> "Flag '-' requires a width"),
    err("f\"$s%.2147483648b\"", 6 -> "Precision out of range"),
    err("f\"$s%-h\"", 5 -> "Flag '-' requires a width"),
    err("f\"$s%.2147483648h\"", 6 -> "Precision out of range"),
    err("f\"$f%.2147483648e\"", 6 -> "Precision out of range"),
    err("f\"$f%.2147483648g\"", 6 -> "Precision out of range"),
    err("f\"${BigDecimal(1)}%A\"", 14 -> "type mismatch;\n found   : scala.math.BigDecimal\n required: Double, Float"),
    err("f\"$f%,(.2147483648a\"", 8 -> "Precision out of range"),
    err("f\"$d%1$$q\"", 8 -> "illegal conversion character 'q'"),
    err("f\"$d%99999999999\"", 4 -> "Missing conversion operator in '%99999999999'; use %% for literal %, %n for newline"),
    err("f\"$d$d%2$$\"", 6 -> "Missing conversion operator in '%2$'; use %% for literal %, %n for newline"),
    err("f\"${d}x%1$$\"", 7 -> "Missing conversion operator in '%1$'; use %% for literal %, %n for newline"),
    err("f\"\"\"a\n  \\u00zz\"\"\"", 12 -> "invalid unicode escape at index 8 of a\n  \\u00zz"),
  )

  // the macro is expanded once when its result is an argument, which is typed silently first
  @Test def silentlyTyped(): Unit = check(
    errAt("\"\".concat(f\"$d%-d\")", "-d" -> "Flag '-' requires a width"),
    ok("\"\".concat(f\"$d%5d\")", "    8"),
  )

  // %% and %n take no arg, so they accept any index; the formatter pads %% to a width
  @Test def literals(): Unit = check(
    ok("f\"%5$$%\"", "%"),
    compiles("f\"%5%\""),
    compiles("f\"%2$$n\""),
  )

  @Test def nullArg(): Unit = check(
    errAt("f\"${null}%a\"", "null" -> "an expression of type Null is ineligible for implicit conversion"),
    errAt("f\"${null}%#s\"", "null" -> "type mismatch;\n found   : Null(null)\n required: java.util.Formattable"),
  )

  // a Formattable formats itself, also if nothing else is formatted
  @Test def formattable(): Unit = check(
    ok("f\"$x\"", "xxx"),
    ok("f\"$x%s\"", "xxx"),
    ok("f\"$x $i%d\"", "xxx 42"),
    // also if the type does not tell
    ok("{ val q: AnyRef = x; f\"$q\" }", "xxx"),
    ok("{ val q: Any = x; f\"$q and $q%s\" }", "xxx and xxx"),
    ok("{ def g[A](a: A) = f\"<$a>\"; g(x) }", "<xxx>"),
    ok("{ class B { override def toString = \"b\" }; class F extends B with java.util.Formattable { def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff.format(\"f\") }; val v: B = new F; f\"$v ${new B}\" }", "f b"),
    ok("f\"$anyVal $any $s ${null}\"", "7 7 Scala null"),
    // a sealed type that cannot hold one is not tested, which would warn
    ok("{ val o: Option[Int] = Some(1); val e: Either[Int, String] = Left(2); val l = List(3); f\"$o $e $l\" }", "Some(1) Left(2) List(3)"),
    ok("{ sealed trait T; case object O extends T; final case class C(i: Int) extends T; val t: T = O; val c: T = C(1); f\"$t $c\" }", "O C(1)"),
    ok("{ sealed trait T; class B extends T { override def toString = \"b\" }; class F extends B with java.util.Formattable { def formatTo(ff: java.util.Formatter, g: Int, w: Int, p: Int): Unit = ff.format(\"f\") }; val t: T = new F; f\"$t\" }", "f"),
  )

  @Test def nonLiteralParts(): Unit = check(
    errAt("{ val p = \"a\"; StringContext(p, \"\").f(1) }", "p, " -> "parts of an f-interpolation must be string literals"),
  )

  // the first type that an arg conforms to, or converts to, decides how it is formatted
  @Test def implicitConversions(): Unit = check(
    ok("{ import scala.language.implicitConversions; implicit def toI(s: String): Int = 1; implicit def toL(s: String): Long = 2L; f\"${\"x\"}%d\" }", "1"),
    ok("{ import scala.language.implicitConversions; implicit def toD(i: java.time.Instant): java.util.Date = java.util.Date.from(i); f\"${java.time.Instant.EPOCH}%tY\" }", "1970"),
    ok("{ val ser: java.io.Serializable = date; f\"$ser%s %<tY\" }", "2012-05-26 2012"),
  )

  // the result must not depend on the compiler's default locale
  @Test def localeOfCompiler(): Unit = {
    val outcomes = FormatInterpolatorTesting.run(Seq("f\"${\"i\"}%S\"", "f\"${1234}%,d\""), compileLocale = Some(new java.util.Locale("tr", "TR")))
    assertEquals(Some(Right("i".toUpperCase(java.util.Locale.getDefault))), outcomes(0).result)
    assertEquals(Some(Right(String.format("%,d", Int.box(1234)))), outcomes(1).result)
  }

  // A conversion that refers to an arg by index or '<' is checked against the arg's type. It is not
  // converted again, and a type that may hold a value that the formatter takes is accepted.
  @Test def references(): Unit = check(
    ok("f\"${5}%s%<d\"", "55"),
    ok("f\"${null}%d %<B\"", "null FALSE"),
    ok("f\"${7}%d %2$$d ${9}%d\"", "7 9 9"),
    ok("f\"${Integer.valueOf(7)}%s %1$$x\"", "7 7"),
    ok("f\"${java.math.BigInteger.ONE}%s %<+d\"", "1 +1"),
    ok("f\"$any%s %<d\"", "7 7"),
    ok("f\"${BigInt(7): Any}%s %<+x\"", "7 +7"),
    ok("f\"$any%s %1$$x\"", "7 7"),
    ok("f\"$anyVal%s %<d\"", "7 7"),
    ok("f\"$number%s %<d\"", "7 7"),
    ok("{ def generic[A](a: A) = f\"$a%s %<d\"; generic(7) }", "7 7"),
    ok("{ def bounded[A <: Int](a: A) = f\"$a%s %<d\"; bounded(7) }", "7 7"),
    ok("f\"$date%s %<tY\"", "2012-05-26 2012"),
    ok("f\"$date%s %1$$tF\"", "2012-05-26 2012-05-26"),
    ok("f\"$temporal%s %<tY\"", "2012-05-26 2012"),
    // the type does not tell which flags are allowed
    throws[java.util.FormatFlagsConversionMismatchException]("f\"$any%s %<+x\""),
  )

  // '<' refers to the previous arg, and ignores an index; a leading one is not the conversion of its own arg
  @Test def previousArg(): Unit = check(
    ok("f\"${\"a\"}%1$$<s\"", "aa").warnsAt("1$$<" -> "Argument index ignored if '<' flag is present"),
    ok("f\"${7}%1$$<d\"", "77").warnsAt("1$$<" -> "Argument index ignored if '<' flag is present"),
    ok("f\"${i}%d %<d ${9}%d\"", "42 42 9"),
    ok("f\"$s%s$i%<s\"", "Scala4242").warnsAt("<s" -> "Index is not this arg"),
  )

  // the formatter takes the arg of a conversion without an index from a counter of its own
  @Test def ownIndex(): Unit = check(
    ok("f\"${i}%1$$d${9}%d\"", "429"),
    ok("f\"${i}%1$$s${9}\"", "429"),
    ok("f\"${i}%1$$d$s%2$$s ${9}%d\"", "42Scala 9"),
  )

  // an index of another arg, or a precision, must reach the formatter also if nothing else is formatted
  @Test def concatenated(): Unit = {
    val ab = "{ val a = \"x\"; val b = \"y\"; "
    def in(e: String) = s"$ab$e }"
    check(
      ok(in("f\"$a%2$$s $b\""), "xy y").warnsAt("2$$" -> "Index is not this arg"),
      ok(in("f\"$a $b%1$$s\""), "x yx").warnsAt("1$$" -> "Index is not this arg"),
      ok(in("f\"$a $b %1$$s\""), "x y x"),
      ok(in("f\"$a x%1$$s\""), "x xx"),
      ok(in("f\"$a%1$$s $b\""), "x y"),
      ok(in("f\"$a%1$$s $b%s\""), "x y"),
      ok(in("f\"$a%1$$3s $b\""), "  x y"),
      ok(in("f\"$a%1$$3s $b%s\""), "  x y"),
      ok(in("f\"$a%1$$s$b%2$$s $b%<s\""), "xy yy").warnsAt("<s" -> "Index is not this arg"),
      ok(in("f\"$a%2$$s $b%5s\""), "xy     y").warnsAt("2$$" -> "Index is not this arg"),
      ok(in("f\"$a%.0s$b\""), "y"),
      ok(in("f\"$a$b%.0s\""), "x"),
      ok(in("f\"$a%1$$.0s$b\""), "y"),
      ok(in("f\"$a%.2s\""), "x"),
      ok("f\"$s%.2s\"", "Sc"),
    )
  }

  @Test def hexFloatPrecision(): Unit = check(
    ok("f\"${1.5}%.2a\"", "0x1.80p0"),
    ok("f\"${1.5f}%.1A\"", "0X1.8P0"),
  )

  // the conversion after a splice is checked strictly: the arg's type must fit, or convert to one that does
  @Test def ownConversionIsStrict(): Unit = check(
    errAt("f\"$any%d\"", "any" -> "type mismatch;\n found   : Any\n required: Int, Long, Byte, Short, BigInt"),
    errAt("f\"$number%d\"", "number" -> "type mismatch;\n found   : Number\n required: Int, Long, Byte, Short, BigInt"),
    errAt("f\"${null}%c\"", "null" -> "an expression of type Null is ineligible for implicit conversion"),
    ok("f\"${null}%d\"", "null"),
  )

  // java.time's dates are taken by %t, if the class has the field
  @Test def ownTemporal(): Unit = check(
    ok("f\"$date%tY\"", "2012"),
    ok("f\"$temporal%tF\"", "2012-05-26"),
    ok("f\"${java.time.Instant.EPOCH}%tQ\"", "0"),
    errAt("f\"${java.time.Instant.EPOCH}%tY\"", "Y" -> "'Y' is not a field of java.time.Instant"),
    ok("f\"${java.time.LocalTime.of(13, 14)}%tR\"", "13:14"),
    errAt("f\"${java.time.LocalTime.of(13, 14)}%tF\"", "F" -> "'F' is not a field of java.time.LocalTime"),
    errAt("f\"$date%s %<tH\"", "H" -> "'H' is not a field of java.time.LocalDate"),
    errAt("{ val cs: CharSequence = \"x\"; f\"$cs%s %<tY\" }", "tY" -> "type mismatch;\n found   : CharSequence\n required: Long, java.util.Calendar, java.util.Date, java.time.temporal.TemporalAccessor"),
  )

  @Test def withoutPositions(): Unit = {
    import scala.reflect.runtime.{currentMirror, universe}, universe._
    import scala.tools.testkit.AssertUtil.assertThrows
    val tb = currentMirror.mkToolBox()
    assertEquals("a 1", tb.eval(q"""val x = 1; f"a $$x%d" """))
    assertEquals("a\t 1", tb.eval(q"""val x = 1; StringContext("a\t", "%2d").f(x)"""))
    assertThrows[ToolBoxError](tb.eval(q"""val x = 1; f"a $$x%-d" """), _.contains("Flag '-' requires a width"))
  }
}

object FormatInterpolatorTest {
  type Located = (Int, String)

  final case class Case(expr: String, result: Option[Either[Class[_], String]], errors: List[Located], warnings: List[Located], runs: Boolean = false) {
    def warns(ws: Located*): Case = copy(warnings = ws.toList)
    /** Warnings at the first occurrence of an anchor in the expression. */
    def warnsAt(ws: (String, String)*): Case = warns(ws.map { case (anchor, msg) => at(expr, anchor) -> msg }: _*)

    def mismatches(o: Outcome): List[String] = {
      def located(ms: List[FormatInterpolatorTesting.Message]) = ms.map(m => (m.column, m.text))
      val errs  = if (located(o.errors) == errors) Nil else List(s"errors: expected $errors, got ${located(o.errors)}")
      val warns = if (located(o.warnings) == warnings) Nil else List(s"warnings: expected $warnings, got ${located(o.warnings)}")
      val res = (result, o.result) match {
        case (None, Some(Left(t))) if runs                                => List(s"result: expected none to be thrown, got $t")
        case (None, _)                                                    => Nil
        case (Some(Right(r)), Some(Right(got))) if r == got               => Nil
        case (Some(Left(t)), Some(Left(got))) if t.isInstance(got)        => Nil
        case (Some(expected), got)                                        => List(s"result: expected $expected, got $got")
      }
      errs ++ warns ++ res
    }
  }

  /** It compiles with no errors and no warnings unless given by `warns`, and runs without throwing. */
  def compiles(expr: String): Case = Case(expr, None, Nil, Nil, runs = true)
  /** It compiles, and evaluates to `result`. */
  def ok(expr: String, result: String): Case = Case(expr, Some(Right(result)), Nil, Nil)
  /** It compiles, and throws a `T` when run. */
  def throws[T <: Throwable](expr: String)(implicit t: ClassTag[T]): Case = Case(expr, Some(Left(t.runtimeClass)), Nil, Nil)
  /** It reports exactly these errors. */
  def err(expr: String, errors: Located*): Case = Case(expr, None, errors.toList, Nil)

  /** It reports exactly these errors, each at the first occurrence of an anchor in the expression. */
  def errAt(expr: String, errors: (String, String)*): Case = err(expr, errors.map { case (anchor, msg) => at(expr, anchor) -> msg }: _*)

  /** The column of the `n`th occurrence of `anchor` in `expr`. */
  def at(expr: String, anchor: String, n: Int = 0): Int =
    (0 until n).foldLeft(expr.indexOf(anchor))((i, _) => expr.indexOf(anchor, i + 1)).tap(i => assert(i >= 0, s"no $anchor in $expr"))

  def check(cases: Case*): Unit = {
    val failures = for {
      (c, o) <- cases.zip(run(cases.map(_.expr)))
      problems = c.mismatches(o)
      if problems.nonEmpty
    } yield problems.mkString(s"${c.expr}\n  ", "\n  ", "")
    if (failures.nonEmpty) fail(failures.mkString(s"${failures.length} of ${cases.length} cases failed:\n", "\n", ""))
  }
}
