package scala.lang.stringinterpol

import java.text.DecimalFormat

import org.junit.Assert.{ assertThrows => _, _ }
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.language.implicitConversions
import scala.tools.testkit.AssertUtil._

object StringContextTestUtils {
  private val decimalSeparator: Char = new DecimalFormat().getDecimalFormatSymbols().getDecimalSeparator()
  private val numberPattern = """(\d+)\.(\d+.*)""".r

  implicit class StringContextOps(val sc: StringContext) extends AnyVal {
    // Use this String interpolator to avoid problems with a locale-dependent decimal mark.
    def locally(numbers: String*): String = {
      val numbersWithCorrectLocale = numbers.map(applyProperLocale)
      sc.s(numbersWithCorrectLocale: _*)
    }

    // Handles cases like locally"3.14" - it's prettier than locally"${"3.14"}".
    def locally(): String = sc.parts.map(applyProperLocale).mkString

    private def applyProperLocale(number: String): String = {
      val numberPattern(intPart, fractionalPartAndSuffix) = number
      s"$intPart$decimalSeparator$fractionalPartAndSuffix"
    }
  }
}

@RunWith(classOf[JUnit4])
class StringContextTest {

  import StringContext._
  import StringContextTestUtils.StringContextOps

  @Test def noEscape() = {
    val s = "string"
    val res = processEscapes(s)
    assertEquals(s, res)
  }
  @Test def tabbed() = {
    val s = """a\tb"""
    val res = processEscapes(s)
    assertEquals("a\tb", res)
  }
  @Test def quoted() = {
    val s = """hello, \"world\""""
    val res = processEscapes(s)
    assertEquals("""hello, "world"""", res)
  }
  @Test def badly() = assertThrows[InvalidEscapeException] {
    val s = """Scala\"""
    val res = processEscapes(s)
    assertEquals("Scala", res)
  }
  @Test def noOctal() = assertThrows[InvalidEscapeException] {
    val s = """\123cala"""
    val res = processEscapes(s)
    assertEquals("Scala", res)
  }
  @Test def t5856(): Unit = {
    class X {
      override def toString = "Test"

      def thistle(): Unit = {
        assertEquals("Test", s"$this")
        assertEquals("TestTest", s"$this$this")
        assertEquals("Test$", s"$this$$")
        assertEquals("Test.##", s"$this.##")
        assertEquals("Test.toString", s"$this.toString")
        assertEquals("Test=THIS", s"$this=THIS")
      }
    }
    new X().thistle()   // this'll be good
  }

  @Test def t6631_baseline() = assertEquals("\f\r\n\t", s"""\f\r\n\t""")

  // verifying that the standard interpolators can be supplanted
  @Test def antiHijack_?() = {
    object AllYourStringsAreBelongToMe { case class StringContext(args: Any*) { def s(args: Any*) = "!!!!" } }
    import AllYourStringsAreBelongToMe._
    //assertEquals("????", s"????")
    assertEquals("!!!!", s"????") // OK to hijack core interpolator ids
  }

  @Test def fIf() = {
    val res = f"${if (true) 2.5 else 2.5}%.2f"
    val expected = locally"2.50"
    assertEquals(expected, res)
  }

  @Test def fIfNot() = {
    val res = f"${if (false) 2.5 else 3.5}%.2f"
    val expected = locally"3.50"
    assertEquals(expected, res)
  }

  @Test def fHeteroArgs() = {
    val res = f"${3.14}%.2f rounds to ${3}%d"
    val expected = locally"${"3.14"} rounds to 3"
    assertEquals(expected, res)
  }

  @annotation.nowarn("msg=Boolean format is null test for non-Boolean")
  @Test def `non booleans as boolean`(): Unit = {
    assertEquals("false", f"${null}%b")
    assertEquals("FALSE", f"${null}%B")
  }

  @Test def `f interpolator baseline`(): Unit = {

    implicit def stringToBoolean(s: String): Boolean = java.lang.Boolean.parseBoolean(s)
    implicit def stringToChar(s: String): Char = s(0)
    implicit def str2fmt(s: String): java.util.Formattable = new java.util.Formattable {
      def formatTo(f: java.util.Formatter, g: Int, w: Int, p: Int) = f.format("%s", s)
    }

    val b_true  = true
    val b_false = false

    val i = 42

    val f_zero = 0.0
    val f_zero_- = -0.0

    val s = "Scala"

    val fff  = new java.util.Formattable {
      def formatTo(f: java.util.Formatter, g: Int, w: Int, p: Int) = f.format("4")
    }
    import java.util.{Calendar, Locale}
    val c = Calendar.getInstance(Locale.US)
    c.set(2012, Calendar.MAY, 26)
    implicit def strToDate(x: String): Calendar = c

    val ss = List[(String, String)] (
      // 'b' / 'B' (category: general)
      // -----------------------------
      f"${b_false}%b" -> "false",
      f"${b_true}%b"  -> "true",

      f"${false}%b" -> "false",
      f"${true}%b"  -> "true",
      f"${true && false}%b"                 -> "false",
      f"${java.lang.Boolean.valueOf(false)}%b"  -> "false",
      f"${java.lang.Boolean.valueOf(true)}%b"   -> "true",

      f"${false}%B" -> "FALSE",
      f"${true}%B"  -> "TRUE",
      f"${java.lang.Boolean.valueOf(false)}%B"  -> "FALSE",
      f"${java.lang.Boolean.valueOf(true)}%B"   -> "TRUE",

      f"${"true"}%b" -> "true",
      f"${"false"}%b"-> "false",

      // 'h' | 'H' (category: general)
      // -----------------------------
      f"${null}%h"   -> "null",
      f"${f_zero}%h"   -> "0",
      f"${f_zero_-}%h" -> "80000000",
      f"${s}%h"       -> "4c01926",

      f"${null}%H"  -> "NULL",
      f"${s}%H"       -> "4C01926",

      // 's' | 'S' (category: general)
      // -----------------------------
      f"${null}%s"  -> "null",
      f"${null}%S"  -> "NULL",
      f"${s}%s"     -> "Scala",
      f"${s}%S"     -> "SCALA",
      f"${5}"       -> "5",
      f"${i}"       -> "42",
      f"${Symbol("foo")}"    -> "Symbol(foo)",

      f"${Thread.State.NEW}" -> "NEW",

      // 'c' | 'C' (category: character)
      // -------------------------------
      f"${120:Char}%c"   -> "x",
      f"${120:Byte}%c"   -> "x",
      f"${120:Short}%c"  -> "x",
      f"${120:Int}%c"    -> "x",
      f"${java.lang.Character.valueOf('x')}%c"   -> "x",
      f"${java.lang.Byte.valueOf(120:Byte)}%c"   -> "x",
      f"${java.lang.Short.valueOf(120:Short)}%c" -> "x",
      f"${java.lang.Integer.valueOf(120)}%c"     -> "x",

      f"${'x' : java.lang.Character}%c"     -> "x",
      f"${(120:Byte) : java.lang.Byte}%c"   -> "x",
      f"${(120:Short) : java.lang.Short}%c" -> "x",
      f"${120 : java.lang.Integer}%c"       -> "x",

      f"${"Scala"}%c"   -> "S",

      // 'd' | 'o' | 'x' | 'X' (category: integral)
      // ------------------------------------------
      f"${120:Byte}%d"    -> "120",
      f"${120:Short}%d"   -> "120",
      f"${120:Int}%d"     -> "120",
      f"${120:Long}%d"    -> "120",
      f"${60 * 2}%d"      -> "120",
      f"${java.lang.Byte.valueOf(120:Byte)}%d"   -> "120",
      f"${java.lang.Short.valueOf(120:Short)}%d" -> "120",
      f"${java.lang.Integer.valueOf(120)}%d"     -> "120",
      f"${java.lang.Long.valueOf(120)}%d"        -> "120",
      f"${120 : java.lang.Integer}%d"        -> "120",
      f"${120 : java.lang.Long}%d"           -> "120",
      f"${BigInt(120)}%d"                    -> "120",

      f"${new java.math.BigInteger("120")}%d" -> "120",

      f"${4}%#10X" -> "       0X4",

      f"She is ${fff}%#s feet tall." -> "She is 4 feet tall.",

      f"Just want to say ${"hello, world"}%#s..." -> "Just want to say hello, world...",

      { implicit val strToShort = (s: String) => java.lang.Short.parseShort(s) ; f"${"120"}%d" } -> "120",
      { implicit val strToInt = (s: String) => 42 ; f"${"120"}%d" } -> "42",

      // 'e' | 'E' | 'g' | 'G' | 'f' | 'a' | 'A' (category: floating point)
      // ------------------------------------------------------------------
      f"${3.4f}%e" -> locally"3.400000e+00",
      f"${3.4}%e"  -> locally"3.400000e+00",
      f"${3.4f : java.lang.Float}%e" -> locally"3.400000e+00",
      f"${3.4 : java.lang.Double}%e" -> locally"3.400000e+00",

      f"${BigDecimal(3.4)}%e" -> locally"3.400000e+00",

      f"${new java.math.BigDecimal(3.4)}%e" -> locally"3.400000e+00",

      f"${3}%e"  -> locally"3.000000e+00",
      f"${3L}%e" -> locally"3.000000e+00",

      // 't' | 'T' (category: date/time)
      // -------------------------------
      f"${c}%TD"                 -> "05/26/12",
      f"${c.getTime}%TD"         -> "05/26/12",
      f"${c.getTime.getTime}%TD" -> "05/26/12",
      f"""${"1234"}%TD"""        -> "05/26/12",

      // literals and arg indexes
      f"%%" -> "%",
      f" mind%n------%nmatter" ->
       """| mind
          |------
          |matter""".stripMargin.linesIterator.mkString(System.lineSeparator),
      f"${i}%d %<d ${9}%d"   -> "42 42 9",
      f"${7}%d %<d ${9}%d"   -> "7 7 9",
      f"${7}%d %2$$d ${9}%d" -> "7 9 9",

      f"${null}%d %<B" -> "null FALSE",

      f"${5: Any}"      -> "5",
      f"${5}%s%<d"      -> "55",
      f"${3.14}%s,%<f"  -> locally"3.14,${"3.140000"}",

      f"z" -> "z"
    )

    for ((f, s) <- ss) assertEquals(s, f)
  }

  @Test
  def globTests(): Unit = {
    val cases = Seq(
      // Empty literal
      StringContext.glob(Seq(""), "")    -> Some(Nil),
      StringContext.glob(Seq(""), "a")   -> None,
      StringContext.glob(Seq(""), "b")   -> None,
      StringContext.glob(Seq(""), "aa")  -> None,
      StringContext.glob(Seq(""), "ab")  -> None,
      StringContext.glob(Seq(""), "ba")  -> None,
  
      // Trivial literal
      StringContext.glob(Seq("a"), "a")  -> Some(Nil),
      StringContext.glob(Seq("a"), "b")  -> None,
      StringContext.glob(Seq("a"), "aa") -> None,
      StringContext.glob(Seq("a"), "ab") -> None,
      StringContext.glob(Seq("a"), "ba") -> None,
  
      // Non-trivial literal
      StringContext.glob(Seq("abc"), "abc")  -> Some(Nil),
      StringContext.glob(Seq("abc"), "babc") -> None,
      StringContext.glob(Seq("abc"), "aabc") -> None,
      StringContext.glob(Seq("abc"), "abcb") -> None,
      StringContext.glob(Seq("abc"), "cba")  -> None,
      StringContext.glob(Seq("abc"), "abbc") -> None,
  
      // Single wildcard
      StringContext.glob(Seq("a", "b"), "ab")      -> Some(Seq("")),
      StringContext.glob(Seq("a", "b"), "a b")     -> Some(Seq(" ")),
      StringContext.glob(Seq("a", "b"), "a    b")  -> Some(Seq("    ")),
      StringContext.glob(Seq("a", "b"), "a***b")   -> Some(Seq("***")),
      StringContext.glob(Seq("a", "b"), "a")       -> None,
      StringContext.glob(Seq("a", "b"), "b")       -> None,
      StringContext.glob(Seq("a", "b"), "abc")     -> None,
      StringContext.glob(Seq("a", "b"), "cab")     -> None,
      StringContext.glob(Seq("a", "b"), " ab ")    -> None,
  
      // Leading wildcard
      StringContext.glob(Seq("", "b"), "ab")     -> Some(Seq("a")),
      StringContext.glob(Seq("", "b"), "a b")    -> Some(Seq("a ")),
      StringContext.glob(Seq("", "b"), "a    b") -> Some(Seq("a    ")),
      StringContext.glob(Seq("", "b"), "a***b")  -> Some(Seq("a***")),
      StringContext.glob(Seq("", "b"), "a")      -> None,
      StringContext.glob(Seq("", "b"), "b")      -> Some(Seq("")),
      StringContext.glob(Seq("", "b"), "abc")    -> None,
      StringContext.glob(Seq("", "b"), "cab")    -> Some(Seq("ca")),
      StringContext.glob(Seq("", "b"), " ab ")   -> None,
  
      // Trailing wildcard
      StringContext.glob(Seq("a", ""), "ab")     -> Some(Seq("b")),
      StringContext.glob(Seq("a", ""), "a b")    -> Some(Seq(" b")),
      StringContext.glob(Seq("a", ""), "a    b") -> Some(Seq("    b")),
      StringContext.glob(Seq("a", ""), "a***b")  -> Some(Seq("***b")),
      StringContext.glob(Seq("a", ""), "a")      -> Some(Seq("")),
      StringContext.glob(Seq("a", ""), "b")      -> None,
      StringContext.glob(Seq("a", ""), "abc")    -> Some(Seq("bc")),
      StringContext.glob(Seq("a", ""), "cab")    -> None,
      StringContext.glob(Seq("a", ""), " ab ")   -> None,
  
      // Lonely wildcard
      StringContext.glob(Seq("", ""), "ab")      -> Some(Seq("ab")),
      StringContext.glob(Seq("", ""), "a b")     -> Some(Seq("a b")),
      StringContext.glob(Seq("", ""), "a    b")  -> Some(Seq("a    b")),
      StringContext.glob(Seq("", ""), "a***b")   -> Some(Seq("a***b")),
      StringContext.glob(Seq("", ""), "a")       -> Some(Seq("a")),
      StringContext.glob(Seq("", ""), "b")       -> Some(Seq("b")),
      StringContext.glob(Seq("", ""), "abc")     -> Some(Seq("abc")),
      StringContext.glob(Seq("", ""), "cab")     -> Some(Seq("cab")),
      StringContext.glob(Seq("", ""), " ab ")    -> Some(Seq(" ab ")),
  
      // Ambiguous lonely wildcard
      StringContext.glob(Seq("", "", ""), "ab")      -> Some(Seq("", "ab")),
      StringContext.glob(Seq("", "", ""), "a b")     -> Some(Seq("", "a b")),
      StringContext.glob(Seq("", "", ""), "a    b")  -> Some(Seq("", "a    b")),
      StringContext.glob(Seq("", "", ""), "a***b")   -> Some(Seq("", "a***b")),
      StringContext.glob(Seq("", "", ""), "a")       -> Some(Seq("", "a")),
      StringContext.glob(Seq("", "", ""), "b")       -> Some(Seq("", "b")),
      StringContext.glob(Seq("", "", ""), "abc")     -> Some(Seq("", "abc")),
      StringContext.glob(Seq("", "", ""), "cab")     -> Some(Seq("", "cab")),
      StringContext.glob(Seq("", "", ""), " ab ")    -> Some(Seq("", " ab ")),
  
      // Multiple wildcards
      StringContext.glob(Seq("a", "bb", "c"), "a__bb___c") -> Some(Seq("__", "___")),
      StringContext.glob(Seq("a", "bb", "c"), "abbc")      -> Some(Seq("", "")),
      StringContext.glob(Seq("a", "bb", "c"), "aabbbc")    -> Some(Seq("a", "b")),
      // Kind ambiguous
      StringContext.glob(Seq("a", "bb", "c"), "abbbc")     -> Some(Seq("", "b")),
      // Failures
      StringContext.glob(Seq("a", "bb", "c"), "a")         -> None,
      StringContext.glob(Seq("a", "bb", "c"), "abc")       -> None,
      StringContext.glob(Seq("a", "bb", "c"), "abb")       -> None,
      StringContext.glob(Seq("a", "bb", "c"), "bbc")       -> None,
      StringContext.glob(Seq("a", "bb", "c"), "bbaaac")    -> None,

      // Escapes!
      StringContext.glob(Seq("Hello,\t", ""), "Hello,\tJames") -> Some(Seq("James")),
  
      // Many many ambiguous wildcards
      StringContext.glob(Seq.fill(100)("a"), "a" * 200) -> Some(Seq.fill(98)("") :+ "a" * 100)
    )
    for((lhs, rhs) <- cases) assertEquals(rhs, lhs)
  }
}
