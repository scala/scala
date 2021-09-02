package scala.runtime

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Tests for the runtime object ScalaRunTime */
@RunWith(classOf[JUnit4])
class ScalaRunTimeTest {
  class Strung(n: Int) {
    override def toString = "X" * n
  }
  def Strung(n: Int) = new Strung(n)
  @Test
  def testStringOf(): Unit = {
    import ScalaRunTime.{replStringOf, stringOf}
    import scala.collection._

    def verboseStringOf(s: String): String = stringOf(s, maxLength = 0, verboseProduct = true)

    assertEquals("null", stringOf(null))
    assertEquals("\"\"", stringOf(""))

    assertEquals("X", stringOf(Strung(1)))
    assertEquals("X"*2, stringOf(Strung(2)))
    assertEquals("X"*3, stringOf(Strung(3)))
    assertEquals("X"*4, stringOf(Strung(4)))
    assertEquals("X"*10, stringOf(Strung(10), 2))
    assertEquals(s"List(${"X"*10}, ...)", stringOf(List(Strung(10), Strung(20)), 5))
    assertEquals(s"List(${"X"*10}, ...)", stringOf(List(Strung(10), Strung(20)), 3))

    assertEquals("abc", stringOf("abc"))
    assertEquals("\"abc\"", stringOf("abc", maxLength = 0, verboseProduct = true))
    assertEquals("\" abc\"", stringOf(" abc"))
    assertEquals("\"abc \"", stringOf("abc "))

    assertEquals("""Array()""", stringOf(Array.empty[AnyRef]))
    assertEquals("""Array()""", stringOf(Array.empty[Int]))
    assertEquals("""Array(1, 2, 3)""", stringOf(Array(1, 2, 3)))
    assertEquals("""Array(a, "", " c", null)""", stringOf(Array("a", "", " c", null)))
    assertEquals("""Array("a", "", " c", null)""", stringOf(Array("a", "", " c", null), maxLength = 0, verboseProduct = true))
    assertEquals("""Array(Array("", 1, Array(5)), Array(1))""",
        stringOf(Array(Array("", 1, Array(5)), Array(1))))

    val map = Map(1->"", 2->"a", 3->" a", 4->null)
    assertEquals(s"""Map(1 -> "", 2 -> a, 3 -> " a", 4 -> null)""", stringOf(map))
    assertEquals(s"""Map(1 -> "", 2 -> "a", 3 -> " a", 4 -> null)""", stringOf(map, maxLength = 0, verboseProduct = true))
    assertEquals(s"""Map(1 -> "", ...)""", stringOf(map, 2))

    val iterable = Iterable("a", "", " c", null)
    assertEquals(s"""List(a, "", " c", null)""", stringOf(iterable))
    assertEquals(s"""List("a", "", " c", null)""", stringOf(iterable, maxLength = 0, verboseProduct = true))
    assertEquals(s"""List(a, ...)""", stringOf(iterable, 2))
    assertEquals(s"""List("a", ...)""", stringOf(iterable, maxLength = 2, verboseProduct = true))

    val tuple1 = Tuple1(0)
    assertEquals("(0,)", stringOf(tuple1))
    assertEquals("(0,)", stringOf(tuple1, 0))
    assertEquals("(Array(0),)", stringOf(Tuple1(Array(0))))

    val tuple2 = Tuple2(0, 1)
    assertEquals("(0,1)", stringOf(tuple2))
    assertEquals("(0,1)", stringOf(tuple2, 0))
    assertEquals("(Array(0),1)", stringOf((Array(0), 1)))

    val tuple3 = Tuple3(0, 1, 2)
    assertEquals("(0,1,2)", stringOf(tuple3))
    assertEquals("(0,1,2)", stringOf(tuple3, 0))
    assertEquals("(Array(0),1,2)", stringOf((Array(0), 1, 2)))

    val x = new Object {
      override def toString(): String = "this is the stringOf string"
    }
    assertEquals("this is the stringOf string", stringOf(x))
    assertEquals("this is the stringOf string", stringOf(x, 2))

    val tpolecat = new Object {
      override def toString(): String = null
    }
    assertEquals(null, stringOf(tpolecat))
    assertEquals("null // non-null reference has null-valued toString", replStringOf(tpolecat, maxLength = 100, verboseProduct = false))

    val TQ = "\"" * 3
    val abcdef =
      s"""abc
         |def""".stripMargin
    def q(s: String) = "\"" + s + "\""
    def escq(s: String) = "\\\"" + s + "\\\""
    def tq(s: String) = TQ + s + TQ
    // escape the quote then wrap in triple quotes
    assertEquals(
      "s" + tq(escq(abcdef)),
      verboseStringOf(q(abcdef))
    )
    // escape ALL the quotes
    assertEquals(
      "s" + tq(escq(escq(escq(escq(abcdef))))),
      verboseStringOf(tq(q(abcdef)))
    )
  }
}
