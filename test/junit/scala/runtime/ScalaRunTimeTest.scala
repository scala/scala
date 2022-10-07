import org.junit.Assert._
import org.junit.Test

import scala.runtime.ScalaRunTime.{replStringOf, stringOf}

package scala.runtime {

  /** Tests for the runtime object ScalaRunTime */
  class ScalaRunTimeTest {
    import test.stuff._

    @Test def testStringOf(): Unit = {

      def verboseStringOf(s: String): String = stringOf(s, maxLength = 0, verboseProduct = true)

      assertEquals("null", stringOf(null))
      assertEquals("\"\"", stringOf(""))

      assertEquals("X", stringOf(Strung(1)))
      assertEquals("X"*2, stringOf(Strung(2)))
      assertEquals("X"*3, stringOf(Strung(3)))
      assertEquals("X"*4, stringOf(Strung(4)))
      assertEquals("XX...", stringOf(Strung(10), 5))
      assertEquals("...", stringOf(Strung(10), 2))
      assertEquals(s"Li...", stringOf(List(Strung(10), Strung(20)), 5))

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
      assertEquals("""Map(1 -> "", 2 -> a, 3 -> " a", 4 -> null)""", stringOf(map))
      assertEquals("""Map(1 -> "", 2 -> "a", 3 -> " a", 4 -> null)""", stringOf(map, maxLength = 0, verboseProduct = true))
      assertEquals("""Map(1 -> "", ...)""", stringOf(map, 20))

      val iterable = Iterable("a", "", " c", null)
      assertEquals("""List(a, "", " c", null)""", stringOf(iterable))
      assertEquals("""List("a", "", " c", null)""", stringOf(iterable, maxLength = 0, verboseProduct = true))
      assertEquals("""List(a, ...)""", stringOf(iterable, 15))
      assertEquals("""List("a", "", ...)""", stringOf(iterable, maxLength = 20, verboseProduct = true))

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
      assertEquals("...", stringOf(x, maxLength = 2))

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
      // escape the quote then wrap in triple quotes and prepend with interpolator
      assertEquals(
        "s" + tq(escq(abcdef)),
        verboseStringOf(q(abcdef))
      )
      // escape ALL the quotes
      assertEquals(
        "s" + tq(escq(escq(escq(escq(abcdef))))),
        verboseStringOf(tq(q(abcdef)))
      )
      // ordinary single quote and no interpolator needed for \t
      assertEquals(q("a\\tb"), verboseStringOf("a\tb"))
    }
    @Test def `stringOf handles recursive-traversable`: Unit = {
      val vs =
        new Iterable[_] {
          def iterator = Iterator.continually(this)
          override def toString = "ha, you caught me"
        }
      assertEquals("ha, you caught me", stringOf(vs))
    }
    // not traversble again means always use toString
    @Test def `stringOf avoids once-traversable`: Unit = {
      val vs =
        new Iterable[Int] {
          var i = 0
          def iterator = Iterator.continually { i+=1; i }
          override def isTraversableAgain = false
        }
      assertEquals("<iterable>", stringOf(vs))
    }
    @Test def `stringOf avoids mystery iterable`: Unit = {
      assertEquals("mystery", stringOf(Mystery))
    }
    @Test def `stringOf handles infinite iterable`: Unit = {
      assertEquals("Custom(42, 42, 42, 42, 42, 42, 42, 42, 42, 42)", stringOf(Custom, maxLength = Int.MaxValue, verboseProduct = false, maxElements = 10))
    }
    @Test def `stringOf draws a line somewhere`: Unit = {
      assertEquals("Iterable(42, 42, 42, 42, 42, 42, 42, 42, 42, 42)", stringOf(Danger, maxLength = Int.MaxValue, verboseProduct = false, maxElements = 10))
    }
  }
}

package test.stuff {
  import scala.collection.AbstractIterable

  class Strung(n: Int) {
    override def toString = "X" * n
  }
  object Strung {
    def apply(n: Int) = new Strung(n)
  }
  // Is not AbstractIterable, so don't expect a legit toString like it's a thing.
  object Custom extends Iterable[Int] {
    def iterator = Iterator.continually(42)
    override def className = "Custom" // signal that I expect mkString, but REPL can pretty-print elements
  }
  object Mystery extends AbstractIterable[Int] {
    def iterator = Iterator.continually(42)
    override def toString = "mystery" // doesn't supply className so intends to use toString, not mkString
  }
  object Danger extends Iterable[Int] {
    def iterator = Iterator.continually(42)
    override def toString = "toxic"  // too dangerous
  }
}
