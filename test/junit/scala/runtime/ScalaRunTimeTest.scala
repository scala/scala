package scala.runtime

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/** Tests for the private class DefaultPromise */
@RunWith(classOf[JUnit4])
class ScalaRunTimeTest {
  @Test
  def testIsTuple() {
    import ScalaRunTime.isTuple
    def check(v: Any) = {
      assertTrue(v.toString, isTuple(v))
    }

    val s = ""
    check(Tuple1(s))
    check((s, s))
    check((s, s, s))
    check((s, s, s, s))
    check((s, s, s, s, s))
    check((s, s, s, s, s, s))
    check((s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))
    check((s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s, s))

    // some specialized variants will have mangled classnames
    check(Tuple1(0))
    check((0, 0))
    check((0, 0, 0))
    check((0, 0, 0, 0))
    check((0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
    check((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

    case class C()
    val c = new C()
    assertFalse(c.toString, isTuple(c))
  }

  @Test
  def testStingOf() {
    import ScalaRunTime.stringOf
    import scala.collection._
    import parallel.ParIterable

    assertEquals("null", stringOf(null))
    assertEquals( "\"\"", stringOf(""))

    assertEquals("abc", stringOf("abc"))
    assertEquals("\" abc\"", stringOf(" abc"))
    assertEquals("\"abc \"", stringOf("abc "))

    assertEquals("""Array()""", stringOf(Array.empty[AnyRef]))
    assertEquals("""Array()""", stringOf(Array.empty[Int]))
    assertEquals("""Array(1, 2, 3)""", stringOf(Array(1, 2, 3)))
    assertEquals("""Array(a, "", " c", null)""", stringOf(Array("a", "", " c", null)))
    assertEquals("""Array(Array("", 1, Array(5)), Array(1))""",
        stringOf(Array(Array("", 1, Array(5)), Array(1))))

    val map = Map(1->"", 2->"a", 3->" a", 4->null)
    assertEquals(s"""${map.stringPrefix}(1 -> "", 2 -> a, 3 -> " a", 4 -> null)""", stringOf(map))
    assertEquals(s"""${map.stringPrefix}(1 -> "", 2 -> a)""", stringOf(map, 2))

    val iterable = Iterable("a", "", " c", null)
    assertEquals(s"""${iterable.stringPrefix}(a, "", " c", null)""", stringOf(iterable))
    assertEquals(s"""${iterable.stringPrefix}(a, "")""", stringOf(iterable, 2))

    val parIterable = ParIterable("a", "", " c", null)
    assertEquals(s"""${parIterable.stringPrefix}(a, "", " c", null)""", stringOf(parIterable))
    assertEquals(s"""${parIterable.stringPrefix}(a, "")""", stringOf(parIterable, 2))

    val traversable = new Traversable[Int] {
       def foreach[U](f: Int => U): Unit = (0 to 3).foreach(f)
    }
    assertEquals(s"${traversable.stringPrefix}(0, 1, 2, 3)", stringOf(traversable))
    assertEquals(s"${traversable.stringPrefix}(0, 1)", stringOf(traversable, 2))

    val tuple1 = Tuple1(0)
    assertEquals("(0,)", stringOf(tuple1))
    assertEquals("(0,)", stringOf(tuple1, 0))

    val tuple2 = Tuple2(0, 1)
    assertEquals("(0,1)", stringOf(tuple2))
    assertEquals("(0,1)", stringOf(tuple2, 0))

    val tuple3 = Tuple3(0, 1, 2)
    assertEquals("(0,1,2)", stringOf(tuple3))
    assertEquals("(0,1,2)", stringOf(tuple3, 0))

    val x = new Object {
        override def toString(): String = "this is the stringOf string"
    }
    assertEquals(stringOf(x), "this is the stringOf string")
    assertEquals(stringOf(x, 2), "this is the stringOf string")
  }
}
