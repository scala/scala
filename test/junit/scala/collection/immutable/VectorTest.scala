package scala.collection.immutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class VectorTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val v = Vector(0) ++ Vector(1 to 64: _*)

    assertEquals(Vector(0, 1), v take 2)
    assertEquals(Vector(63, 64), v takeRight 2)
    assertEquals(Vector(2 to 64: _*), v drop 2)
    assertEquals(Vector(0 to 62: _*), v dropRight 2)

    assertEquals(v, v take Int.MaxValue)
    assertEquals(v, v takeRight Int.MaxValue)
    assertEquals(Vector.empty[Int], v drop Int.MaxValue)
    assertEquals(Vector.empty[Int], v dropRight Int.MaxValue)

    assertEquals(Vector.empty[Int], v take Int.MinValue)
    assertEquals(Vector.empty[Int], v takeRight Int.MinValue)
    assertEquals(v, v drop Int.MinValue)
    assertEquals(v, v dropRight Int.MinValue)
  }

  @Test
  def hasCorrectPrependedAll(): Unit = {
    val els = Vector(1 to 1000: _*)

    for (i <- 0 until els.size) {
      val (prefix, suffix) = els.splitAt(i)

      assertEquals(els, prefix ++: suffix)
      assertEquals(els, prefix.toList ++: suffix)
    }
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(Vector.empty, Vector.empty)
    assertSame(Vector.empty, Vector())
    val m = Vector("a")
    assertSame(m, Vector.from(m))
    assertSame(m, Vector.apply(m: _*))
  }

  @Test def checkSearch: Unit = SeqTests.checkSearch(Vector(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

  @Test
  def emptyIteratorReuse(): Unit = {
    assertSame(Vector.empty.iterator, Vector.empty.iterator)
    assertSame(Vector.empty.iterator, Vector(1).drop(1).iterator)
  }

  @Test
  def t11122_prependedAll_Iterator(): Unit = {
    val i = Iterator.from(1).take(3)
    assertEquals(Vector(1, 2, 3, 0), Vector(0).prependedAll(i))
  }
}
