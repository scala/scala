package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val set = TreeSet(1, 2, 3)

    assertEquals(TreeSet.empty[Int], set take Int.MinValue)
    assertEquals(TreeSet.empty[Int], set takeRight Int.MinValue)
    assertEquals(set, set drop Int.MinValue)
    assertEquals(set, set dropRight Int.MinValue)
  }

  @Test
  def intersects(): Unit = {
    val setA = TreeSet("a", "b", "c")
    val setB = TreeSet("d", "e")
    val setC = TreeSet("f", "g", "a", "i")
    assertFalse("setA does not intersects with setB", setA.intersects(setB))
    assertTrue("setA intersects with setC", setA.intersects(setC))
    assertFalse("setC does not intersects with setB", setC.intersects(setB))
  }
}
