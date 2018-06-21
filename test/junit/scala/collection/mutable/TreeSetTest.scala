package scala.collection.mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable


@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def rangeMkString(): Unit = {

    val set = mutable.TreeSet("a", "b", "c", "d")
    assertEquals("b", set.range("b", "c").mkString(","))
  }

  @Test
  def intersects(): Unit = {
    val setA = mutable.TreeSet("a", "b", "c")
    val setB = mutable.TreeSet("d", "e")
    val setC = mutable.TreeSet("f", "g", "a", "i")
    assertFalse("setA does not intersects with setB", setA.intersects(setB))
    assertTrue("setA intersects with setC", setA.intersects(setC))
    assertFalse("setC does not intersects with setB", setC.intersects(setB))
  }
}
