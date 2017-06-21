package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable


@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def rangeMkString() {

    val set = mutable.TreeSet("a", "b", "c", "d")
    assertEquals("b", set.range("b", "c").mkString(","))
  }
}
