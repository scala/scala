package strawman.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import strawman.collection.mutable


@RunWith(classOf[JUnit4])
class TreeSetTest {

  @Test
  def rangeMkString(): Unit = {

    val set = mutable.TreeSet("a", "b", "c", "d")
    assertEquals("b", set.range("b", "c").mkString(","))
  }
}
