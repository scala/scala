package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.mutable

@RunWith(classOf[JUnit4])
class TreeMapTest {

  @Test
  def rangeMkString() {

    val map = mutable.TreeMap[String, String]()

    List("a", "b", "c", "d").foreach(v => map.put(v, v))

    val range = map.range("b", "c")

    val valuesRange = range.values
    val keysRange = range.keys

    assertEquals(1, valuesRange.size)
    assertEquals(1, keysRange.size)

    assertEquals("b", valuesRange.mkString(","))
    assertEquals("b", keysRange.mkString(","))
    assertEquals("b -> b", range.mkString(","))

  }

}
