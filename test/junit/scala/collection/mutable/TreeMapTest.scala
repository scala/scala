package scala.collection.mutable

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

import scala.collection.mutable

class TreeMapTest {

  @Test
  def rangeMkString(): Unit = {

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
