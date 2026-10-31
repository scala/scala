package scala.collection.mutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.{Stepper, immutable, mutable}

@RunWith(classOf[JUnit4])
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

  private def drain[A](s: Stepper[A]): List[A] = {
    val prefix = s.trySplit()
    (if (prefix == null) Nil else prefix.iterator.toList) ++ s.iterator.toList
  }

  // Projections used to inherit whole-tree overrides of foreachEntry, minAfter/maxBefore and the steppers
  @Test
  def rangeProjectionsRespectBounds(): Unit = {
    val map = mutable.TreeMap((1 to 9).map(i => i -> i.toString): _*)
    val projections = List(
      map.rangeFrom(4)              -> (4 to 9),
      map.rangeUntil(6)             -> (1 to 5),
      map.range(3, 7)               -> (3 to 6),
      map.range(3, 7).rangeFrom(5)  -> (5 to 6),
      map.range(20, 30)             -> (1 until 1),
    )
    for ((proj, keys) <- projections) {
      val expected = keys.map(i => i -> i.toString).toList
      val visited = ListBuffer.empty[(Int, String)]
      proj.foreachEntry((k, v) => visited += (k -> v))
      assertEquals(expected, visited.toList)

      for (k <- 0 to 10) {
        assertEquals(s"minAfter($k)", expected.find(_._1 >= k), proj.minAfter(k))
        assertEquals(s"maxBefore($k)", expected.findLast(_._1 < k), proj.maxBefore(k))
      }

      assertEquals(expected, proj.stepper.iterator.toList)
      assertEquals(keys.toList, proj.keyStepper.iterator.toList)
      assertEquals(expected.map(_._2), proj.valueStepper.iterator.toList)
      assertEquals(expected, drain(proj.stepper))
      assertEquals(keys.toList, drain(proj.keyStepper))

      assertEquals(expected, mutable.TreeMap.from(proj).toList)
      assertEquals(expected, proj.to(mutable.TreeMap).toList)
      assertEquals(expected, immutable.TreeMap.from(proj).toList)
      assertEquals(expected.toMap, immutable.HashMap.from(proj))
    }
  }
}
