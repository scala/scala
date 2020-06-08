package scala.collection.immutable

import org.junit._
import org.junit.Assert._

import scala.tools.testing.AllocationTest

class SmallMapTest extends AllocationTest {
  def iterator(m:Map[_,_]) = m.iterator
  def keysIterator(m:Map[_,_]) = m.keysIterator
  def valuesIterator(m:Map[_,_]) = m.valuesIterator

  //we use this side effect to avoid the git optimising away the tuples
  //but without affecting the allocations
  val nonAllocationResult = new Array[Any](5)
  def consume(it:Iterator[_]): Int = {
    var size = 0
    nonAllocationResult(0) = it
    while (it.hasNext) {
      size += 1
      nonAllocationResult(size) = it.next()
    }
    size
  }
  def consume1(it:Iterator[_]): Int = {
    nonAllocationResult(0) = it
    nonAllocationResult(1) = it.next()
    1
  }
  def testIterator(test: Map[String, String], entries: (String, String)* ): Unit = {
    val keys = entries map(_._1)
    val values = entries map(_._2)

    assertEquals(!entries.isEmpty, test.iterator.hasNext)
    assertEquals(!entries.isEmpty, test.keysIterator.hasNext)
    assertEquals(!entries.isEmpty, test.valuesIterator.hasNext)

    assertEquals(entries.toList, test.iterator.toList)
    assertEquals(entries.drop(0).toList, test.iterator.drop(0).toList)
    assertEquals(entries.drop(1).toList, test.iterator.drop(1).toList)
    assertEquals(entries.drop(2).toList, test.iterator.drop(2).toList)
    assertEquals(entries.drop(3).toList, test.iterator.drop(3).toList)
    assertEquals(entries.drop(4).toList, test.iterator.drop(4).toList)
    assertEquals(Nil, test.iterator.drop(5).toList)

    assertEquals(keys.toList, test.keysIterator.toList)
    assertEquals(keys.drop(0).toList, test.keysIterator.drop(0).toList)
    assertEquals(keys.drop(1).toList, test.keysIterator.drop(1).toList)
    assertEquals(keys.drop(2).toList, test.keysIterator.drop(2).toList)
    assertEquals(keys.drop(3).toList, test.keysIterator.drop(3).toList)
    assertEquals(keys.drop(4).toList, test.keysIterator.drop(4).toList)
    assertEquals(Nil, test.keysIterator.drop(5).toList)

    assertEquals(values.toList, test.valuesIterator.toList)
    assertEquals(values.drop(0).toList, test.valuesIterator.drop(0).toList)
    assertEquals(values.drop(1).toList, test.valuesIterator.drop(1).toList)
    assertEquals(values.drop(2).toList, test.valuesIterator.drop(2).toList)
    assertEquals(values.drop(3).toList, test.valuesIterator.drop(3).toList)
    assertEquals(values.drop(4).toList, test.valuesIterator.drop(4).toList)
    assertEquals(Nil, test.valuesIterator.drop(5).toList)
  }

  @Test def mapEmptyAllocating(): Unit = {
    val test = Map.empty
    nonAllocating(iterator(test), ignoreEqualCheck = true)
    nonAllocating(consume(iterator(test)), ignoreEqualCheck = true)
    nonAllocating(consume(iterator(test).drop(1)), ignoreEqualCheck = true)

    nonAllocating(keysIterator(test), ignoreEqualCheck = true)
    nonAllocating(consume(keysIterator(test)), ignoreEqualCheck = true)
    nonAllocating(consume(keysIterator(test).drop(1)), ignoreEqualCheck = true)

    nonAllocating(valuesIterator(test), ignoreEqualCheck = true)
    nonAllocating(consume(valuesIterator(test)), ignoreEqualCheck = true)
    nonAllocating(consume(valuesIterator(test).drop(1)), ignoreEqualCheck = true)

  }
  @Test def mapEmptyBehaviour(): Unit = {
    val test = Map.empty[String, String]
    testIterator(test)
  }

  @Test def map1Allocating(): Unit = {
    val test = new Map.Map1("k1", "v1")
    exactAllocates(48, ignoreEqualCheck = true)(iterator(test))
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume(iterator(test))), "k1" -> "v1")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume(iterator(test).drop(1))))

    exactAllocates(24, ignoreEqualCheck = true)(keysIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test))), "k1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test).drop(1))))

    exactAllocates(24, ignoreEqualCheck = true)(valuesIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test))), "v1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test).drop(1))))
  }


  @Test def map1Behaviour(): Unit = {
    val test = new Map.Map1("k1", "v1")
    testIterator(test, ("k1", "v1"))
  }

  def assertResult(reportedSize: Int, expected: AnyRef *): Unit = {
    assertTrue(nonAllocationResult(0).isInstanceOf[Iterator[_]])
    assertEquals(expected.length  , reportedSize)
    assertEquals(expected.toList, nonAllocationResult.slice(1, reportedSize+1).toList)
  }

  @Test def map2Allocating(): Unit = {
    val test = new Map.Map2("k1", "v1", "k2", "v2")
    exactAllocates(24, ignoreEqualCheck = true)(iterator(test))
    assertResult(exactAllocates(56, ignoreEqualCheck = true)(consume(iterator(test))), "k1" -> "v1", "k2" -> "v2")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test))), "k1" -> "v1")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume(iterator(test).drop(1))), "k2" -> "v2")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test).drop(1))), "k2" -> "v2")

    exactAllocates(24, ignoreEqualCheck = true)(keysIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test))), "k1", "k2")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test))), "k1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test).drop(1))), "k2")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test).drop(1))), "k2")

    exactAllocates(24, ignoreEqualCheck = true)(valuesIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test))), "v1", "v2")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test))), "v1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test).drop(1))), "v2")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test).drop(1))), "v2")
  }
  @Test def map2Behaviour(): Unit = {
    val test = new Map.Map2("k1", "v1", "k2", "v2")
    testIterator(test, ("k1", "v1"), ("k2", "v2"))
  }
  @Test def map3Allocating(): Unit = {
    val test = new Map.Map3("k1", "v1", "k2", "v2", "k3", "v3")
    exactAllocates(24, ignoreEqualCheck = true)(iterator(test))
    assertResult(exactAllocates(80, ignoreEqualCheck = true)(consume(iterator(test))),"k1"-> "v1", "k2"-> "v2", "k3"-> "v3")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test))),"k1"-> "v1")
    assertResult(exactAllocates(56, ignoreEqualCheck = true)(consume(iterator(test).drop(1))), "k2"-> "v2", "k3"-> "v3")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test).drop(1))), "k2"-> "v2")

    exactAllocates(24, ignoreEqualCheck = true)(keysIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test))), "k1", "k2", "k3")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test))), "k1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test).drop(1))), "k2", "k3")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test).drop(1))), "k2")

    exactAllocates(24, ignoreEqualCheck = true)(valuesIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test))), "v1", "v2", "v3")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test))), "v1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test).drop(1))), "v2", "v3")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test).drop(1))), "v2")
  }
  @Test def map3Behaviour(): Unit = {
    val test = new Map.Map3("k1", "v1", "k2", "v2", "k3", "v3")
    testIterator(test, ("k1", "v1"), ("k2", "v2"), ("k3", "v3"))
  }
  @Test def map4Allocating(): Unit = {
    val test = new Map.Map4("k1", "v1", "k2", "v2", "k3", "v3", "k4", "v4")
    exactAllocates(24, ignoreEqualCheck = true)(iterator(test))
    assertResult(exactAllocates(104, ignoreEqualCheck = true)(consume(iterator(test))), "k1"-> "v1", "k2"-> "v2", "k3"-> "v3", "k4"-> "v4")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test))), "k1"-> "v1")
    assertResult(exactAllocates(80, ignoreEqualCheck = true)(consume(iterator(test).drop(1))), "k2"-> "v2", "k3"-> "v3", "k4"-> "v4")
    assertResult(exactAllocates(32, ignoreEqualCheck = true)(consume1(iterator(test).drop(1))),"k2"-> "v2")

    exactAllocates(24, ignoreEqualCheck = true)(keysIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test))), "k1", "k2", "k3", "k4")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test))), "k1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(keysIterator(test).drop(1))), "k2", "k3", "k4")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(keysIterator(test).drop(1))), "k2")

    exactAllocates(24, ignoreEqualCheck = true)(valuesIterator(test))
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test))), "v1", "v2", "v3", "v4")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test))), "v1")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume(valuesIterator(test).drop(1))), "v2", "v3", "v4")
    assertResult(exactAllocates(8, ignoreEqualCheck = true)(consume1(valuesIterator(test).drop(1))), "v2")
  }
  @Test def map4Behaviour(): Unit = {
    val test = new Map.Map4("k1", "v1", "k2", "v2", "k3", "v3", "k4", "v4")
    testIterator(test, ("k1", "v1"), ("k2", "v2"), ("k3", "v3"), ("k4", "v4"))
  }
}
