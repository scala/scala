package strawman.collection.mutable

import strawman.collection.immutable.{List, Range}

import org.junit.Test
import org.junit.Assert._

class HashSetTest {
  // based on run/hashset.scala partest
  @Test
  def testPar: Unit = {
    val h1 = new HashSet[Int]
    for (i <- Range(0, 20)) h1 += i
    for (i <- Range(0, 20)) assertTrue(h1.contains(i))
    for (i <- Range(20, 40)) assertFalse(h1.contains(i))
    assertEquals(Range(0, 20).to(List).sorted, h1.to(List).sorted)

    val h2 = new HashSet[String]
    h2 += null
    for (i <- Range(0, 20)) h2 +=  "" + i
    assertTrue(h2 contains null)
    for (i <- Range(0, 20)) assertTrue(h2.contains("" + i))
    for (i <- Range(20, 40)) assertFalse(h2.contains("" + i))
    assertEquals(Range(0, 20).map("" + _).to(List).sorted.mkString(",") + ",null", h2.to(List).map("" + _).sorted.mkString(","))

    h2 -= null
    h2 -= "" + 0
    assertFalse(h2 contains null)
    assertEquals(Range(1, 20).map("" + _).to(List).sorted.mkString(","), h2.to(List).map("" + _).sorted.mkString(","))
  }

  @Test
  def si4894: Unit = {
    val hs = HashSet[Int]()
    hs ++= Range.inclusive(1, 10)
    hs --= Range.inclusive(1, 10)
    assertTrue(hs.isEmpty)
  }

  @Test
  def mapInPlace_addOneToAll(): Unit = {
    val hs = HashSet[Int]()
    hs += 1
    hs += 2
    hs += 3
    hs.mapInPlace(_ + 1)
    assertEquals(List(2, 3, 4), hs.toList.sorted)
  }

  @Test
  def mapInPlace_reducedToOneElement(): Unit = {
    val hs = HashSet[Int]()
    hs += 1
    hs += 2
    hs += 3
    hs.mapInPlace(_ => 1)
    assert(hs.size == 1)
    assert(hs.contains(1))
  }
}
