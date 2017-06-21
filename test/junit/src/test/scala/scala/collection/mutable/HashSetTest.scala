package scala.collection.mutable

import org.junit.Test
import org.junit.Assert._

class HashSetTest {
  // based on run/hashset.scala partest
  @Test
  def testPar: Unit = {
    val h1 = new HashSet[Int]
    for (i <- 0 until 20) h1 += i
    for (i <- 0 until 20) assertTrue(h1.contains(i))
    for (i <- 20 until 40) assertFalse(h1.contains(i))
    assertEquals((0 until 20).toList.sorted, h1.toList.sorted)

    val h2 = new HashSet[String]
    h2 += null
    for (i <- 0 until 20) h2 +=  "" + i
    assertTrue(h2 contains null)
    for (i <- 0 until 20) assertTrue(h2.contains("" + i))
    for (i <- 20 until 40) assertFalse(h2.contains("" + i))
    assertEquals((0 until 20).map("" + _).toList.sorted.mkString(",") + ",null", h2.toList.map("" + _).sorted.mkString(","))

    h2 -= null
    h2 -= "" + 0
    assertFalse(h2 contains null)
    assertEquals((1 until 20).map("" + _).toList.sorted.mkString(","), h2.toList.map("" + _).sorted.mkString(","))
  }

  @Test
  def si4894: Unit = {
    val hs = HashSet[Int]()
    hs ++= 1 to 10
    hs --= 1 to 10
    assertTrue(hs.isEmpty)
  }
}
