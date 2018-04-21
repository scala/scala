package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashMapTest {

  @Test
  def getOrElseUpdate_mutationInCallback() {
    val hm = new mutable.HashMap[String, String]()
    // add enough elements to resize the hash table in the callback
    def add() = 1 to 100000 foreach (i => hm(i.toString) = "callback")
    hm.getOrElseUpdate("0", {
      add()
      ""
    })
    assertEquals(Some(""), hm.get("0"))
  }

  @Test
  def getOrElseUpdate_evalOnce(): Unit = {
    var i = 0
    val hm = new mutable.HashMap[Int, Int]()
    hm.getOrElseUpdate(0, {i += 1; i})
    assertEquals(1, hm(0))
  }

  @Test
  def getOrElseUpdate_noEval(): Unit = {
    val hm = new mutable.HashMap[Int, Int]()
    hm.put(0, 0)
    hm.getOrElseUpdate(0, throw new AssertionError())
  }

  def getOrElseUpdate_keyIdempotence(): Unit = {
    val map = mutable.HashMap[String, String]()

    val key = "key"
    map.getOrElseUpdate(key, {
      map.getOrElseUpdate(key, "value1")
      "value2"
    })

    assertEquals(List((key, "value2")), map.toList)
  }

  @Test
  def mapInPlace_addOneToAll(): Unit = {
    val hm = mutable.HashMap[Int, Int]()
    hm.put(1, 1)
    hm.put(2, 2)
    hm.put(3, 3)
    hm.mapInPlace{ case (k, v) => (k, v + 1) }
    assertEquals(List((1, 2), (2, 3), (3, 4)), hm.toList.sortBy(_._1))
  }

  @Test
  def mapInPlace_reducedToOneKey(): Unit = {
    val hm = mutable.HashMap[Int, Int]()
    hm.put(1, 1)
    hm.put(2, 2)
    hm.put(3, 3)
    hm.mapInPlace{ case (_, v) => (1, v + 1) }
    assert(hm.size == 1)
    assert(hm.toList.head._2 > 1)
  }

  @Test
  def flatMapInPlace(): Unit = {
    val hm = mutable.HashMap(1 -> 1, 2 -> 2)
    val fmip = hm.flatMapInPlace { case (k, v) => HashMap(k * 2 -> v * 2) }
    assert(fmip.size == 2)
    assert(fmip == HashMap(2 -> 2, 4 -> 4))
  }

  @Test
  def flatMapInPlace_reducedToOneKey(): Unit = {
    val hm = mutable.HashMap(1 -> 1, 2 -> 2, 3->3)
    val fmip = hm.flatMapInPlace { case (_, v) => HashMap(1 -> v) }
    assert(fmip.size == 1)
    assert(fmip.contains(1))
    assert(fmip(1) == 1 || fmip(1) == 2 || fmip(1) == 3)
  }

  @Test // From collection/strawman #509
  def flatMapInPlace_dropElements(): Unit = {
    val hm = mutable.HashMap(1 -> 1)
    val empty = mutable.HashMap.empty[Int, Int]
    val fmip = hm.flatMapInPlace(_ => empty)
    assert(fmip.size == 0)
  }
}
