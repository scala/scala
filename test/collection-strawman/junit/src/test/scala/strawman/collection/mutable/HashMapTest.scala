package strawman.collection
package mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.List

@RunWith(classOf[JUnit4])
class HashMapTest {

  @Test
  def getOrElseUpdate_mutationInCallback(): Unit = {
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

}
