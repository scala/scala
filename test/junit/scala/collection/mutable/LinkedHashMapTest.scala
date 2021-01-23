package scala.collection.mutable

import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Assert, Test}

import scala.annotation.nowarn
import scala.collection.mutable

/* Test for scala/bug#9095 */
@RunWith(classOf[JUnit4])
class LinkedHashMapTest {
  @nowarn("msg=inheritance from class LinkedHashMap")
  class TestClass extends mutable.LinkedHashMap[String, Int] {
    def lastItemRef = lastEntry
  }
  
  @Test
  def testClear(): Unit = {
    val lhm = new TestClass
    Seq("a" -> 8, "b" -> 9).foreach(kv => lhm.put(kv._1, kv._2))
    
    Assert.assertNotNull(lhm.lastItemRef)
    lhm.clear()
    Assert.assertNull(lhm.lastItemRef)
  }

  @Test
  def testUpdateWith(): Unit = {
    val insertIfAbsent: Option[String] => Option[String] = _.orElse(Some("b"))

    val hashMap1 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap1.updateWith(1)(insertIfAbsent), Some("a"))
    assertEquals(hashMap1, mutable.LinkedHashMap(1 -> "a"))

    val hashMap2 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap2.updateWith(2)(insertIfAbsent), Some("b"))
    assertEquals(hashMap2, mutable.LinkedHashMap(1 -> "a", 2 -> "b"))

    val noneAnytime: Option[String] => Option[String] =  _ => None

    val hashMap3 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap3.updateWith(1)(noneAnytime), None)
    assertEquals(hashMap3, mutable.LinkedHashMap())

    val hashMap4 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap4.updateWith(2)(noneAnytime), None)
    assertEquals(hashMap4, mutable.LinkedHashMap(1 -> "a"))

    val flip: Option[String] => Option[String] =  {
      case None => Some("X")
      case Some(x) => None}

    val hashMap5 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap5.updateWith(1)(flip), None)
    assertEquals(hashMap5, mutable.LinkedHashMap())

    val hashMap6 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap6.updateWith(2)(flip), Some("X"))
    assertEquals(hashMap6, mutable.LinkedHashMap(1 -> "a", 2 -> "X"))

    val transform: Option[String] => Option[String] =  _.map(_ + "2")

    val hashMap7 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap7.updateWith(1)(transform), Some("a2"))
    assertEquals(hashMap7, mutable.LinkedHashMap(1 -> "a2"))

    val hashMap8 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap8.updateWith(2)(transform), None)
    assertEquals(hashMap8, mutable.LinkedHashMap(1 -> "a"))

    val overwrite: Option[String] => Option[String] = _ => Some("X")

    val hashMap9 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap9.updateWith(1)(overwrite), Some("X"))
    assertEquals(hashMap9, mutable.LinkedHashMap(1 -> "X"))

    val hashMap10 = mutable.LinkedHashMap(1 -> "a")
    assertEquals(hashMap10.updateWith(2)(overwrite), Some("X"))
    assertEquals(hashMap10, mutable.LinkedHashMap(1 -> "a", 2 -> "X"))


    val hashMapMulti1 = mutable.LinkedHashMap(1 -> "a", 2 -> "b")
    assertEquals(hashMapMulti1.updateWith(2)(noneAnytime), None)
    assertEquals(hashMapMulti1, mutable.LinkedHashMap(1 -> "a"))

    val hashMapMulti2 = mutable.LinkedHashMap(1 -> "a", 2 -> "b")
    assertEquals(hashMapMulti2.updateWith(1)(noneAnytime), None)
    assertEquals(hashMapMulti2, mutable.LinkedHashMap(2 -> "b"))

    val hashMapMulti3 = mutable.LinkedHashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti3.updateWith(1)(noneAnytime), None)
    assertEquals(hashMapMulti3, mutable.LinkedHashMap(2 -> "b", 3 -> "c"))

    val hashMapMulti4 = mutable.LinkedHashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti4.updateWith(2)(noneAnytime), None)
    assertEquals(hashMapMulti4, mutable.LinkedHashMap(1 -> "a", 3 -> "c"))

    val hashMapMulti5 = mutable.LinkedHashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti5.updateWith(3)(noneAnytime), None)
    assertEquals(hashMapMulti5, mutable.LinkedHashMap(1 -> "a", 2 -> "b"))


    val hashMapCollide1 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide1.updateWith(0)(noneAnytime), None)
    assertEquals(hashMapCollide1, mutable.LinkedHashMap((null: Any) -> "b", "" -> "c"))

    val hashMapCollide2 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide2.updateWith((null: Any))(noneAnytime), None)
    assertEquals(hashMapCollide2, mutable.LinkedHashMap[Any, String](0 -> "a", "" -> "c"))

    val hashMapCollide3 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide3.updateWith("")(noneAnytime), None)
    assertEquals(hashMapCollide3, mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b"))

    val hashMapCollide4 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide4.updateWith(())(noneAnytime), None)
    assertEquals(hashMapCollide4, mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c"))

    val hashMapCollide5 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide5.updateWith(())(insertIfAbsent), Some("b"))
    assertEquals(hashMapCollide5, mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c", () -> "b"))

    val hashMapCollide6 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide6.updateWith("")(insertIfAbsent), Some("c"))
    assertEquals(hashMapCollide6, mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c"))

    val hashMapCollide7 = mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide7.updateWith("")(overwrite), Some("X"))
    assertEquals(hashMapCollide7, mutable.LinkedHashMap(0 -> "a", (null: Any) -> "b", "" -> "X"))


    var count = 0
    val countingKey1 = new Object{
      override def hashCode() = {
        count += 1
        super.hashCode()
      }
    }
    val countingKey2 = new Object{
      override def hashCode() = {
        count += 1
        super.hashCode()
      }
    }

    val hashMapCount1 = mutable.LinkedHashMap(countingKey1 -> "a")
    assertEquals(hashMapCount1.updateWith(countingKey1)(overwrite), Some("X"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount1, mutable.LinkedHashMap(countingKey1 -> "X"))

    count = 0
    val hashMapCount2 = mutable.LinkedHashMap(countingKey1 -> "a")
    assertEquals(hashMapCount2.updateWith(countingKey2)(overwrite), Some("X"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount2, mutable.LinkedHashMap(countingKey1 -> "a", countingKey2 -> "X"))

    count = 0
    val hashMapCount3 = mutable.LinkedHashMap(countingKey1 -> "a")
    assertEquals(hashMapCount3.updateWith(countingKey1)(transform), Some("a2"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount3, mutable.LinkedHashMap(countingKey1 -> "a2"))

    count = 0
    val hashMapCount4 = mutable.LinkedHashMap(countingKey1 -> "a")
    assertEquals(hashMapCount4.updateWith(countingKey2)(transform), None)
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount4, mutable.LinkedHashMap(countingKey1 -> "a"))

  }

  @Test
  def testfromfactorymethod(): Unit = {
    val data = List((1, 'a'), (2,'b'),(3,'c'), (4,'d'),(5,'e'),(6,'f'))
    val lhm = new mutable.LinkedHashMap[Int, Char]
    data.foreach(x => lhm.addOne(x))

    val fromlhm1 = LinkedHashMap.from(data)
    assertEquals(fromlhm1, lhm)

    val fromlhm2 = LinkedHashMap.from(lhm)
    assertEquals(fromlhm2, lhm)
    assertFalse(fromlhm2 eq lhm)
  }
}
