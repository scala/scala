package scala.collection
package mutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

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

  @deprecated("Uses deprecated extension", since="2.13")
  @Test
  def customGet(): Unit = {
    val gotItAll = new mutable.HashMap[String, String] {
      override def get(key: String): Option[String] = Some(key)
    }
    assertEquals("a", gotItAll.getOrElse("a", "b"))
    assertEquals("a", gotItAll.getOrElseUpdate("a", "b"))
  }
  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = mutable.HashMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("")

    assertEquals(m2(1), "a")
    assertEquals(m2(3), "")

    m2 += (3 -> "c")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "")

    m2 ++= List(4 -> "d", 5 -> "e", 6 -> "f")
    assertEquals(m2(3), "c")
    assertEquals(m2(4), "d")
    assertEquals(m2(5), "e")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    m2 --= List(3, 4, 5)
    assertEquals(m2(3), "")
    assertEquals(m2(4), "")
    assertEquals(m2(5), "")
    assertEquals(m2(6), "f")
    assertEquals(m2(7), "")

    val m3 = m2 ++ List(3 -> "333")
    assertEquals(m2(3), "")
    assertEquals(m3(3), "333")
  }
  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def testWithDefault(): Unit = {
    val m1 = mutable.HashMap(1 -> "a", 2 -> "b")

    val m2: mutable.Map[Int, String] = m1.withDefault(i => (i + 1).toString)
    m2.update(1, "aa")
    m2.update(100, "bb")
    m2.addAll(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: mutable.Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: mutable.Map[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }
  @Test
  def testUpdateWith(): Unit = {
    val insertIfAbsent: Option[String] => Option[String] = _.orElse(Some("b"))

    val hashMap1 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap1.updateWith(1)(insertIfAbsent), Some("a"))
    assertEquals(hashMap1, mutable.HashMap(1 -> "a"))

    val hashMap2 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap2.updateWith(2)(insertIfAbsent), Some("b"))
    assertEquals(hashMap2, mutable.HashMap(1 -> "a", 2 -> "b"))

    val noneAnytime: Option[String] => Option[String] =  _ => None

    val hashMap3 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap3.updateWith(1)(noneAnytime), None)
    assertEquals(hashMap3, mutable.HashMap())

    val hashMap4 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap4.updateWith(2)(noneAnytime), None)
    assertEquals(hashMap4, mutable.HashMap(1 -> "a"))

    val flip: Option[String] => Option[String] =  {
      case None => Some("X")
      case Some(x) => None}

    val hashMap5 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap5.updateWith(1)(flip), None)
    assertEquals(hashMap5, mutable.HashMap())

    val hashMap6 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap6.updateWith(2)(flip), Some("X"))
    assertEquals(hashMap6, mutable.HashMap(1 -> "a", 2 -> "X"))

    val transform: Option[String] => Option[String] =  _.map(_ + "2")

    val hashMap7 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap7.updateWith(1)(transform), Some("a2"))
    assertEquals(hashMap7, mutable.HashMap(1 -> "a2"))

    val hashMap8 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap8.updateWith(2)(transform), None)
    assertEquals(hashMap8, mutable.HashMap(1 -> "a"))

    val overwrite: Option[String] => Option[String] = _ => Some("X")

    val hashMap9 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap9.updateWith(1)(overwrite), Some("X"))
    assertEquals(hashMap9, mutable.HashMap(1 -> "X"))

    val hashMap10 = mutable.HashMap(1 -> "a")
    assertEquals(hashMap10.updateWith(2)(overwrite), Some("X"))
    assertEquals(hashMap10, mutable.HashMap(1 -> "a", 2 -> "X"))


    val hashMapMulti1 = mutable.HashMap(1 -> "a", 2 -> "b")
    assertEquals(hashMapMulti1.updateWith(2)(noneAnytime), None)
    assertEquals(hashMapMulti1, mutable.HashMap(1 -> "a"))

    val hashMapMulti2 = mutable.HashMap(1 -> "a", 2 -> "b")
    assertEquals(hashMapMulti2.updateWith(1)(noneAnytime), None)
    assertEquals(hashMapMulti2, mutable.HashMap(2 -> "b"))

    val hashMapMulti3 = mutable.HashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti3.updateWith(1)(noneAnytime), None)
    assertEquals(hashMapMulti3, mutable.HashMap(2 -> "b", 3 -> "c"))

    val hashMapMulti4 = mutable.HashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti4.updateWith(2)(noneAnytime), None)
    assertEquals(hashMapMulti4, mutable.HashMap(1 -> "a", 3 -> "c"))

    val hashMapMulti5 = mutable.HashMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(hashMapMulti5.updateWith(3)(noneAnytime), None)
    assertEquals(hashMapMulti5, mutable.HashMap(1 -> "a", 2 -> "b"))


    val hashMapCollide1 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide1.updateWith(0)(noneAnytime), None)
    assertEquals(hashMapCollide1, mutable.HashMap((null: Any) -> "b", "" -> "c"))

    val hashMapCollide2 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide2.updateWith((null: Any))(noneAnytime), None)
    assertEquals(hashMapCollide2, mutable.HashMap[Any, String](0 -> "a", "" -> "c"))

    val hashMapCollide3 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide3.updateWith("")(noneAnytime), None)
    assertEquals(hashMapCollide3, mutable.HashMap(0 -> "a", (null: Any) -> "b"))

    val hashMapCollide4 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide4.updateWith(())(noneAnytime), None)
    assertEquals(hashMapCollide4, mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c"))

    val hashMapCollide5 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide5.updateWith(())(insertIfAbsent), Some("b"))
    assertEquals(hashMapCollide5, mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c", () -> "b"))

    val hashMapCollide6 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide6.updateWith("")(insertIfAbsent), Some("c"))
    assertEquals(hashMapCollide6, mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c"))

    val hashMapCollide7 = mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "c")
    assertEquals(hashMapCollide7.updateWith("")(overwrite), Some("X"))
    assertEquals(hashMapCollide7, mutable.HashMap(0 -> "a", (null: Any) -> "b", "" -> "X"))


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

    val hashMapCount1 = mutable.HashMap(countingKey1 -> "a")
    assertEquals(hashMapCount1.updateWith(countingKey1)(overwrite), Some("X"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount1, mutable.HashMap(countingKey1 -> "X"))

    count = 0
    val hashMapCount2 = mutable.HashMap(countingKey1 -> "a")
    assertEquals(hashMapCount2.updateWith(countingKey2)(overwrite), Some("X"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount2, mutable.HashMap(countingKey1 -> "a", countingKey2 -> "X"))

    count = 0
    val hashMapCount3 = mutable.HashMap(countingKey1 -> "a")
    assertEquals(hashMapCount3.updateWith(countingKey1)(transform), Some("a2"))
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount3, mutable.HashMap(countingKey1 -> "a2"))

    count = 0
    val hashMapCount4 = mutable.HashMap(countingKey1 -> "a")
    assertEquals(hashMapCount4.updateWith(countingKey2)(transform), None)
    assertEquals(2, count) // once during hashtable construction, once during updateWith
    assertEquals(hashMapCount4, mutable.HashMap(countingKey1 -> "a"))
  }

  @Test
  def t11737(): Unit = {
    val m = mutable.HashMap(
      0 -> 1, 1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 1, 7 -> 1, 8 -> 1, 9 -> 1, 241 -> 1)
      .mapValuesInPlace((_, _) => -1)
    assertTrue(m.toString, m.forall(_._2 == -1))
  }
}
