package scala.collection.immutable

import java.util.Collections

import org.junit.Assert._
import org.junit.{Ignore, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AllocationTest
import scala.tools.testkit.AssertUtil.assertThrows

@RunWith(classOf[JUnit4])
class HashMapTest extends AllocationTest{

  @Test
  def canMergeIdenticalHashMaps(): Unit = {
    def m = HashMap(1 -> 1)
    val merged = m.merged(m)(null)
    assertEquals(m, merged)
  }

  @Test
  def canMergeIdenticalHashMapsCustomMerge(): Unit = {
    def m = HashMap(1 -> 1)
    val merged = m.merged(m) {
      case ((k1, v1), (_, v2)) => (k1, v1 + v2)
    }
    assertEquals(HashMap(1 -> 2), merged)
  }

  @Test
  def canMergeHashMapsWithHashCollision(): Unit = {
    val key1 = 1000L * 1000 * 1000 * 10
    val key2 = key1.##.toLong
    assert(key1.## == key2.##)

    val m1 = HashMap(key1 -> 1)
    val m2 = HashMap(key2 -> 1)
    val expected = HashMap(key1 -> 1, key2 -> 1)
    val merged = m1.merged(m2)(null)
    assertEquals(expected, merged)
    val mergedWithMergeFunction = m1.merged(m2) { (kv1, kv2) =>
      throw new RuntimeException("Should not be reached.")
    }
    assertEquals(expected, mergedWithMergeFunction)
  }

  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("missing")
    assertEquals("a", m2(1))
    assertEquals("missing", m2(3))
  }

  @Test
  def factoryReuse(): Unit = {
    assertSame(HashMap.empty, HashMap.empty)
    assertSame(HashMap.empty, HashMap())
    val m = HashMap("a" -> "a")
    assertSame(m, HashMap.from(m))
  }

  @Test
  def testGetOrElse(): Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")
    assertEquals("a", m1.getOrElse(1, ???))
    assertEquals("c", m1.getOrElse(3, "c"))

    class Collider {
      override def hashCode = 0
    }
    val a, b, c = new Collider
    val m2 = HashMap(a -> "a", b -> "b")
    assertEquals("a", m2.getOrElse(a, ???))
    assertEquals("c", m2.getOrElse(c, "c"))
  }

  @Test
  def testWithDefault(): Unit = {
    val m1 = HashMap(1 -> "a", 2 -> "b")

    val m2: Map[Int, String] =
      m1.withDefault(i => (i + 1).toString)
        .updated(1, "aa")
        .updated(100, "bb")
        .concat(List(500 -> "c", 501 -> "c"))

    assertEquals(m2(1), "aa")
    assertEquals(m2(2), "b")
    assertEquals(m2(3), "4")
    assertEquals(m2(4), "5")
    assertEquals(m2(500), "c")
    assertEquals(m2(501), "c")
    assertEquals(m2(502), "503")

    val m3: Map[Int, String] = m2 - 1
    assertEquals(m3(1), "2")

    val m4: Map[Int, String] = m3 -- List(2, 100)
    assertEquals(m4(2), "3")
    assertEquals(m4(100), "101")
  }

  @Test
  def canMergeHashMapCollision1WithCorrectMerge(): Unit = {
    case class A(k: Int) { override def hashCode = 0 }
    val m1 = HashMap(A(0) -> 2, A(1) -> 2)
    val m2 = HashMap(A(0) -> 1, A(1) -> 1)
    val merged = m1.merged(m2) { case ((k, l), (_, r)) => k -> (l - r) }
    val expected = HashMap(A(0) -> 1, A(1) -> 1)
    assertEquals(merged, expected)
  }

  @Test
  def transformReturnsOriginalMap(): Unit = {
    case class A(i: Int, j: Int) { override def hashCode = j }

    val hashMap = HashMap(
      A(1, 1) -> 1,
      A(1, 2) -> 1,
      A(2, 1) -> 1,
      A(2, 2) -> 1
    )

    assert(hashMap.transform((_, v) => v) eq hashMap)
  }

  @Test
  def testUpdatedWith(): Unit = {
    val hashMap = HashMap(1 -> "a")

    val insertIfAbesent: Option[String] => Option[String] = _.orElse(Some("b"))
    assertEquals(hashMap.updatedWith(1)(insertIfAbesent), HashMap(1 -> "a"))
    assertEquals(hashMap.updatedWith(2)(insertIfAbesent), HashMap(1 -> "a", 2 -> "b"))

    val noneAnytime: Option[String] => Option[String] = _ => None
    assertEquals(hashMap.updatedWith(1)(noneAnytime), HashMap())
    assertEquals(hashMap.updatedWith(2)(noneAnytime), HashMap(1 -> "a"))
  }

  @Test
  def t11559(): Unit = {
    assertEquals(HashMap("c" -> "c"), HashMap("a" -> "b").merged(HashMap("a" -> "b"))((_,_) => "c" -> "c"))
    assertEquals(HashMap(4 -> 4), HashMap(1 -> 1).merged(HashMap(1 -> 1))((_,_) => 4 -> 4))
    assertEquals(HashMap(1 -> 1, 3 -> 3, 4 -> 4), HashMap(1 -> 1, 2 -> 2).merged(HashMap(2 -> 2, 3 -> 3))((_,_) => 4 -> 4))
  }

  def generate(): HashMap[String, String] = {
    HashMap.from((1 to 1000).map { i => s"key $i" -> s"value $i" })
  }
  @Test
  def nonAllocatingIdentical(): Unit = {
    val base = generate()
    assertTrue(nonAllocating {
      base == base
    })
  }

  @Test
  def nonAllocatingNotShared(): Unit = {
    val base = generate()
    val notShared = generate()

    assertTrue(nonAllocating {
      base == notShared
    })
    assertTrue(nonAllocating {
      notShared == base
    })
  }
  @Test
  def nonAllocatingShared(): Unit = {
    val base = generate()
    val shared = (base - base.head._1) + base.head

    assertTrue(nonAllocating {
      base == shared
    })
    assertTrue(nonAllocating {
      shared == base
    })
  }

  private val transformTestCases: List[HashMap[String, String]] = List(
    HashMap("a" -> "b"),
    HashMap("a" -> "b", "b" -> "c"),
    HashMap("a" -> "b", "b" -> "c", "c" -> "d"),
    HashMap("Ea" -> "FB", "FB" -> "Ea", "xyz" -> "xyz")
  )

  @Test
  def transform(): Unit = {
    def check(hm: HashMap[String, String]): Unit = {
      val hm1 = hm transform ((k, v) => s"$k, $v")
      assert(hm.size == hm1.size, (hm, hm1))
      assert(hm.map { case (k, v) => k -> s"$k, $v" }.toSet == hm1.toSet, (hm, hm1))
    }
    transformTestCases foreach check
  }

  @Test
  def addEmptyAllocations(): Unit = {
    val nonEmpty = HashMap("a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "e" -> 5,
      "f" -> 6,
      "g" -> 7,
      "h" -> 8,
      "i" -> 9,
      "j" -> 10
    )
    assertSame(nonEmpty, nonAllocating(nonEmpty ++ HashMap.empty))
    assertSame(nonEmpty, nonAllocating(nonEmpty ++ Map.empty))
    assertSame(nonEmpty, nonAllocating(HashMap.empty ++ nonEmpty))
    assertSame(nonEmpty, nonAllocating(Map.empty.concat(nonEmpty)))
  }

  @Test
  def addSharedAllocations1(): Unit = {
    val nonEmpty1 = HashMap("a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "e" -> 5,
      "f" -> 6,
      "g" -> 7,
      "h" -> 8,
      "i" -> 9,
      "j" -> 10
    )
    assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty1))

  }

  @Test
  @Ignore // TODO Port {HashMap, HashSet}.concat allocation reduction
  def addSharedAllocations2(): Unit = {
    val nonEmpty1 = HashMap("a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "e" -> 5,
      "f" -> 6,
      "g" -> 7,
      "h" -> 8,
      "i" -> 9,
      "j" -> 10
    )
    assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty1))

     val nonEmpty2 = nonEmpty1 - "a"
     val nonEmpty3 = nonEmpty1 + ("k" -> 11)
     assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty2))
     assertSame(nonEmpty3, nonAllocating(nonEmpty1 ++ nonEmpty3))
  }

  @Test
  @Ignore // TODO Port {HashMap, HashSet}.concat allocation reduction
  def addCollidingAllocations(): Unit = {
    val nonEmpty1 = HashMap("a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "e" -> 5,
      "f" -> 6,
      "g" -> 7,
      "h" -> 8,
      "i" -> 9,
      "j" -> 10
    )
    val nonEmpty2 = HashMap("a" -> 1,
      "b" -> 2,
      "c" -> 3,
      "d" -> 4,
      "e" -> 5,
      "f" -> 6,
      "g" -> 7,
      "h" -> 8,
      "i" -> 9,
      "j" -> 10
    )
    assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty2))
  }

  @Test
  def `++_1`(): Unit = {
    val m1 = Map[Any, Int] (
      1 -> 1,
      2 -> 1,
      3 -> 1,
      4 -> 1,
      5 -> 1
      )
    val m2 = Map[Int, Int] (2->2)
    val m3: Iterable[(Any, Int)] = m2 ++ m1
    assertEquals(1, Map.from(m3).apply(2))
  }

  @Test
  def `++_2`(): Unit = {
    val m1 = Map[Int, Int] (
      1 -> 1,
      2 -> 1,
      3 -> 1,
      4 -> 1,
      5 -> 1
      )
    val m2 = Map[Int, Int] (2->2)
    val m3 = m2 ++ m1
    assertEquals(1, m3.apply(2))
  }

  @Test
  def retainLeft(): Unit = {
    case class C(a: Int)(override val toString: String)
    implicit val ordering: Ordering[C] = Ordering.by(_.a)
    val c0l = C(0)("l")
    val c0r = C(0)("r")
    def assertIdenticalKeys(expected: Map[C, Unit], actual: Map[C, Unit]): Unit = {
      val expected1, actual1 = Collections.newSetFromMap[C](new java.util.IdentityHashMap())
      expected.keys.foreach(expected1.add)
      actual.keys.foreach(actual1.add)
      assertEquals(expected1, actual1)
    }
    assertIdenticalKeys(Map((c0l, ())), HashMap((c0l, ())).updated(c0r, ()))

    def check(factory: Seq[(C, Unit)] => Map[C, Unit]): Unit = {
      val c0LMap = factory(Seq((c0l, ())))
      val c0RMap = factory(Seq((c0r, ())))
      assertIdenticalKeys(Map((c0l, ())), HashMap((c0l, ())).++(c0RMap))
      assertIdenticalKeys(Map((c0l, ())), HashMap.newBuilder[C, Unit].++=(HashMap((c0l, ()))).++=(c0RMap).result())
      assertIdenticalKeys(Map((c0l, ())), HashMap((c0l, ())).++(c0RMap))
      assertIdenticalKeys(Map((c0l, ())), c0LMap ++ HashMap((c0r, ())))
    }
    check(cs => HashMap(cs: _*)) // exercise special case for HashMap/HashMap
    check(cs => TreeMap(cs: _*)) // exercise special case for HashMap/HasForEachEntry
    check(cs => HashMap(cs: _*).withDefault(_ => ???)) // default cases
  }

  @Test
  def noSuchElement(): Unit = {
    assertThrows[NoSuchElementException](HashMap(1->1)(2), _ == "key not found: 2")
    assertThrows[NoSuchElementException](HashMap.empty(3), _ == "key not found: 3")
  }
}
