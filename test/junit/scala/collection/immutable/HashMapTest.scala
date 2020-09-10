package scala.collection.immutable

import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class HashMapTest extends AllocationTest {

  private val computeHashF = {
    HashMap.empty.computeHash _
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvs() {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)

    val merged = m.merged(m)(null)
    assertEquals(m, merged)
  }

  @Test
  def canMergeIdenticalHashMap1sWithNullKvsCustomMerge() {
    def m = new HashMap.HashMap1(1, computeHashF(1), 1, null)

    val merged = m.merged(m) {
      case ((k1, v1), (k2, v2)) =>
        (k1, v1 + v2)
    }
    assertEquals(new HashMap.HashMap1(1, computeHashF(1), 2, null), merged)
  }

  @Test
  def canMergeHashMap1sWithNullKvsHashCollision() {
    val key1 = 1000L * 1000 * 1000 * 10
    val key2 = key1.##.toLong
    assert(key1.## == key2.##)

    val m1 = new HashMap.HashMap1(key1, computeHashF(key1.##), 1, null)
    val m2 = new HashMap.HashMap1(key2, computeHashF(key2.##), 1, null)
    val expected = HashMap(key1 -> 1, key2 -> 1)
    val merged = m1.merged(m2)(null)
    assertEquals(expected, merged)
    val mergedWithMergeFunction = m1.merged(m2) { (kv1, kv2) =>
      throw new RuntimeException("Should not be reached.")
    }
    assertEquals(expected, mergedWithMergeFunction)
  }

  @Test
  def canMergeHashMapCollision1WithCorrectMerege() {
    case class A(k: Int) {
      override def hashCode = 0
    }
    val m1 = HashMap(A(0) -> 2, A(1) -> 2)
    val m2 = HashMap(A(0) -> 1, A(1) -> 1)
    val merged = m1.merged(m2) { case ((k, l), (_, r)) => k -> (l - r) }
    val expected = HashMap(A(0) -> 1, A(1) -> 1)
    assertEquals(merged, expected)
  }

  @Test
  def t11257(): Unit = {
    case class PoorlyHashed(i: Int) {
      override def hashCode(): Int = i match {
        case 0 | 1 => 42
        case _ => super.hashCode()
      }
    }
    val hashMapCollision = HashMap(PoorlyHashed(0) -> 0, PoorlyHashed(1) -> 1)
    val singleElementMap = hashMapCollision.split.head
    assert(singleElementMap.isInstanceOf[HashMap.HashMap1[_, _]])
    val stillSingleElement = singleElementMap.split.head
    assert(stillSingleElement.isInstanceOf[HashMap.HashMap1[_, _]])
    val twoElemTrie = stillSingleElement + (PoorlyHashed(2) -> 2)
    assert(twoElemTrie.isInstanceOf[HashMap.HashTrieMap[_, _]])
  }

  def generate(): HashMap[String, String] = {
    (1 to 1000).map { i => s"key $i" -> s"value $i" }(scala.collection.breakOut)
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
  def transform {
    def check(hm: HashMap[String, String]) {
      val hm1 = hm transform ((k, v) => s"$k, $v")
      assert(hm.size == hm1.size, (hm, hm1))
      assert(hm.map { case (k, v) => k -> s"$k, $v" }.toSet == hm1.toSet, (hm, hm1))
    }
    transformTestCases foreach check
  }
  @Test
  def transformBreakout {
    def check(hm: HashMap[String, String]) {
      val hm1: List[(String, String)] = hm.transform((k, v) => s"$k, $v")(collection.breakOut)
      assert(hm1 == hm.toList.map { case (k, v) => k -> s"$k, $v"})
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
    assertSame(nonEmpty, nonAllocating(Map.empty ++ nonEmpty))
  }
  @Test
  def addSharedAllocations(): Unit = {
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
    val nonEmpty2 = nonEmpty1 - "a"
    val nonEmpty3 = nonEmpty1 + ("k" -> 11)
    assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty1))
    assertSame(nonEmpty1, nonAllocating(nonEmpty1 ++ nonEmpty2))
    assertSame(nonEmpty3, nonAllocating(nonEmpty1 ++ nonEmpty3))
  }
  @Test
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
  def mergeWorks(): Unit = {
    def hm(as: Int*): HashMap[Int, Int] = HashMap(as.map(x => (x, x)) : _*)
    def check(m1: HashMap[Int, Int], m2: HashMap[Int, Int]): Unit = {
      val expected = HashMap(m1.toSeq ++ m2.toSeq: _*)
      val actual1 = m1 ++ m2
      val actual2 = m2 ++ m1
      assertEquals(expected, actual1)
      assertEquals(expected, actual2)
      for ((k, v) <- expected) {
        assertEquals(v, actual1(k))
        assertEquals(v, actual2(k))
      }
    }
    check(hm(1, 2, 3), hm(4, 5, 6))
    check(hm(), hm(4, 5, 6))
    check(hm(1, 2, 3), hm(1, 2))
    check(hm(1, 2, 3), hm(1, 2, 3, 4))
    check(hm((1 to 1000): _*), hm((2000 to 3000): _*))
  }
  @Test
  def mixTrie(): Unit = {
    for (start <- 1 to 20) {
      val m1: Map[Int, Int] = ((start + 1) to (start + 20)).map(x => x -> x).toMap
      val m2: Map[Int, Int] = ((start + 101) to (start + 120)).map(x => x -> x).toMap

      val b = HashMap.newBuilder[Int, Int]
      b ++= m1
      b ++= m2
      val res = b.result()
      assertEquals(40, res.size)
      for (i <- (start + 1) to (start + 20)) {
        assertEquals(Some(i), res.get(i))
        assertEquals(Some(i + 100), res.get(i + 100))
      }
    }
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
    //calls TraversableLike.++
    assertEquals(1, (m2 ++ m1).apply(2))
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
    //calls GenTraversableOnce.++
    assertEquals(1, (m2 ++ m1).apply(2))
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
      assertIdenticalKeys(Map((c0l, ())), c0LMap ++: HashMap((c0r, ())))
    }
    check(cs => HashMap(cs: _*)) // exercise special case for HashMap/HashMap
    check(cs => TreeMap(cs: _*)) // exercise special case for HashMap/HasForEachEntry
    check(cs => HashMap(cs: _*).withDefault(_ => ???)) // default cases
  }

  @Test
  def t12140(): Unit = {
    trait DataType              extends Product with Serializable
    case class Foo(arg: String) extends DataType
    case class Bar(arg: String) extends DataType

    // Create a HashMap consisting of a single HashMapCollision1.
    val collision = HashMap(Foo("toxic") -> 0, Bar("toxic") -> 0)
    val singleton = (new HashMap[DataType, Int]).updated(Foo("toxic"), 0)

    collision ++ singleton // was NullPointerExeption
  }
}
