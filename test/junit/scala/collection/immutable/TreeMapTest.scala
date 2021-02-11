package scala.collection.immutable

import java.util.Collections

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.AllocationTest

@RunWith(classOf[JUnit4])
class TreeMapTest extends AllocationTest {

  @Test
  def hasCorrectDropAndTakeMethods(): Unit = {
    val tree = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")

    assertEquals(TreeMap.empty[Int, String], tree take Int.MinValue)
    assertEquals(TreeMap.empty[Int, String], tree takeRight Int.MinValue)
    assertEquals(tree, tree drop Int.MinValue)
    assertEquals(tree, tree dropRight Int.MinValue)
  }

  @Test
  def factoryReuse(): Unit = {
    val m = TreeMap("a" -> "a")
    assertSame(m, TreeMap.from(m))
  }
  @Test
  def testWithDefaultValue(): Unit = {
    val m1 = TreeMap(1 -> "a", 2 -> "b")
    val m2 = m1.withDefaultValue("missing")
    assertEquals("a", m2(1))
    assertEquals("missing", m2(3))
  }
  @Test
  def testWithDefault(): Unit = {
    val m1 = TreeMap(1 -> "a", 2 -> "b")

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

  @Test def entriesEqualSimple(): Unit = {
    val tree1 = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    val tree2 = TreeMap(1 -> "a", 2 -> "b", 3 -> "c")
    assertEquals(tree1, tree2)
  }
  @Test def entriesEqual(): Unit = {
    val b1 = TreeMap.newBuilder[Int, String]
    for ( i <- 10 to 1000) {
      b1 += i -> s"$i value"
    }
    val tree1 = b1.result()
    val b2 = TreeMap.newBuilder[Int, String]
    for ( i <- 1 to 1000) {
      b2 += i -> s"$i value"
    }
    val tree2 = b2.result().drop(9)

    assertEquals(tree1, tree2)
    assertNotEquals(tree1, (tree2+ (9999 -> "zzz")))
    assertNotEquals((tree1+ (9999 -> "zzz")), (tree2))
    assertEquals((tree1+ (9999 -> "zzz")), (tree2+ (9999 -> "zzz")))
    assertNotEquals((tree1+ (9999 -> "zzz")), (tree2+ (9999999 -> "zzz")))
  }
  @Test def equalFastPath(): Unit = {
    class V(val s: String) {
      override def equals(obj: Any): Boolean = obj match {
        case v:V => v.s == s
      }
    }
    var compareCount = 0
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        val res = s.compareTo(that.s)
        compareCount += 1
        res
      }

      override def equals(obj: Any): Boolean = {
        fail("equals should not be called = the trees should be ordered and compared via the sort order")
        false
      }
    }
    val b1 = TreeMap.newBuilder[K, V]
    for ( i <- 10 to 1000) {
      b1 += new K(i.toString) -> new V(s"$i value")
    }
    val tree1 = b1.result()
    compareCount = 0
    nonAllocating(assertEquals(tree1, tree1))
    assertEquals(0, compareCount)
    var exp = tree1.drop(5)
    var act = tree1.drop(5)

    compareCount = 0
    onlyAllocates(240)(assertEquals(exp, act))
    assertEquals(0, compareCount)

    exp += new K("XXX") -> new V("YYY")
    act += new K("XXX") -> new V("YYY")

    compareCount = 0
    assertEquals(exp, act)
    assertTrue(compareCount.toString, compareCount < 30)

    onlyAllocates(408)(assertEquals(exp, act))
  }
  @Test
  def plusWithContains(): Unit = {
    val data = Array.tabulate(1000)(i => s"${i}Key" -> s"${i}Value")
    val tree = (TreeMap.newBuilder[String, String] ++= data).result()

    data foreach {
      case (k, v) =>
        assertSame(tree, nonAllocating(tree.updated(k, v)))
    }
  }
  @Test def consistentEquals(): Unit = {
    class V(val s: String) {

      override def equals(obj: Any): Boolean = obj match {
        case v:V => v.s == s
        case _ => false
      }
      override def toString: String = s"V-$s"
    }
    class K(val s: String) extends Ordered[K] {
      override def toString: String = s"K-$s"

      override def compare(that: K): Int = {
        fail("compare should not be called  - should be handled by the custom ordering")
        0
      }
      override def equals(obj: Any): Boolean = obj match {
        case k:K => k.s == s
        case _ => false
      }
      override def hashCode(): Int = s.hashCode

    }
    class CustomOrder(val selfEqual: Boolean) extends Ordering[K] {
      override def compare(x: K, y: K): Int = x.s compareTo y.s

      override def equals(obj: Any): Boolean = obj match {
        case c: CustomOrder => (c eq this) || this.selfEqual && c.selfEqual
        case _ => false
      }
    }
    val o1 = new CustomOrder(true)
    val o2_1 = new CustomOrder(false)
    val o2_2 = new CustomOrder(false)

    val b1_1 = TreeMap.newBuilder[K, V](o1)
    val b1_2 = TreeMap.newBuilder[K, V](o1)

    val b2_1 = TreeMap.newBuilder[K, V](o2_1)
    val b2_2 = TreeMap.newBuilder[K, V](o2_2)

    val bHash = HashMap.newBuilder[K,V]
    for (i <- 10 to 20) {
      b1_1 += new K(i.toString) -> new V(s"$i value")
      b1_2 += new K(i.toString) -> new V(s"$i value")

      b2_1 += new K(i.toString) -> new V(s"$i value")
      b2_2 += new K(i.toString) -> new V(s"$i value")

      bHash += new K(i.toString) -> new V(s"$i value")
    }
    val tree1_1 = b1_1.result()
    val tree1_2 = b1_2.result()

    val tree2_1 = b1_1.result()
    val tree2_2 = b1_2.result()

    val treeHash = bHash.result()

    val all = List[(Map[K, V], String)]((tree1_1,"tree1_1"), (tree1_2, "tree1_2"), (tree2_1, "tree2_1"), (tree2_2, "tree2_2"), (treeHash, "treeHash"))
    for ((lhs, lText ) <- all;
         (rhs, rText) <-all) {
      assertEquals(s"$lText $rText", lhs, rhs)
      assertEquals(s"$rText $lText", rhs, lhs)
    }
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
    assertIdenticalKeys(Map((c0l, ())), HashMap((c0l, ())).concat(HashMap((c0r, ()))))
    assertIdenticalKeys(Map((c0l, ())), TreeMap((c0l, ())).concat(HashMap((c0r, ()))))
    assertIdenticalKeys(Map((c0l, ())), TreeMap((c0l, ())).concat(TreeMap((c0r, ()))))

    assertIdenticalKeys(Map((c0l, ())), HashMap.newBuilder[C, Unit].++=(HashMap((c0l, ()))).++=(HashMap((c0r, ()))).result())
    assertIdenticalKeys(Map((c0l, ())), TreeMap.newBuilder[C, Unit].++=(TreeMap((c0l, ()))).++=(HashMap((c0r, ()))).result())
    assertIdenticalKeys(Map((c0l, ())), TreeMap.newBuilder[C, Unit].++=(TreeMap((c0l, ()))).++=(TreeMap((c0r, ()))).result())
  }

  @Test
  def overwriteEntryRegression(): Unit = {
    val x = TreeMap(1 -> "herring", 2 -> "cod", 3 -> "salmon")
    val y = TreeMap(3 -> "wish")
    val r1 = x ++ y
    val r2 = (x.toSeq ++ y.toSeq).toMap
    assertEquals(r1, r2)
  }

  @Test def caseIndependent1(): Unit = {
    val m = scala.collection.immutable.TreeMap[String, String]()(_ compareToIgnoreCase _)
    val r = m ++ Seq("a" -> "1", "A" -> "2")
    assertMapSameElements(Map("a" -> "2"), r)
  }

  @Test def caseIndependent2(): Unit = {
    val m = scala.collection.immutable.TreeMap[String, String]()(_ compareToIgnoreCase _)
    val r = Seq("a" -> "1", "A" -> "2").foldLeft (m) {
      case (acc, t) => acc + t
    }
    assertMapSameElements(Map("a" -> "2"), r)
  }

  // Strictly compare keys. Relates to a bug in Map.equals scala/bug#12228
  private def assertMapSameElements[K, V](expected: Map[K, V], map: Map[K, V]): Unit = {
    def withoutOrdering(m: Map[K, V]): Map[K, V] = collection.immutable.Map.apply(m.iterator.toSeq: _*)
    assertEquals(withoutOrdering(expected), withoutOrdering(map))
  }

  @Test def removeNonContent(): Unit = {
    val src: Map[Int, String] = TreeMap(Range(0, 100, 2).map((_, "")) :_*)
    for (i <- Range(-1, 101, 2)) {
      src - i
      assertSame(i.toString, src, nonAllocating(src - i, text = i.toString))
    }
  }
}
