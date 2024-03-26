package scala.collection

import org.junit.Assert._
import org.junit.Test

import scala.tools.testkit.AssertUtil.{assertFails, assertNotReachable}

class MapTest {
  @deprecated("Tests deprecated API", since="2.13")
  @Test def test(): Unit = {
    val map = collection.Map(
      1 -> 1,
      2 -> 2,
      4 -> 4,
      5 -> 5
    )

    val actual = map -- List(1, 2, 3)

    val expected = collection.Map(
      4 -> 4,
      5 -> 5
    )

    assertEquals(expected, actual)
  }

  @Test def mkString(): Unit = {
    assert(Map().mkString == "")
    assert(Map(1 -> 1).mkString(",") == "1 -> 1")
    assert(Map(1 -> 1, 2 -> 2).mkString(",") == "1 -> 1,2 -> 2")
  }

  @Test def addString(): Unit = {
    assert(Map().addString(new StringBuilder).toString == "")
    assert(Map(1 -> 1).addString(new StringBuilder).toString == "1 -> 1")
    assert(Map(1 -> 1, 2 -> 2).mkString("foo [", ", ", "] bar").toString ==
      "foo [1 -> 1, 2 -> 2] bar")
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test def t11188(): Unit = {
    import scala.collection.immutable.ListMap
    val m = ListMap(1 -> "one")
    val mm = Map(2 -> "two") ++: m
    assert(mm.isInstanceOf[ListMap[Int,String]])
    assertEquals(mm.mkString("[", ", ", "]"), "[2 -> two, 1 -> one]")
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test def deprecatedPPE(): Unit = {
    val m = (1 to 10).map(x => (x, x)).toMap
    val m1 = m ++: m
    assertEquals(m.toList.sorted, (m1: Map[Int, Int]).toList.sorted)
    val s1: Iterable[Any] = List(1) ++: m
    assertEquals(::[Any](1, m.toList.sorted), s1.toList.sortBy { case (x: Int, _) => x ; case x: Int => x })
  }

  @Test
  def flatMapOption(): Unit = {
    def f(p: (Int, Int)) = if (p._1 < p._2) Some((p._1, p._2)) else None
    val m = (1 to 10).zip(11 to 20).toMap
    val m2 = m.flatMap(f)
    (m2: Map[Int, Int]).head
    val m3 = m.flatMap(p => Some(p))
    (m3: Map[Int, Int]).head
    val m4 = m.flatMap(_ => Some(3))
    (m4: Iterable[Int]).head
  }

  @deprecated("Tests deprecated API", since="2.13")
  @Test
  def t11589(): Unit = {
    // tests the strictness of Map#values

    def check(m: collection.Map[Int, Int]): Unit = {
      def checkImmutable[K, V](m: immutable.Map[Int, Int]): Unit = {
        var i = 0
        m.withDefault(_ => -1).values.map{v => i = 1; v}
        assertEquals(1, i)
        i = 0
        m.withDefaultValue(-1).values.map{v => i = 1; v}
        assertEquals(1, i)
      }
      var i = 0
      m.values.map{v => i = 1; v}
      assertEquals(1, i)

      m match {
        case im: immutable.Map[Int, Int] =>
          checkImmutable(im)
        case _ =>
          ()
      }
    }



    check(collection.Map(1 -> 1))
    check(immutable.Map(1 -> 1))
    check(mutable.Map(1 -> 1))

    check(collection.SortedMap(1 -> 1))
    check(immutable.SortedMap(1 -> 1))
    check(mutable.SortedMap(1 -> 1))

    check(immutable.HashMap(1 -> 1))
    check(mutable.HashMap(1 -> 1))

    check(immutable.TreeMap(1 -> 1))
    check(mutable.TreeMap(1 -> 1))

    check(immutable.SeqMap(1 -> 1))
    check(mutable.SeqMap(1 -> 1))

    check(immutable.ListMap(1 -> 1))
    check(mutable.ListMap(1 -> 1))

    check(immutable.VectorMap(1 -> 1))
    check(immutable.TreeSeqMap(1 -> 1))

    check(mutable.LinkedHashMap(1 -> 1))

    check(mutable.OpenHashMap(1 -> 1))
    check(mutable.CollisionProofHashMap(1 -> 1))
  }

  @Test
  def t12228(): Unit = {
    assertFalse(Set("") == immutable.BitSet(1))
    assertFalse(Map("" -> 2) == scala.collection.immutable.LongMap(1L -> 2))
  }

  @Test def `map is reachable from its keyset`: Unit =
    assertFails(_.contains("held reference")) {
      val m = Map("one" -> "eins")
      assertNotReachable(m, m.keySet)(())
    }
}
