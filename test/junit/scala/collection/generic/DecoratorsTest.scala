package scala.collection.generic

import org.junit.Test
import org.junit.Assert.{assertEquals, assertTrue}

import scala.AdaptedArrowAssocWorkaround.Tx
import scala.collection.immutable.{BitSet, IntMap, LongMap, TreeMap, TreeSet}
import scala.collection.{BuildFrom, View, mutable}
import scala.language.implicitConversions
import scala.tools.testkit.AssertUtil.assertSameElements

@annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
  @annotation.nowarn("cat=deprecation&origin=scala.collection.generic.IsMap.anyRefMapIsMap")
class DecoratorsTest {

  @Test
  def iterableOnceDecorator(): Unit = {
    implicit def filterMap[Repr](coll: Repr)(implicit it: IsIterableOnce[Repr]): FilterMapImpl[Repr, it.type] =
      new FilterMapImpl(coll, it)
    class FilterMapImpl[Repr, I <: IsIterableOnce[Repr]](coll: Repr, it: I) {
      final def filterMap[B, That](f: it.A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
        val b = bf.newBuilder(coll)
        for (e <- it(coll).iterator ; y <- f(e)) b += y
        b.result()
      }
    }

    val f = (i: Int) => if (i % 2 == 0) Some(i) else None

    val xs1 = Iterator(1, 2, 3, 4, 5)
    val xs2 = xs1.filterMap(f)
    val xs2T: Iterator[Int] = xs2
    assertSameElements(List(2, 4), xs2T)

    val xs3 = Array(1, 2, 3, 4, 5)
    val xs4 = xs3.filterMap(f)
    val xs4T: Array[Int] = xs4
    assertSameElements(List(2, 4), xs4T)
  }

  @Test
  def iterableDecorator(): Unit = {
    implicit def IterableDecorator[Repr](coll: Repr)(implicit it: IsIterable[Repr]): IterableDecorator[Repr, it.type] =
      new IterableDecorator(coll)(it)
    class IterableDecorator[Repr, I <: IsIterable[Repr]](coll: Repr)(implicit val it: I) {
      final def filterMap[B, That](f: it.A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That =
        bf.fromSpecific(coll)(it(coll).flatMap(f(_)))
    }

    val f: Int => Option[String] = x => if (x % 2 == 0) Some(x.toString) else None
    val g: Char => Option[Int] = c => if (c.isDigit) Some(c.asDigit) else None
    val h: ((Int, String)) => Option[(String, Int)] = {
      case (x, s) => if (s.length == x) Some((s, x)) else None
    }
    val i: Int => Option[Int] = x => if (x % 2 == 0) Some(2 * x) else None

    val xs1 = List(1, 2, 3).filterMap(f)
    val xs1T: List[String] = xs1
    assertSameElements(List("2"), xs1T)
    val xs2 = List(1, 2, 3).view.filterMap(f)
    val xs2T: View[String] = xs2
    assertSameElements(List("2"), xs2T)
    val xs3 = Vector(1, 2, 3).filterMap(f)
    val xs3T: Vector[String] = xs3
    assertSameElements(List("2"), xs3T)
    val xs4 = Vector(1, 2, 3).view.filterMap(f)
    val xs4T: View[String] = xs4
    assertSameElements(List("2"), xs4T)
    val xs5 = (1 to 10).filterMap(f)
    val xs5T: IndexedSeq[String] = xs5
    assertSameElements(List(2,4,6,8,10).map(_.toString), xs5T)
    val xs6 = (1 to 10).view.filterMap(f)
    val xs6T: View[String] = xs6
    assertSameElements(List(2,4,6,8,10).map(_.toString), xs6T)
    val xs7 = Array(1, 2, 3).filterMap(f)
    val xs7T: Array[String] = xs7
    assertSameElements(List("2"), xs7T)
    val xs8 = Array(1, 2, 3).view.filterMap(f)
    val xs8T: View[String] = xs8
    assertSameElements(List("2"), xs8T)
    val xs9 = "foo".filterMap(g)
    val xs9T: IndexedSeq[Int] = xs9
    assertTrue(xs9T.isEmpty)
    val xs10 = "foo".view.filterMap(g)
    val xs10T: View[Int] = xs10
    assertTrue(xs10T.isEmpty)
    val xs11 = Map(1 -> "foo").filterMap(h)
    val xs11T: Map[String, Int] = xs11
    assertTrue(xs11T.isEmpty)
    val xs12 = Map(1 -> "foo").view.filterMap(h)
    val xs12T: View[(String, Int)] = xs12
    assertTrue(xs12T.isEmpty)
    val xs13 = TreeMap(1 -> "foo").filterMap(h)
    val xs13T: TreeMap[String, Int] = xs13
    assertTrue(xs13T.isEmpty)
    val xs14 = TreeMap(1 -> "foo").view.filterMap(h)
    val xs14T: View[(String, Int)] = xs14
    assertTrue(xs14T.isEmpty)
    val xs15 = BitSet(1, 2, 3).filterMap(i)
    val xs15T: BitSet = xs15
    assertSameElements(List(4), xs15T)
    val xs16 = BitSet(1, 2, 3).view.filterMap(i)
    val xs16T: View[Int] = xs16
    assertSameElements(List(4), xs16T)
    val xs17 = Set(1, 2, 3).filterMap(f)
    val xs17T: Set[String] = xs17
    assertSameElements(List("2"), xs17T)
    val xs18 = Set(1, 2, 3).view.filterMap(f)
    val xs18T: View[String] = xs18
    assertSameElements(List("2"), xs18T)
    val xs19 = TreeSet(1, 2, 3).filterMap(f)
    val xs19T: TreeSet[String] = xs19
    assertSameElements(List("2"), xs19T)
    val xs20 = TreeSet(1, 2, 3).view.filterMap(f)
    val xs20T: View[String] = xs20
    assertSameElements(List("2"), xs20T)
    val xs21 = TreeSet(1, 2, 3).filterMap(f)
    val xs21T: TreeSet[String] = xs21
    assertSameElements(List("2"), xs21T)
  }

  @Test
  def seqDecorator(): Unit = {
    // Taken from https://github.com/scala/collection-strawman/pull/286
    implicit def SeqDecorator[Repr](coll: Repr)(implicit seq: IsSeq[Repr]): SeqDecorator[Repr, seq.type] =
      new SeqDecorator(coll)(seq)
    class SeqDecorator[Repr, S <: IsSeq[Repr]](coll: Repr)(implicit val seq: S) {
      def groupedWith[Group, That](p: seq.A => Boolean)(implicit
        group: BuildFrom[Repr, seq.A, Group],
        bf: BuildFrom[Repr, Group, That]
      ): That = {
        val `this` = seq(coll)

        val groups = bf.newBuilder(coll)
        val it = `this`.iterator

        var currentGroup = group.newBuilder(coll)
        var lastTestResult = Option.empty[Boolean]

        while (it.hasNext) {
          val elem = it.next()
          val currentTest = p(elem)

          lastTestResult match {
            case None =>
              currentGroup.addOne(elem)
            case Some(`currentTest`) =>
              currentGroup.addOne(elem)
            case Some(_) =>
              groups.addOne(currentGroup.result())
              currentGroup = group.newBuilder(coll).addOne(elem)
          }

          lastTestResult = Some(currentTest)
        }

        groups.addOne(currentGroup.result()).result()
      }
    }

    val p: Int => Boolean = _ % 2 == 0
    val xs = List(1, 2, 3).groupedWith(p)
    val xsT: List[List[Int]] = xs
    assertEquals(List(1, 2, 3).map(List(_)), xsT)
    val xs2 = List(1, 2, 3).view.groupedWith(p)
    val xs2T: View[View[Int]] = xs2
    assertSameElements(List(1, 2, 3).map(List(_)), xs2T.map(_.toList))
    val xs3 = Vector(1, 2, 3).groupedWith(p)
    val xs3T: Vector[Vector[Int]] = xs3
    assertEquals(Vector(1, 2, 3).map(Vector(_)), xs3T)
    val xs4 = Vector(1, 2, 3).view.groupedWith(p)
    val xs4T: View[View[Int]] = xs4
    assertSameElements(Vector(1, 2, 3).map(Vector(_)), xs4T.map(_.toVector))
    val xs5 = (1 to 10).groupedWith(p)
    val xs5T: IndexedSeq[IndexedSeq[Int]] = xs5
    assertSameElements((1 to 10).toVector.map(Vector(_)), xs5T.map(_.toVector))
    val xs6 = (1 to 10).view.groupedWith(p)
    val xs6T: View[View[Int]] = xs6
    assertSameElements((1 to 10).toVector.map(Vector(_)), xs6T.map(_.toVector))
    val xs7 = Array(1, 2, 3).groupedWith(p)
    val xs7T: Array[Array[Int]] = xs7
    assertSameElements(Vector(1, 2, 3).map(Vector(_)), xs7T.map(_.toVector))
    val xs8 = Array(1, 2, 3).view.groupedWith(p)
    val xs8T: View[View[Int]] = xs8
    assertSameElements(Vector(1, 2, 3).map(Vector(_)), xs8T.map(_.toVector))
    val xs9 = "foo".groupedWith(_.isLower)
    val xs9T: IndexedSeq[String] = xs9
    assertEquals(List("foo"), xs9T)
    val xs10 = "foo".view.groupedWith(_.isLower)
    val xs10T: View[View[Char]] = xs10
    assertEquals(Vector("foo".toVector), xs10T.toVector.map(_.toVector))
  }

  @Test
  def mapDecorator(): Unit = {

    implicit def MapDecorator[Repr](coll: Repr)(implicit map: IsMap[Repr]): MapDecorator[Repr, map.type] =
      new MapDecorator(coll)(map)

    class MapDecorator[C, M <: IsMap[C]](coll: C)(implicit val map: M) {
      def leftOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, Option[W])), That]): That = {
        val b = bf.newBuilder(coll)
        for ((k, v) <- map(coll)) {
          b += k -> Tx(v, other.get(k))
        }
        b.result()
      }
      def rightOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], W)), That]): That = {
        val b = bf.newBuilder(coll)
        for ((k, w) <- other) {
          b += k -> Tx(map(coll).get(k), w)
        }
        b.result()
      }
    }

    val map = Map(1 -> "foo")
    val mapLJoin = map.leftOuterJoin(Map(1 -> "bar"))
    val mapLJoinT: Map[Int, (String, Option[String])] = mapLJoin
    assertEquals(Map(1 -> ("foo" -> Some("bar"))), mapLJoinT)
    val mapRJoin = map.rightOuterJoin(Map(1 -> "bar"))
    val mapRJoinT: Map[Int, (Option[String], String)] = mapRJoin
    assertEquals(Map(1 -> (Some("foo") -> "bar")), mapRJoinT)

    val mapView = Map(1 -> "foo").view
    val mapViewLJoin = mapView.leftOuterJoin(Map(1 -> "bar"))
    val mapViewLJoinT: View[(Int, (String, Option[String]))] = mapViewLJoin
    assertSameElements(List(1 -> ("foo" -> Some("bar"))), mapViewLJoinT)
    val mapViewRJoin = mapView.rightOuterJoin(Map(1 -> "bar"))
    val mapViewRJoinT: View[(Int, (Option[String], String))] = mapViewRJoin
    assertSameElements(List(1 -> (Some("foo") -> "bar")), mapViewRJoinT)

    val treemap = TreeMap(1 -> "foo")
    val treemapLJoin = treemap.leftOuterJoin(Map(1 -> "bar"))
    val treemapLJoinT: TreeMap[Int, (String, Option[String])] = treemapLJoin
    assertEquals(Map(1 -> ("foo" -> Some("bar"))), treemapLJoinT)
    val treemapRJoin = treemap.rightOuterJoin(Map(1 -> "bar"))
    val treemapRJoinT: TreeMap[Int, (Option[String], String)] = treemapRJoin
    assertEquals(Map(1 -> (Some("foo") -> "bar")), treemapRJoinT)

    val treemapView = TreeMap(1 -> "foo").view
    val treemapViewLJoin = treemapView.leftOuterJoin(Map(1 -> "bar"))
    val treemapViewLJoinT: View[(Int, (String, Option[String]))] = treemapViewLJoin
    assertSameElements(List(1 -> ("foo" -> Some("bar"))), treemapViewLJoinT)
    val treemapViewRJoin = treemapView.rightOuterJoin(Map(1 -> "bar"))
    val treemapViewRJoinT: View[(Int, (Option[String], String))] = treemapViewRJoin
    assertSameElements(List(1 -> (Some("foo") -> "bar")), treemapViewRJoinT)

    val mmap = mutable.Map(1 -> "foo")
    val mmapLJoin = mmap.leftOuterJoin(Map(1 -> "bar"))
    val mmapLJoinT: mutable.Map[Int, (String, Option[String])] = mmapLJoin
    assertEquals(Map(1 -> ("foo" -> Some("bar"))), mmapLJoinT)
    val mmapRJoin = mmap.rightOuterJoin(Map(1 -> "bar"))
    val mmapRJoinT: mutable.Map[Int, (Option[String], String)] = mmapRJoin
    assertEquals(Map(1 -> (Some("foo") -> "bar")), mmapRJoinT)

    val mmapView = mutable.Map(1 -> "foo").view
    val mmapViewLJoin = mmapView.leftOuterJoin(Map(1 -> "bar"))
    val mmapViewLJoinT: View[(Int, (String, Option[String]))] = mmapViewLJoin
    assertSameElements(List(1 -> ("foo" -> Some("bar"))), mmapViewLJoinT)
    val mmapViewRJoin = mmapView.rightOuterJoin(Map(1 -> "bar"))
    val mmapViewRJoinT: View[(Int, (Option[String], String))] = mmapViewRJoin
    assertSameElements(List(1 -> (Some("foo") -> "bar")), mmapViewRJoinT)

    val anyrefmap = mutable.AnyRefMap("foo" -> 1)
    val anyrefmapLJoin = anyrefmap.leftOuterJoin(Map("bar" -> true))
    val anyrefmapLJoinT: mutable.AnyRefMap[String, (Int, Option[Boolean])] = anyrefmapLJoin
    assertEquals(Map("foo" -> (1 -> None)), anyrefmapLJoinT)
    val anyrefmapRJoin = anyrefmap.rightOuterJoin(Map("bar" -> true))
    val anyrefmapRJoinT: mutable.AnyRefMap[String, (Option[Int], Boolean)] = anyrefmapRJoin
    assertEquals(Map("bar" -> (None -> true)), anyrefmapRJoinT)

    val intmap = IntMap(1 -> "foo")
    val intmapLJoin = intmap.leftOuterJoin(Map(1 -> "bar"))
    val intmapLJoinT: IntMap[(String, Option[String])] = intmapLJoin
    assertEquals(Map(1 -> ("foo" -> Some("bar"))), intmapLJoinT)
    val intmapRJoin = intmap.rightOuterJoin(Map(1 -> "bar"))
    val intmapRJoinT: IntMap[(Option[String], String)] = intmapRJoin
    assertEquals(Map(1 -> (Some("foo") -> "bar")), intmapRJoinT)

    val longmap = LongMap(1L -> "foo")
    val longmapLJoin = longmap.leftOuterJoin(Map(1L -> "bar"))
    val longmapLJoinT: LongMap[(String, Option[String])] = longmapLJoin
    assertEquals(Map(1L -> ("foo" -> Some("bar"))), longmapLJoinT)
    val longmapRJoin = longmap.rightOuterJoin(Map(1L -> "bar"))
    val longmapRJoinT: LongMap[(Option[String], String)] = longmapRJoin
    assertEquals(Map(1L -> (Some("foo") -> "bar")), longmapRJoinT)

    val mlongmap = mutable.LongMap(1L -> "foo")
    val mlongmapLJoin = mlongmap.leftOuterJoin(Map(1L -> "bar"))
    val mlongmapLJoinT: mutable.LongMap[(String, Option[String])] = mlongmapLJoin
    assertEquals(Map(1L -> ("foo" -> Some("bar"))), mlongmapLJoinT)
    val mlongmapRJoin = mlongmap.rightOuterJoin(Map(1L -> "bar"))
    val mlongmapRJoinT: mutable.LongMap[(Option[String], String)] = mlongmapRJoin
    assertEquals(Map(1L -> (Some("foo") -> "bar")), mlongmapRJoinT)
  }

}
