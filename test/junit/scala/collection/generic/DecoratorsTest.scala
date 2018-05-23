package scala.collection.generic

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.immutable.{BitSet, IntMap, LongMap, TreeMap, TreeSet}
import scala.collection.{BuildFrom, View, mutable}
import scala.language.implicitConversions

@RunWith(classOf[JUnit4])
class DecoratorsTest {

  @Test
  def iterableOnceDecorator: Unit = {
    implicit def filterMap[Repr](coll: Repr)(implicit it: IsIterableOnce[Repr]): FilterMapImpl[Repr, it.type] =
      new FilterMapImpl(coll, it)
    class FilterMapImpl[Repr, I <: IsIterableOnce[Repr]](coll: Repr, it: I) {
      final def filterMap[B, That](f: it.A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
        val b = bf.newBuilder(coll)
        for (e <- it(coll)) f(e) foreach (b +=)
        b.result()
      }
    }

    val f = (i: Int) => if (i % 2 == 0) Some(i) else None

    val xs1 = Iterator(1, 2, 3, 4, 5)
    val xs2 = xs1.filterMap(f)
    val xs2T: Iterator[Int] = xs2

    val xs3 = Array(1, 2, 3, 4, 5)
    val xs4 = xs3.filterMap(f)
    val xs4T: Array[Int] = xs4
  }

  @Test
  def iterableDecorator: Unit = {
    implicit def IterableDecorator[Repr](coll: Repr)(implicit it: IsIterableLike[Repr]): IterableDecorator[Repr, it.type] =
      new IterableDecorator(coll)(it)
    class IterableDecorator[Repr, I <: IsIterableLike[Repr]](coll: Repr)(implicit val it: I) {
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
    val xs2 = List(1, 2, 3).view.filterMap(f)
    val xs2T: View[String] = xs2
    val xs3 = Vector(1, 2, 3).filterMap(f)
    val xs3T: Vector[String] = xs3
    val xs4 = Vector(1, 2, 3).view.filterMap(f)
    val xs4T: View[String] = xs4
    val xs5 = (1 to 10).filterMap(f)
    val xs5T: IndexedSeq[String] = xs5
    val xs6 = (1 to 10).view.filterMap(f)
    val xs6T: View[String] = xs6
    val xs7 = Array(1, 2, 3).filterMap(f)
    val xs7T: Array[String] = xs7
    val xs8 = Array(1, 2, 3).view.filterMap(f)
    val xs8T: View[String] = xs8
    val xs9 = "foo".filterMap(g)
    val xs9T: IndexedSeq[Int] = xs9
    val xs10 = "foo".view.filterMap(g)
    val xs10T: View[Int] = xs10
    val xs11 = Map(1 -> "foo").filterMap(h)
    val xs11T: Map[String, Int] = xs11
    val xs12 = Map(1 -> "foo").view.filterMap(h)
    val xs12T: View[(String, Int)] = xs12
    val xs13 = TreeMap(1 -> "foo").filterMap(h)
    val xs13T: TreeMap[String, Int] = xs13
    val xs14 = TreeMap(1 -> "foo").view.filterMap(h)
    val xs14T: View[(String, Int)] = xs14
    val xs15 = BitSet(1, 2, 3).filterMap(i)
    val xs15T: BitSet = xs15
    val xs16 = BitSet(1, 2, 3).view.filterMap(i)
    val xs16T: View[Int] = xs16
    val xs17 = Set(1, 2, 3).filterMap(f)
    val xs17T: Set[String] = xs17
    val xs18 = Set(1, 2, 3).view.filterMap(f)
    val xs18T: View[String] = xs18
    val xs19 = TreeSet(1, 2, 3).filterMap(f)
    val xs19T: TreeSet[String] = xs19
    val xs20 = TreeSet(1, 2, 3).view.filterMap(f)
    val xs20T: View[String] = xs20
    val xs21 = TreeSet(1, 2, 3).filterMap(f)
    val xs21T: TreeSet[String] = xs21
  }

  @Test
  def seqDecorator: Unit = {
    // Taken from https://github.com/scala/collection-strawman/pull/286
    implicit def SeqDecorator[Repr](coll: Repr)(implicit seq: IsSeqLike[Repr]): SeqDecorator[Repr, seq.type] =
      new SeqDecorator(coll)(seq)
    class SeqDecorator[Repr, S <: IsSeqLike[Repr]](coll: Repr)(implicit val seq: S) {
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
            case Some(lastTest) if currentTest == lastTest =>
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
    val xs2 = List(1, 2, 3).view.groupedWith(p)
    val xs2T: View[View[Int]] = xs2
    val xs3 = Vector(1, 2, 3).groupedWith(p)
    val xs3T: Vector[Vector[Int]] = xs3
    val xs4 = Vector(1, 2, 3).view.groupedWith(p)
    val xs4T: View[View[Int]] = xs4
    val xs5 = (1 to 10).groupedWith(p)
    val xs5T: IndexedSeq[IndexedSeq[Int]] = xs5
    val xs6 = (1 to 10).view.groupedWith(p)
    val xs6T: View[View[Int]] = xs6
    val xs7 = Array(1, 2, 3).groupedWith(p)
    val xs7T: Array[Array[Int]] = xs7
    val xs8 = Array(1, 2, 3).view.groupedWith(p)
    val xs8T: View[View[Int]] = xs8
    val xs9 = "foo".groupedWith(_.isLower)
    val xs9T: IndexedSeq[String] = xs9
    val xs10 = "foo".view.groupedWith(_.isLower)
    val xs10T: View[View[Char]] = xs10
  }

  @Test
  def mapDecorator: Unit = {

    implicit def MapDecorator[Repr](coll: Repr)(implicit map: IsMapLike[Repr]): MapDecorator[Repr, map.type] =
      new MapDecorator(coll)(map)

    class MapDecorator[C, M <: IsMapLike[C]](coll: C)(implicit val map: M) {
      def leftOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (map.V, Option[W])), That]): That = {
        val b = bf.newBuilder(coll)
        for ((k, v) <- map(coll)) {
          b += k -> (v, other.get(k))
        }
        b.result()
      }
      def rightOuterJoin[W, That](other: Map[map.K, W])(implicit bf: BuildFrom[C, (map.K, (Option[map.V], W)), That]): That = {
        val b = bf.newBuilder(coll)
        for ((k, w) <- other) {
          b += k -> (map(coll).get(k), w)
        }
        b.result()
      }
    }

    val map = Map(1 -> "foo")
    val mapLJoin = map.leftOuterJoin(Map(1 -> "bar"))
    val mapLJoinT: Map[Int, (String, Option[String])] = mapLJoin
    val mapRJoin = map.rightOuterJoin(Map(1 -> "bar"))
    val mapRJoinT: Map[Int, (Option[String], String)] = mapRJoin

    val mapView = Map(1 -> "foo").view
    val mapViewLJoin = mapView.leftOuterJoin(Map(1 -> "bar"))
    val mapViewLJoinT: View[(Int, (String, Option[String]))] = mapViewLJoin
    val mapViewRJoin = mapView.rightOuterJoin(Map(1 -> "bar"))
    val mapViewRJoinT: View[(Int, (Option[String], String))] = mapViewRJoin

    val treemap = TreeMap(1 -> "foo")
    val treemapLJoin = treemap.leftOuterJoin(Map(1 -> "bar"))
    val treemapLJoinT: TreeMap[Int, (String, Option[String])] = treemapLJoin
    val treemapRJoin = treemap.rightOuterJoin(Map(1 -> "bar"))
    val treemapRJoinT: TreeMap[Int, (Option[String], String)] = treemapRJoin

    val treemapView = TreeMap(1 -> "foo").view
    val treemapViewLJoin = treemapView.leftOuterJoin(Map(1 -> "bar"))
    val treemapViewLJoinT: View[(Int, (String, Option[String]))] = treemapViewLJoin
    val treemapViewRJoin = treemapView.rightOuterJoin(Map(1 -> "bar"))
    val treemapViewRJoinT: View[(Int, (Option[String], String))] = treemapViewRJoin

    val mmap = mutable.Map(1 -> "foo")
    val mmapLJoin = mmap.leftOuterJoin(Map(1 -> "bar"))
    val mmapLJoinT: mutable.Map[Int, (String, Option[String])] = mmapLJoin
    val mmapRJoin = mmap.rightOuterJoin(Map(1 -> "bar"))
    val mmapRJoinT: mutable.Map[Int, (Option[String], String)] = mmapRJoin

    val mmapView = mutable.Map(1 -> "foo").view
    val mmapViewLJoin = mmapView.leftOuterJoin(Map(1 -> "bar"))
    val mmapViewLJoinT: View[(Int, (String, Option[String]))] = mmapViewLJoin
    val mmapViewRJoin = mmapView.rightOuterJoin(Map(1 -> "bar"))
    val mmapViewRJoinT: View[(Int, (Option[String], String))] = mmapViewRJoin

    val anyrefmap = mutable.AnyRefMap("foo" -> 1)
    val anyrefmapLJoin = anyrefmap.leftOuterJoin(Map("bar" -> true))
    val anyrefmapLJoinT: mutable.AnyRefMap[String, (Int, Option[Boolean])] = anyrefmapLJoin
    val anyrefmapRJoin = anyrefmap.rightOuterJoin(Map("bar" -> true))
    val anyrefmapRJoinT: mutable.AnyRefMap[String, (Option[Int], Boolean)] = anyrefmapRJoin

    val intmap = IntMap(1 -> "foo")
    val intmapLJoin = intmap.leftOuterJoin(Map(1 -> "bar"))
    val intmapLJoinT: IntMap[(String, Option[String])] = intmapLJoin
    val intmapRJoin = intmap.rightOuterJoin(Map(1 -> "bar"))
    val intmapRJoinT: IntMap[(Option[String], String)] = intmapRJoin

    val longmap = LongMap(1L -> "foo")
    val longmapLJoin = longmap.leftOuterJoin(Map(1L -> "bar"))
    val longmapLJoinT: LongMap[(String, Option[String])] = longmapLJoin
    val longmapRJoin = longmap.rightOuterJoin(Map(1L -> "bar"))
    val longmapRJoinT: LongMap[(Option[String], String)] = longmapRJoin

    val mlongmap = mutable.LongMap(1L -> "foo")
    val mlongmapLJoin = mlongmap.leftOuterJoin(Map(1L -> "bar"))
    val mlongmapLJoinT: mutable.LongMap[(String, Option[String])] = mlongmapLJoin
    val mlongmapRJoin = mlongmap.rightOuterJoin(Map(1L -> "bar"))
    val mlongmapRJoinT: mutable.LongMap[(Option[String], String)] = mlongmapRJoin
  }

}
