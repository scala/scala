package strawman
package collection.test

import java.lang.String

import scala.{Any, Array, Boolean, Char, Either, Int, Left, Nothing, Option, StringContext, Unit}
import scala.Predef.{assert, charWrapper, identity, println}
import collection._
import collection.immutable.{ImmutableArray, LazyList, List, Nil, Range, Vector}
import collection.mutable.{ArrayBuffer, ListBuffer}
import org.junit.Test

class StrawmanTest {

  def iterableOps(xs: Iterable[Int]): Unit = {
    val x1 = xs.map(x => (x, x))
    val (x2, x3) = x1.unzip
    assert(xs == x2 && xs == x3)
  }

  def seqOps(xs: Seq[Int]): Unit = {
    iterableOps(xs)
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x3 = xs.indexWhere(_ % 2 == 0)
    val y3: Int = x3
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Int] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: Seq[Int] = xs6
    val ys7: Seq[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: Seq[Int] = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: Seq[Int] = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: Seq[Int] = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: Seq[Int] = xs8_4
    val xs9 = xs.map(_ >= 0)
    val ys9: Seq[Boolean] = xs9
    val xs10 = xs.flatMap(x => x :: -x :: Nil)
    val ys10: Seq[Int] = xs10
    val xs11 = xs ++ xs
    val ys11: Seq[Int] = xs11
    val xs12 = xs ++ Nil
    val ys12: Seq[Int] = xs12
    val xs13 = Nil ++ xs
    val ys13: Seq[Int] = xs13
    val xs14 = xs ++ ("a" :: Nil)
    val ys14: Seq[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: Seq[(Int, Boolean)] = xs15
    val xs16 = xs.reverse
    val ys16: Seq[Int] = xs16
    val xs17 = xs.init
    val ys17: Seq[Int] = xs17
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs7)
    println(xs8)
    println(xs8_2)
    println(xs8_3)
    println(xs8_4)
    println(xs9)
    println(xs10)
    println(xs11)
    println(xs12)
    println(xs13)
    println(xs14)
    println(xs15)
    println(xs16)
    println(xs17)
  }

  def viewOps(xs: View[Int]): Unit = {
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Int] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: View[Int] = xs6
    val ys7: View[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: View[Int] = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: View[Int] = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: View[Int] = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: View[Int] = xs8_4
    val xs9 = xs.map(_ >= 0)
    val ys9: View[Boolean] = xs9
    val xs10 = xs.flatMap(x => x :: -x :: Nil)
    val ys10: View[Int] = xs10
    val xs11 = xs ++ xs
    val ys11: View[Int] = xs11
    val xs12 = xs ++ Nil
    val ys12: View[Int] = xs12
    val xs13 = Nil ++ xs
    val ys13: List[Int] = xs13
    val xs14 = xs ++ ("a" :: Nil)
    val ys14: View[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: View[(Int, Boolean)] = xs15
    val xs17 = xs.init
    val ys17: View[Int] = xs17
    println("-------")
    println(x1)
    println(x2)
    println(x4)
    println(x5)
    println(xs6.to(List))
    println(xs7.to(List))
    println(xs8.to(List))
    println(xs8_2.to(List))
    println(xs8_3.to(List))
    println(xs8_4.to(List))
    println(xs9.to(List))
    println(xs10.to(List))
    println(xs11.to(List))
    println(xs12.to(List))
    println(xs13.to(List))
    println(xs14.to(List))
    println(xs15.to(List))
    println(xs17.to(List))
  }

  def stringOps(xs: String): Unit = {
    iterableOps(xs.map(_.intValue))
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x3 = xs.indexWhere(_ % 2 == 0)
    val y3: Int = x3
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Char] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: String = xs6
    val ys7: String = xs7
    val xs8 = xs.drop(2)
    val ys8: String = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: String = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: String = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: String = xs8_4
    val xs9 = xs.map(_ + 1)
    val ys9: Seq[Int] = xs9
    val xs9a = xs.map(_.toUpper)
    val ys9a: String = xs9a
    val xs10 = xs.flatMap(x => s"$x,$x")
    val ys10: String = xs10
    val xs11 = xs ++ xs
    val ys11: String = xs11
    val xs11a = xs ++ List('x', 'y')
    val ys11a: String = xs11a
    val xs12 = xs ++ Nil
    val ys12: String = xs12
    val xs13 = Nil ++ xs
    val ys13: List[Char] = xs13
    val xs14 = xs ++ List("xyz")
    val ys14: Seq[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: Seq[(Char, Int)] = xs15
    val xs17 = xs.init
    val ys17: String = xs17
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs7)
    println(xs8)
    println(xs8_2)
    println(xs8_3)
    println(xs8_4)
    println(xs9)
    println(xs9a)
    println(xs10)
    println(xs11)
    println(xs11a)
    println(xs12)
    println(xs13)
    println(xs14)
    println(xs15)
    println(xs17)
  }

  def arrayOps(xs: Array[Int]): Unit = {
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x3 = xs.indexWhere(_ % 2 == 0)
    val y3: Int = x3
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Int] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: Array[Int] = xs6
    val ys7: Array[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: Array[Int] = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: Array[Int] = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: Array[Int] = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: Array[Int] = xs8_4
    val xs9 = arrayToArrayOps(xs).map(_ >= 0)
    val ys9: Array[Boolean] = xs9
    val xs10 = xs.flatMap(x => List(x, -x))
    val ys10: Array[Int] = xs10
    val xs11 = xs ++ xs
    val ys11: Array[Int] = xs11
    val xs12 = arrayToArrayOps(xs) ++ Nil
    val ys12: Array[Int] = xs12
    val xs13 = Nil ++ xs
    val ys13: List[Int] = xs13
    val xs14 = xs ++ (("a": Any) :: Nil)
    val ys14: Array[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: Array[(Int, Boolean)] = xs15
    val xs16 = xs.reverse
    val ys16: Array[Int] = xs16
    val xs17 = xs.init
    val ys17: Array[Int] = xs17
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6.view)
    println(xs7.view)
    println(xs8.view)
    println(xs8_2.view)
    println(xs8_3.view)
    println(xs8_4.view)
    println(xs9.view)
    println(xs10.view)
    println(xs11.view)
    println(xs12.view)
    println(xs13)
    println(xs14.view)
    println(xs15.view)
    println(xs16.view)
    println(xs17.view)
  }

  def immutableSeqOps(xs: immutable.Seq[Int]): Unit = {
    val xs1 = xs :+ 42
    assert(xs1 == (xs ++ immutable.Seq(42)))
    val xs2 = 42 +: xs
    assert(xs2 == (immutable.Seq(42) ++ xs))
    val xs3 = xs.updated(1, 42)
    assert(xs3(1) == 42)
    val xs4 = xs ++: xs
    val ys4: Seq[Int] = xs4
    val xs5 = xs ++: Nil
    val ys5: Seq[Int] = xs5
    val xs6 = Nil ++: xs
    val ys6: Seq[Int] = xs6
    val xs7 = xs ++: ("a" :: Nil)
    val ys7: Seq[Any] = xs7
    val xs8 = xs.patch(0, Nil, 0)
    val ys8: Seq[Int] = xs8
    val xs9 = xs.patch(0, List(1, 2, 3), 2)
    val ys9: Seq[Int] = xs9
    val xs10 = xs.patch(xs.length, List(1, 2, 3), 0)
    val ys10: Seq[Int] = xs10
    val x11 +: xs11 = xs
    val y11: Int = x11
    val ys11: Seq[Int] = xs11
    val xs12 :+ x12 = xs
    val y12: Int = x12
    val ys12: Seq[Int] = xs12
  }

  def immutableArrayOps(xs: immutable.ImmutableArray[Int]): Unit = {
    iterableOps(xs)
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x3 = xs.indexWhere(_ % 2 == 0)
    val y3: Int = x3
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Int] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: immutable.ImmutableArray[Int] = xs6
    val ys7: immutable.ImmutableArray[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: immutable.ImmutableArray[Int] = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: immutable.ImmutableArray[Int] = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: immutable.ImmutableArray[Int] = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: immutable.ImmutableArray[Int] = xs8_4
    val xs9 = xs.map(_ >= 0)
    val ys9: immutable.ImmutableArray[Boolean] = xs9
    val xs10 = xs.flatMap(x => x :: -x :: Nil)
    val ys10: immutable.ImmutableArray[Int] = xs10
    val xs11 = xs ++ xs
    val ys11: immutable.ImmutableArray[Int] = xs11
    val xs12 = xs ++ Nil
    val ys12: immutable.ImmutableArray[Int] = xs12
    val xs13 = Nil ++ xs
    val ys13: List[Int] = xs13
    val xs14 = xs ++ ("a" :: Nil)
    val ys14: immutable.ImmutableArray[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: immutable.ImmutableArray[(Int, Boolean)] = xs15
    val xs16 = xs.reverse
    val ys16: immutable.ImmutableArray[Int] = xs16
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs7)
    println(xs8)
    println(xs8_2)
    println(xs8_3)
    println(xs8_4)
    println(xs9)
    println(xs10)
    println(xs11)
    println(xs12)
    println(xs13)
    println(xs14)
    println(xs15)
    println(xs16)
  }

  def lazyListOps(xs: Seq[Int]): Unit = {
    val x1 = xs.foldLeft("")(_ + _)
    val y1: String = x1
    val x2 = xs.foldRight("")(_ + _)
    val y2: String = x2
    val x3 = xs.indexWhere(_ % 2 == 0)
    val y3: Int = x3
    val x4 = xs.head
    val y4: Int = x4
    val x5 = xs.to(List)
    val y5: List[Int] = x5
    val (xs6, xs7) = xs.partition(_ % 2 == 0)
    val ys6: Seq[Int] = xs6
    val ys7: Seq[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: Seq[Int] = xs8
    val xs8_2 = xs.dropRight(2)
    val ys8_2: Seq[Int] = xs8_2
    val xs8_3 = xs.take(2)
    val ys8_3: Seq[Int] = xs8_3
    val xs8_4 = xs.takeRight(2)
    val ys8_4: Seq[Int] = xs8_4
    val xs9 = xs.map(_ >= 0)
    val ys9: Seq[Boolean] = xs9
    val xs10 = xs.flatMap(x => x :: -x :: Nil)
    val ys10: Seq[Int] = xs10
    val xs11 = xs ++ xs
    val ys11: Seq[Int] = xs11
    val xs12 = xs ++ Nil
    val ys12: Seq[Int] = xs12
    val xs13 = Nil ++ xs
    val ys13: Seq[Int] = xs13
    val xs14 = xs ++ ("a" :: Nil)
    val ys14: Seq[Any] = xs14
    val xs15 = xs.zip(xs9)
    val ys15: Seq[(Int, Boolean)] = xs15
    val xs16 = xs.reverse
    val ys16: Seq[Int] = xs16
    val ys17 = LazyList.empty ++ LazyList.empty
    val ys18: LazyList[Nothing] = ys17
    val xs19 = xs.init
    val ys19: Seq[Int] = xs19
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs6.to(List))
    println(xs7)
    println(xs7.to(List))
    println(xs8)
    println(xs8.to(List))
    println(xs8_2)
    println(xs8_2.to(List))
    println(xs8_3)
    println(xs8_3.to(List))
    println(xs8_4)
    println(xs8_4.to(List))
    println(xs9)
    println(xs9.to(List))
    println(xs10)
    println(xs10.to(List))
    println(xs11)
    println(xs11.to(List))
    println(xs12)
    println(xs12.to(List))
    println(xs13)
    println(xs13.to(List))
    println(xs14)
    println(xs14.to(List))
    println(xs15)
    println(xs15.to(List))
    println(xs16)
    println(xs16.to(List))

    import LazyList.#::
    val xs17 = 1 #:: 2 #:: 3 #:: LazyList.Empty
    println(xs17)
    xs17 match {
      case a #:: b #:: xs18 =>
        println(s"matched: $a, $b, $xs18")
      case _ =>
    }
    println(xs17)
    println(xs17.to(List))
    println(xs19)
    println(xs19.to(List))

    // laziness may differ in dotty, so test only that we are as lazy as Stream
    import scala.collection.{immutable => old}
    lazy val fibsStream: old.Stream[Int] = 0 #:: 1 #:: fibsStream.zip(fibsStream.tail).map { n => n._1 + n._2 }
    if(old.List(0,1,1,2)==fibsStream.take(4).toList) {
      lazy val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
      assert(List(0, 1, 1, 2) == fibs.take(4).to(List))
    }

    var lazeCountS = 0
    var lazeCountL = 0
    def lazeL(i: Int) = {lazeCountL += 1; i}
    def lazeS(i: Int) = {lazeCountS += 1; i}
    val xs20 = lazeS(1) #:: lazeS(2) #:: lazeS(3) #:: old.Stream.empty
    val xs21 = lazeL(1) #:: lazeL(2) #:: lazeL(3) #:: LazyList.empty
    assert(lazeCountS==lazeCountL)
  }

  def sortedSets(xs: immutable.SortedSet[Int]): Unit = {
    iterableOps(xs)
    val xs1 = xs.map((x: Int) => x.toString) // TODO Remove type annotation when https://github.com/scala/scala/pull/5708 is published
    val xs2: immutable.SortedSet[String] = xs1
    val l = List(1,2,3)
    val s1 = l.to(immutable.TreeSet)
    val s1t: immutable.TreeSet[Int] = s1
    val m1 = s1.map(x => x.toString)
    val m1t: immutable.TreeSet[String] = m1
   }

  def mapOps(xs: Map[Int, String]): Unit = {
    val xs1 = xs.map ({ case (k, v) => (v, k) }: scala.PartialFunction[(Int, String), (String, Int)])
    val xs2: strawman.collection.Map[String, Int] = xs1
    val xs3 = xs.map(kv => (kv._2, kv._1))
    val xs4: strawman.collection.Iterable[(String, Int)] = xs3
    println(xs1)
    println(xs2)
    println(xs3)
    println(xs4)
  }

  def sortedMaps(xs: immutable.SortedMap[String, Int]): Unit = {
    val x1 = xs.get("foo")
    val x2: Option[Int] = x1
    val xs1 = xs + ("foo", 1)
    val xs2: immutable.SortedMap[String, Int] = xs1
    val xs3 = xs.map(kv => kv._1)
    // val xs4: immutable.Iterable[String] = xs3  // FIXME: does not work under dotty, we get a collection.Iterable
    val xs5 = xs.map ({ case (k, v) => (v, k) }: scala.PartialFunction[(String, Int), (Int, String)])
    val xs6: immutable.SortedMap[Int, String] = xs5
    class Foo
//    val xs7 = xs.map((k: String, v: Int) => (new Foo, v)) Error: No implicit Ordering defined for Foo
    val xs7 = (xs: immutable.Map[String, Int]) map ({ case (k, v) => (new Foo, v) }: scala.PartialFunction[(String, Int), (Foo, Int)])
    val xs8: immutable.Map[Foo, Int] = xs7
    val xs9 = xs6.to(List).to(mutable.HashMap)
    val xs9t: mutable.HashMap[Int, String] = xs9
    val xs10 = xs1 ++ xs2
    val xs11: immutable.SortedMap[String, Int] = xs10
    val xs12 = xs11.collect({ case (k, v) if k.length == v => (v, k) }: scala.PartialFunction[(String, Int), (Int, String)])
    val xs13: immutable.SortedMap[Int, String] = xs12
  }

  def bitSets(xs: immutable.BitSet, ys: BitSet, zs: Set[Int]): Unit = {
    iterableOps(xs)
    val xs1 = xs & zs
    val xs2: immutable.BitSet = xs1
    val xs3 = xs ^ ys
    val xs4: immutable.BitSet = xs3
    val b = xs.subsetOf(zs)
    val xs5 = xs.map(x => x + 1)
    val xs6: immutable.BitSet = xs5
    val xs7 = (xs: immutable.SortedSet[Int]).map((x: Int) => x.toString)
    val xs8: immutable.SortedSet[String] = xs7
    val xs9 = (xs: immutable.Set[Int]).map(x => Left(x): Either[Int, Nothing])
    //val xs10: immutable.Set[Either[Int, Nothing]] = xs9
    val xs11 = xs + 42
    val xs12: immutable.BitSet = xs11
    val l = List(1, 2, 3)
    val bs1 = l.to(immutable.BitSet)
    val bs1t: immutable.BitSet = bs1
  }

  @Test
  def distinct(): Unit = {
    // Lazy collections distinct
    assert(LazyList[Int]().distinct.isEmpty)
    assert(LazyList(1,1,1,1).distinct.equals(LazyList(1)))
    assert(LazyList(1,2,3,1).distinct.equals(LazyList(1,2,3)))

    // Strict collections distinct
    assert(List().distinct.equals(List()))
    assert(List(1,1,1,1).distinct.equals(List(1)))
    assert(List(1,2,3,1).distinct.equals(List(1,2,3)))
  }

  @Test
  def linearSeqSize(): Unit = {
    val list = 1 :: 2 :: 3 :: Nil
    assert(list.length == list.size && list.size == 3)
    val lazyList = 1 #:: 2 #:: 3 #:: LazyList.Empty
    assert(lazyList.length == lazyList.size && lazyList.size == 3)
  }

  @Test
  def mainTest(): Unit = {
    val ints = List(1, 2, 3)
    val intsVec = ints.to(Vector)
    val intsLzy = ints.to(LazyList)
    val intsArr = ints.to(ImmutableArray)
    val intsBuf = ints.to(ArrayBuffer)
    val intsListBuf = ints.to(ListBuffer)
    val intsView = ints.view
    seqOps(ints)
    seqOps(intsVec)
    seqOps(intsLzy)
    seqOps(intsArr)
    seqOps(intsBuf)
    seqOps(intsListBuf)
    viewOps(intsView)
    stringOps("abc")
    arrayOps(Array(1, 2, 3))
    immutableSeqOps(ints)
    immutableSeqOps(intsVec)
    immutableSeqOps(intsLzy)
    immutableSeqOps(intsArr)
    immutableArrayOps(intsArr)
    lazyListOps(intsLzy)
    distinct()
    linearSeqSize()
  }
}
