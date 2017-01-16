package strawman.collection.test

import java.lang.String
import scala.{Int, Unit, Array, StringContext, Boolean, Any, Char}
import scala.Predef.{println, charWrapper}

import strawman.collection.immutable._
import strawman.collection.mutable._
import strawman.collection.{Seq, View, _}
import org.junit.Test

class StrawmanTest {

  def seqOps(xs: Seq[Int]): Unit = {
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
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs7)
    println(xs8)
    println(xs9)
    println(xs10)
    println(xs11)
    println(xs12)
    println(xs13)
    println(xs14)
    println(xs15)
    println(xs16)
  }

  def viewOps(xs: View[Int]): Unit = {
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
    val ys6: View[Int] = xs6
    val ys7: View[Int] = xs7
    val xs8 = xs.drop(2)
    val ys8: View[Int] = xs8
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
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6.to(List))
    println(xs7.to(List))
    println(xs8.to(List))
    println(xs9.to(List))
    println(xs10.to(List))
    println(xs11.to(List))
    println(xs12.to(List))
    println(xs13.to(List))
    println(xs14.to(List))
    println(xs15.to(List))
  }

  def stringOps(xs: String): Unit = {
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
    val xs9 = xs.map(_ + 1)
    val ys9: Seq[Int] = xs9
    val xs9a = xs.map(_.toUpper)
    val ys9a: String = xs9a
    val xs10 = xs.flatMap((x: Char) => s"$x,$x")
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
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6)
    println(xs7)
    println(xs8)
    println(xs9)
    println(xs9a)
    println(xs10)
    println(xs11)
    println(xs11a)
    println(xs12)
    println(xs13)
    println(xs14)
    println(xs15)
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
    println("-------")
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
    println(xs6.view)
    println(xs7.view)
    println(xs8.view)
    println(xs9.view)
    println(xs10.view)
    println(xs11.view)
    println(xs12.view)
    println(xs13)
    println(xs14.view)
    println(xs15.view)
    println(xs16.view)
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
  }

  @Test
  def mainTest: Unit = {
    val ints = List(1, 2, 3)
    val intsBuf = ints.to(ArrayBuffer)
    val intsListBuf = ints.to(ListBuffer)
    val intsView = ints.view
    seqOps(ints)
    seqOps(intsBuf)
    seqOps(intsListBuf)
    viewOps(intsView)
    stringOps("abc")
    arrayOps(Array(1, 2, 3))
    lazyListOps(LazyList(1, 2, 3))
  }
}
