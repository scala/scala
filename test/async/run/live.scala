/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.async
package run
package live

import org.junit.Test

import internal.AsyncTestLV
import AsyncTestLV._

case class Cell[T](v: T)

class Meter(val len: Long) extends AnyVal

case class MCell[T](var v: T)


class LiveVariablesSpec {
  AsyncTestLV.clear()

  @Test
  def `zero out fields of reference type`(): Unit = {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      async { Cell(x.v + 1) }

    def m2(x: Cell[Int]): String =
      async { x.v.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$1$1
      // a == Cell(1)
      val b: Cell[Int] = await(m1(a))  // await$2$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)), AsyncTestLV.log)
      val res = await(m2(b))           // await$3$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields of type Any`(): Unit = {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      async { Cell(x.v + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$4$1
      // a == Cell(1)
      val b: Any = await(m1(a))        // await$5$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)))
      val res = await(m2(b))           // await$6$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `do not zero out fields of primitive type`(): Unit = {
    val f = async { 1 }

    def m1(x: Int): Cell[Int] =
      async { Cell(x + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Int = await(f)            // await$7$1
      // a == 1
      val b: Any = await(m1(a))        // await$8$1
      // b == Cell(2)
      // assert(!AsyncTestLV.log.exists(p => p._1 == "await$7$1"))
      val res = await(m2(b))           // await$9$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `zero out fields of value class type`(): Unit = {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Meter =
      async { new Meter(x.v + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$10$1
      // a == Cell(1)
      val b: Meter = await(m1(a))      // await$11$1
      // b == Meter(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)))
      val res = await(m2(b.len))       // await$12$1
      assert(AsyncTestLV.log.exists(_._2.asInstanceOf[Meter].len == 2L))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields after use in loop`(): Unit = {
    val f = async { MCell(1) }

    def m1(x: MCell[Int], y: Int): Int =
      async { x.v + y }

    def m3() = async {
      // state #1
      val a: MCell[Int] = await(f)     // await$13$1
      // state #2
      var y = MCell(0)

      while (a.v < 10) {
        // state #4
        a.v = a.v + 1
        y = MCell(await(a).v + 1)      // await$14$1
        // state #7
      }

      // state #3
      // assert(AsyncTestLV.log.exists(entry => entry._1 == "await$14$1"))

      val b = await(m1(a, y.v))        // await$15$1
      // state #8
      assert(AsyncTestLV.log.exists(_._2 == MCell(10)), AsyncTestLV.log)
      assert(AsyncTestLV.log.exists(_._2 == MCell(11)))
      b
    }

    assert(m3() == 21, m3())
  }

  @Test
  def `don't zero captured fields captured lambda`(): Unit = {
    val f = async {
      val x = "x"
      val y = "y"
      await(0)
      y.reverse
      val f = () => assert(x != null)
      await(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields captured by-name`(): Unit = {
    def func0[A](a: => A): () => A =  () => a
    val f = async {
      val x = "x"
      val y = "y"
      await(0)
      y.reverse
      val f = func0(assert(x != null))
      await(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested class`(): Unit = {
    def func0[A](a: => A): () => A = () => a
    val f = async {
      val x = "x"
      val y = "y"
      await(0)
      y.reverse
      val f = new Function0[Unit] {
        def apply = assert(x != null)
      }
      await(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested object`(): Unit = {
    def func0[A](a: => A): () => A = () => a
    val f = async {
      val x = "x"
      val y = "y"
      await(0)
      y.reverse
      object f extends Function0[Unit] {
        def apply = assert(x != null)
      }
      await(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested def`(): Unit = {
    val f = async {
      val x = "x"
      val y = "y"
      await(0)
      y.reverse
      def xx = x
      val f = xx _
      await(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `capture bug`(): Unit = {
    sealed trait Base
    case class B1() extends Base
    case class B2() extends Base
    val outer = List[(Base, Int)]((B1(), 8))

    def getMore(b: Base) = 4

    def baz = async {
      outer.head match {
        case (a @ B1(), r) => {
          val ents = await(getMore(a))

          { () =>
            println(a)
            assert(a ne null)
          }
        }
        case (b @ B2(), x) =>
          () => ???
      }
    }
    baz()
  }

  // https://github.com/scala/async/issues/104
  @Test def dontNullOutVarsOfTypeNothing_t104(): Unit = {
    import scala.async.Async._
    import scala.concurrent.duration.Duration
    import scala.concurrent.{Await, Future}
    import scala.concurrent.ExecutionContext.Implicits.global
    def errorGenerator(randomNum: Double) = {
      Future {
        if (randomNum < 0) {
          throw new IllegalStateException("Random number was too low!")
        } else {
          throw new IllegalStateException("Random number was too high!")
        }
      }
    }
    def randomTimesTwo = async {
      val num = _root_.scala.math.random
      if (num < 0 || num > 1) {
        await(errorGenerator(num))
      }
      num * 2
    }
    Await.result(randomTimesTwo, TestLatch.DefaultTimeout) // was: NotImplementedError
  }
}
