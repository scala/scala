package scala

import org.junit.Assert._
import org.junit.{Ignore, Test}

import scala.collection.mutable.ArrayBuffer

// based on run/t6448.scala partest

// Tests to show that various `collect` functions avoid calling
// both `PartialFunction#isDefinedAt` and `PartialFunction#apply`.
//
class CollectTest {
  var calls = new ArrayBuffer[Int]
  def f(i: Int) = { calls += i; true }

  class Counter {
    var count = 0
    def apply(i: Int) = synchronized {count += 1; true}
  }

  def testing(expectedCalls: Seq[Int], expectedRes: Any = null)(body: => Any): Unit = {
    calls.clear()
    val res = body
    assertEquals(expectedCalls, calls)
    if(expectedRes != null) assertEquals(expectedRes, res)
  }

  @Test
  def testListCollect: Unit =
    testing(Seq(1, 2), List(1))(List(1, 2) collect { case x if f(x) && x < 2 => x})

  @Test
  def testListCollectFirst: Unit =
    testing(Seq(1), Some(1))(List(1, 2) collectFirst { case x if f(x) && x < 2 => x})

  @Test
  def testOptionCollect1: Unit =
    testing(Seq(1), Some(1))(Some(1) collect { case x if f(x) && x < 2 => x})

  @Test
  def testOptionCollect2: Unit =
    testing(Seq(2), None)(Some(2) collect { case x if f(x) && x < 2 => x})

  @Test
  def testStreamCollect: Unit =
    testing(Seq(1, 2), List(1))((Stream(1, 2).collect { case x if f(x) && x < 2 => x}).toList)

  @Test
  def testStreamCollectFirst: Unit =
    testing(Seq(1), Some(1))(Stream.continually(1) collectFirst { case x if f(x) && x < 2 => x})

  @Ignore @Test
  def testIteratorCollect: Unit =
    testing(???)((Iterator(1, 2) collect { case x if f(x) && x < 2 => x}).toList)

  @Ignore @Test
  def testListViewCollect: Unit =
    testing(???)((Iterator(1, 2) collect { case x if f(x) && x < 2 => x}).toList)

  @Ignore @Test
  def testFutureCollect: Unit = {
    // This would do the trick in Future.collect, but I haven't added this yet as there is a tradeoff
    // with extra allocations to consider.
    //
    // pf.lift(v) match {
    //   case Some(x) => p success x
    //   case None    => fail(v)
    // }
    testing(???) {
      import scala.concurrent.Await
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration.Duration
      val result = scala.concurrent.Future(1) collect { case x if f(x) => x}
      Await.result(result, Duration.Inf)
    }
  }

  // TODO Future.{onSuccess, onFailure, recoverWith, andThen}
}
