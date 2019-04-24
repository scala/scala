package scala.collection.immutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._

import scala.ref.WeakReference
import scala.util.Try

// TODO: fill this out with all relevant LazyList methods
@RunWith(classOf[JUnit4])
class LazyListGCTest {
  /** Test helper to verify that the given LazyList operation allows
    * GC of the head during processing of the tail.
    */
  def assertLazyListOpAllowsGC(op: (=> LazyList[Int], Int => Unit) => Any, f: Int => Unit): Unit = {
    val msgSuccessGC = "GC success"
    val msgFailureGC = "GC failure"

    // A LazyList of 500 elements at most. We will test that the head can be collected
    // while processing the tail. After each element we will GC and wait 10 ms, so a
    // failure to collect will take roughly 5 seconds.
    val ref = WeakReference( LazyList.from(1).take(500) )

    def gcAndThrowIfCollected(n: Int): Unit = {
      System.gc()                                                   // try to GC
      Thread.sleep(10)                                              // give it 10 ms
      if (ref.get.isEmpty) throw new RuntimeException(msgSuccessGC) // we're done if head collected
      f(n)
    }

    val res = Try { op(ref(), gcAndThrowIfCollected) }.failed       // success is indicated by an
    val msg = res.map(_.getMessage).getOrElse(msgFailureGC)         // exception with expected message
    // failure is indicated by no
    assertTrue(msg == msgSuccessGC)                                 // exception, or one with different message
  }

  @Test
  def foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.foreach(_), _ => ())
  }

  @Test
  def filter_all_foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.filter(_ => true).foreach(_), _ => ())
  }

  @Test
  def filter_none_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.filter(i => { check(i); false }).headOption, _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.withFilter(_ > 1).foreach(_), _ => ())
  }

  @Test // scala/bug#8990
  def withFilter_after_first_withFilter_foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.withFilter(_ > 1).withFilter(_ < 100).foreach(_), _ => ())
  }

  @Test // scala/bug#11443
  def find_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.find(i => { check(i); false }), _ => ())
  }

  @Test
  def collect_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.collect({ case i if { check(i); false } => i }).headOption, _ => ())
  }

  @Test // scala/bug#11443
  def collectFirst_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.collectFirst({ case i if { check(i); false } => i }), _ => ())
  }

  @Test
  def map_foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.map(_ + 1).foreach(_), _ => ())
  }

  @Test
  def tapEach_foreach_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.tapEach(_ + 1).foreach(_), _ => ())
  }

  @Test
  def tapEach_tail_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.tapEach(_).tail.headOption, _ => ())
  }

  @Test
  def flatMap_none_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.flatMap(i => { check(i); Nil }).headOption, _ => ())
  }

  @Test
  def tapEach_drop_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.tapEach(_).drop(1000000).headOption, _ => ())
  }

  @Test
  def dropWhile_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC((ll, check) => ll.dropWhile(i => {check(i); i < 1000000}).headOption, _ => ())
  }

  @Test
  def tapEach_takeRight_headOption_allowsGC(): Unit = {
    assertLazyListOpAllowsGC(_.tapEach(_).takeRight(2).headOption, _ => ())
  }
}
