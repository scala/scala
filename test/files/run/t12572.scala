
import java.util.concurrent.ConcurrentSkipListMap
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import java.util.{Timer, TimerTask}

import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import scala.util.chaining._

import org.junit.Assert.assertNull

// Concurrent map should be safe (and atomic) for single-element operations.
// Multi-element operations are not atomic, IDK about safe.

trait Testable {
  def populate(): Unit
  def clear(): Unit
  def query(): Unit
}

class Tester(testable: Testable) {

  testable.populate()

  // Three threads to populate, query, and depopulate the map.
  // Previous behavior is that `lastOption` in `IterableOps` can throw because non-atomic.
  // Good behavior is that `lastOption` should use `lastEntry`.

  abstract class Task extends Runnable {
    val done = new AtomicBoolean
    final def run(): Unit = {
      while (!done.get) {
        task()
        Thread.`yield`()
      }
    }
    def task(): Unit
  }
  def run(): Unit = {
    val populater = new Task {
      def task() = testable.populate()
    }
    val cleaner = new Task {
      def task() = testable.clear()
    }
    val query = new Task {
      def task() = testable.query()
    }
    val tasks = List(query, cleaner, populater)
    def allDone() = tasks.foreach(t => t.done.set(true))
    val thrown = new AtomicReference[Throwable]
    @volatile var ended: Long = 0L
    val handler = new Thread.UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable) = { thrown.set(e); ended = System.nanoTime; allDone() }
    }
    val threads = tasks.map(new Thread(_).tap(_.setUncaughtExceptionHandler(handler)))
    val timer = new Timer
    val finish = new TimerTask { def run() = tasks.foreach(t => t.done.set(true)) }
    val timeout = 10L  // expect racy test to fail fast, but success could be false negative in CI with short timeout
    val started = System.nanoTime
    timer.schedule(finish, timeout)
    threads.foreach(_.start())
    threads.foreach(_.join())
    if (ended != 0L)
      println(s"Terminated after ${ ended - started } ns or ${ TimeUnit.NANOSECONDS.toMillis(ended - started) } ms")
    Option(thrown.get).foreach(_.printStackTrace())
    assertNull(testable.toString, thrown.get)
  }
}

object Test extends App {
  val skipper1 = new Testable {
    val data: ConcurrentSkipListMap[Int, Int] = new ConcurrentSkipListMap[Int, Int]()
    def populate() = {
      data.put(1, 10)
      data.put(2, 20)
      data.put(3, 30)
    }
    def clear() = data.clear()
    def query() = data.subMap(1, 3).asScala.headOption
  }
  val skipper2 = new Testable {
    val data: ConcurrentSkipListMap[Int, Int] = new ConcurrentSkipListMap[Int, Int]()
    def populate() = {
      data.put(1, 10)
      data.put(2, 20)
      data.put(3, 30)
    }
    def clear() = data.clear()
    def query() = data.subMap(1, 3).asScala.lastOption
  }
  val trie1 = new Testable {
    val data: TrieMap[Int, Int] = new TrieMap[Int, Int]()
    def populate() = {
      data.put(1, 10)
      data.put(2, 20)
      data.put(3, 30)
    }
    def clear() = data.clear()
    def query() = data.headOption
  }
  val trie2 = new Testable {
    val data: TrieMap[Int, Int] = new TrieMap[Int, Int]()
    def populate() = {
      data.put(1, 10)
      data.put(2, 20)
      data.put(3, 30)
    }
    def clear() = data.clear()
    def query() = data.lastOption
    override val toString = "TrieMap#lastOption"
  }
  new Tester(skipper1).run()
  new Tester(skipper2).run()
  new Tester(trie1).run()
  new Tester(trie2).run()
}

/*
was:
Uncaught exception on thread Thread[Thread-3,5,main]: java.util.NoSuchElementException
java.util.NoSuchElementException
        at java.base/java.util.concurrent.ConcurrentSkipListMap$SubMap$SubMapIter.advance(ConcurrentSkipListMap.java:2910)
        at java.base/java.util.concurrent.ConcurrentSkipListMap$SubMap$SubMapEntryIterator.next(ConcurrentSkipListMap.java:3013)
        at java.base/java.util.concurrent.ConcurrentSkipListMap$SubMap$SubMapEntryIterator.next(ConcurrentSkipListMap.java:3009)
        at scala.collection.convert.JavaCollectionWrappers$JMapWrapperLike$$anon$5.next(JavaCollectionWrappers.scala:370)
        at scala.collection.convert.JavaCollectionWrappers$JMapWrapperLike$$anon$5.next(JavaCollectionWrappers.scala:367)
        at scala.collection.IterableOps.last(Iterable.scala:241)
        at scala.collection.IterableOps.last$(Iterable.scala:239)
        at scala.collection.AbstractIterable.last(Iterable.scala:933)
        at scala.collection.IterableOps.lastOption(Iterable.scala:251)
        at scala.collection.IterableOps.lastOption$(Iterable.scala:251)
        at scala.collection.AbstractIterable.lastOption(Iterable.scala:933)
        at Tester$$anon$3.task(t12572.scala:40)
        at Tester$Task.run(t12572.scala:24)
        at java.base/java.lang.Thread.run(Thread.java:833)
 */
