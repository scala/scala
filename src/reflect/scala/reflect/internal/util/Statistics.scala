/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect.internal.util

import scala.collection.mutable
import scala.reflect.internal.SymbolTable
import scala.reflect.internal.settings.MutableSettings
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.annotation.nowarn
import scala.runtime.LongRef

abstract class Statistics(val symbolTable: SymbolTable, settings: MutableSettings) {
  type TimerSnapshot = (Long, Long)

  /** If enabled, increment counter by one */
  @inline final def incCounter(c: Counter): Unit = {
    if (enabled && c != null) c.value += 1
  }

  /** If enabled, increment counter by given delta */
  @inline final def incCounter(c: Counter, delta: Int): Unit = {
    if (enabled && c != null) c.value += delta
  }

  /** If enabled, increment counter in map `ctrs` at index `key` by one */
  @inline final def incCounter[K](ctrs: QuantMap[K, Counter], key: K) =
    if (enabled && ctrs != null) ctrs(key).value += 1

  /** If enabled, start subcounter. While active it will track all increments of
   *  its base counter.
   */
  @inline final def startCounter(sc: SubCounter): (Int, Int) =
    if (enabled && sc != null) sc.start() else null

  /** If enabled, stop subcounter from tracking its base counter. */
  @inline final def stopCounter(sc: SubCounter, start: (Int, Int)): Unit = {
    if (enabled && sc != null) sc.stop(start)
  }

  /** If enabled, start timer */
  @inline final def startTimer(tm: Timer): TimerSnapshot =
    if (enabled && tm != null) tm.start() else null

  /** If enabled, stop timer */
  @inline final def stopTimer(tm: Timer, start: TimerSnapshot): Unit = {
    if (enabled && tm != null) tm.stop(start)
  }

  /** If enabled, push and start a new timer in timer stack */
  @inline final def pushTimer(timers: TimerStack, timer: => StackableTimer): TimerSnapshot =
    if (enabled && timers != null) timers.push(timer) else null

  /** If enabled, stop and pop timer from timer stack */
  @inline final def popTimer(timers: TimerStack, prev: TimerSnapshot): Unit = {
    if (enabled && timers != null) timers.pop(prev)
  }

  /** Create a new counter that shows as `prefix` and is active in given phases */
  def newCounter(prefix: String, phases: String*) = new Counter(prefix, phases)

  /** Create a new relative counter that shows as `prefix` and is active
   *  in the same phases as its base counter. Relative counters print as percentages
   *  of their base counters.
   */
  def newRelCounter(prefix: String, ctr: Counter): Counter = new RelCounter(prefix, ctr)

  /** Create a new subcounter that shows as `prefix` and is active
   *  in the same phases as its base counter. Subcounters can track
   *  increments of their base counters and print as percentages
   *  of their base counters.
   */
  def newSubCounter(prefix: String, ctr: Counter): SubCounter = new SubCounter(prefix, ctr)

  /** Create a new counter that shows as `prefix` and is active in given phases */
  def newTimer(prefix: String, phases: String*): Timer = new Timer(prefix, phases)

  /** Create a new subtimer that shows as `prefix` and is active
   *  in the same phases as its base timer. Subtimers can track
   *  increments of their base timers and print as percentages
   *  of their base timers.
   */
  def newSubTimer(prefix: String, timer: Timer): Timer = new SubTimer(prefix, timer)

  /** Create a new stackable that shows as `prefix` and is active
   *  in the same phases as its base timer. Stackable timers are subtimers
   *  that can be stacked in a timerstack, and that print aggregate, as well as specific
   *  durations.
   */
  def newStackableTimer(prefix: String, timer: Timer): StackableTimer = new StackableTimer(prefix, timer)

  /** Create a new view that shows as `prefix` and is active in given phases.
   *  The view always reflects the current value of `quant` as a quantity.
   */
  def newView(prefix: String, phases: String*)(quant: => Any): View = new View(prefix, phases,
quant)

  /** Create a new quantity map that shows as `prefix` and is active in given phases.
   */
  def newQuantMap[K, V](prefix: String, phases: String*)(initValue: => V)(implicit ev: V => Ordered[V]): QuantMap[K, V] = new QuantMap(prefix, phases, initValue)

  /** Same as newQuantMap, where the key type is fixed to be Class[_] */
  def newByClass[V](prefix: String, phases: String*)(initValue: => V)(implicit ev: V => Ordered[V]): QuantMap[Class[_], V] = new QuantMap(prefix, phases, initValue)

  /** Create a new timer stack */
  def newTimerStack() = new TimerStack()

  def allQuantities: Iterable[Quantity] =
    for ((_, q) <- qs if q.underlying == q;
         r <- q :: q.children.toList if r.prefix.nonEmpty) yield r

  private def showPercent(x: Long, base: Long) =
    if (base == 0) "" else f" (${x.toDouble / base.toDouble * 100}%2.1f%%)"

  /** The base trait for quantities.
   *  Quantities with non-empty prefix are printed in the statistics info.
   */
  trait Quantity {
    if (prefix.nonEmpty) {
      val key = s"${if (underlying != this) underlying.prefix else ""}/$prefix"
      qs(key) = this
    }
    val prefix: String
    val phases: Seq[String]
    def underlying: Quantity = this
    def showAt(phase: String) = phases.isEmpty || (phases contains phase)
    def line = f"$prefix%-30s: ${this}"
    val children = new mutable.ListBuffer[Quantity]
  }

  trait SubQuantity extends Quantity {
    protected def underlying: Quantity
    underlying.children += this
  }

  class Counter(val prefix: String, val phases: Seq[String]) extends Quantity with Ordered[Counter] {
    var value: Int = 0
    def compare(that: Counter): Int =
      if (this.value < that.value) -1
      else if (this.value > that.value) 1
      else 0
    override def equals(that: Any): Boolean =
      that match {
        case that: Counter => (this compare that) == 0
        case _ => false
      }
    override def hashCode = value
    override def toString = value.toString
  }

  class View(val prefix: String, val phases: Seq[String], quant: => Any) extends Quantity {
    override def toString = quant.toString
  }

  private class RelCounter(prefix: String, override val underlying: Counter) extends Counter(prefix, underlying.phases) with SubQuantity {
    override def toString =
      if (value == 0) "0"
      else {
        assert(underlying.value != 0, prefix+"/"+underlying.line)
        f"${value.toFloat / underlying.value}%2.1f"
      }
  }

  class SubCounter(prefix: String, override val underlying: Counter) extends Counter(prefix, underlying.phases) with SubQuantity {
    def start() = (value, underlying.value)
    def stop(prev: (Int, Int)): Unit = {
      val (value0, uvalue0) = prev
      value = value0 + underlying.value - uvalue0
    }
    override def toString = s"${value}${showPercent(value.toLong, underlying.value.toLong)}"
  }

  class Timer(val prefix: String, val phases: Seq[String]) extends Quantity {
    private[this] val totalThreads = new AtomicInteger()
    private[this] val threadNanos = new ThreadLocal[LongRef] {
      override def initialValue() = {
        totalThreads.incrementAndGet()
        new LongRef(0)
      }
    }
    private[util] val totalNanos = new AtomicLong
    private[util] val timings = new AtomicInteger
    def nanos = totalNanos.get
    def start(): TimerSnapshot = {
      (threadNanos.get.elem, System.nanoTime())
    }
    def stop(prev: TimerSnapshot): Unit = {
      val (nanos0, start) = prev
      val newThreadNanos = nanos0 + System.nanoTime() - start
      val threadNanosCount = threadNanos.get
      val diff = newThreadNanos - threadNanosCount.elem
      threadNanosCount.elem = newThreadNanos
      totalNanos.addAndGet(diff)
      timings.incrementAndGet()
    }
    protected def show(ns: Long) = s"${ns/1000/1000.0}ms"
    override def toString = {
      val threads = totalThreads.get
      s"$timings spans, ${if (threads > 1) s"$threads threads, "}${show(totalNanos.get)}"
    }
  }

  class SubTimer(prefix: String, override val underlying: Timer) extends Timer(prefix, underlying.phases) with SubQuantity {
    override protected def show(ns: Long) = super.show(ns) + showPercent(ns, underlying.totalNanos.get)
  }

  class StackableTimer(prefix: String, underlying: Timer) extends SubTimer(prefix, underlying) with Ordered[StackableTimer] {
    var specificNanos: Long = 0
    def compare(that: StackableTimer): Int =
      if (this.specificNanos < that.specificNanos) -1
      else if (this.specificNanos > that.specificNanos) 1
      else 0
    override def equals(that: Any): Boolean =
      that match {
        case that: StackableTimer => (this compare that) == 0
        case _ => false
      }
    override def hashCode = specificNanos.##
    override def toString = s"${super.toString} aggregate, ${show(specificNanos)} specific"
  }

  // FIXME: Redesign `QuantMap` so that it no longer extends `HashMap` (probably
  //        using composition), then remove the `@nowarn` annotation
  /** A mutable map quantity where missing elements are automatically inserted
   *  on access by executing `initValue`.
   */
  @nowarn("""cat=deprecation&origin=scala\.collection\.mutable\.HashMap""")
  class QuantMap[K, V](val prefix: String, val phases: Seq[String], initValue: => V)(implicit ev: V => Ordered[V])
      extends mutable.HashMap[K, V] with Quantity {
    override def default(key: K) = {
      val elem = initValue
      this(key) = elem
      elem
    }
    //TODO clients may need to do additional synchronization; QuantMap used to extend SynchronizedMap before 2.13
    override def apply(key: K): V = super.apply(key)
    override def toString =
      this.toSeq.sortWith(_._2 > _._2).map {
        case (cls: Class[_], elem) =>
          s"${cls.toString.substring(cls.toString.lastIndexOf("$") + 1)}: $elem"
        case (key, elem) =>
          s"$key: $elem"
      }.mkString(", ")
  }

  /** A stack of timers, all active, where a timer's specific "clock"
   *  is stopped as long as it is buried by some other timer in the stack, but
   *  its aggregate clock keeps on ticking.
   *
   *  Note: Not threadsafe
   */
  class TimerStack {
    private[this] var elems: List[(StackableTimer, Long)] = Nil
    /** Start given timer and push it onto the stack */
    def push(t: StackableTimer): TimerSnapshot = {
      elems = (t, 0L) :: elems
      t.start()
    }
    /** Stop and pop top timer in stack
     */
    def pop(prev: TimerSnapshot) = {
      val (nanos0, start) = prev
      val duration = System.nanoTime() - start
      val (topTimer, nestedNanos) :: rest = elems: @unchecked
      topTimer.totalNanos.addAndGet(nanos0 + duration)
      topTimer.specificNanos += duration - nestedNanos
      topTimer.timings.incrementAndGet()
      elems = rest match {
        case (outerTimer, outerNested) :: elems1 =>
          (outerTimer, outerNested + duration) :: elems1
        case Nil =>
          Nil
      }
    }
  }

  private[this] val qs = new mutable.HashMap[String, Quantity]

  @inline final def enabled: Boolean = settings.areStatisticsEnabled

  import scala.reflect.internal.Reporter
  /** Reports the overhead of measuring statistics via the nanoseconds variation. */
  final def reportStatisticsOverhead(reporter: Reporter): Unit = {
      val start = System.nanoTime()
      var total = 0L
      for (i <- 1 to 10000) {
        val time = System.nanoTime()
        total += System.nanoTime() - time
      }
      val total2 = System.nanoTime() - start
      val variation = s"${total/10000.0}ns to ${total2/10000.0}ns"
      reporter.echo(NoPosition, s"Enabling statistics, measuring overhead = $variation per timer")
  }

  /** Helper for measuring the overhead of a concrete thunk `body`. */
  @inline final def timed[T](timer: Timer)(body: => T): T = {
    val start = startTimer(timer)
    try body finally stopTimer(timer, start)
  }
}
