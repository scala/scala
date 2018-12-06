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

package scala.collection.convert

import java.util.Spliterator

/**
 * A Stepper is a specialized collection that can step through its contents once.  It provides the
 * same test-and-get methods as `Iterator`, named `hasStep` and `nextStep` so they can coexist with
 * iterator methods.  However, like `Spliterator`, steppers provide a `tryStep` method to call a
 * closure if another element exists, a `substep()` method to split into pieces, and
 * `characteristics` and size-reporting methods that implement the subdivision and report what is
 * known about the remaining size of the `Stepper`. `Stepper` thus naturally implements both
 * `Iterator` and `Spliterator`.
 *
 * A `Stepper` can present itself as a Spliterator via the `spliterator` method, or as a Scala
 * `Iterator` via the `iterator` method.  The `Stepper` trait is compatible with both `Spliterator`
 * and Java's generic and primitive iterators, so a `Stepper` may already be one or both.
 *
 * Subtraits `NextStepper` and `TryStepper` fill in the basic capability by either implementing
 * `tryStep` in terms of `hasStep` and `nextStep`, or vice versa.
 *
 * Subtraits `AnyStepper`, `DoubleStepper`, `IntStepper`, and `LongStepper` implement both the
 * `Stepper` trait and the corresponding Java `Spliterator` and `Iterator`/`PrimitiveIterator`.
 *
 * Example:
 * {{{
 * val s = Vector(1,2,3,4).stepper
 * if (s.hasStep) println(s.nextStep())    //  Prints 1
 * println(s.tryStep(i => println(i*i)))   //  Prints 2, then true
 * s.substep.foreach(println)              //  Prints 3
 * println(s.count(_ > 3))                 //  Prints 4
 * println(s.hasStep)                      //  Prints `false`
 * }}}
 */
trait Stepper[@specialized(Double, Int, Long) A] extends StepperOps[A, Stepper[A]]

/** An (optional) marker trait that indicates that a `Stepper` can call `substep` with
 * at worst O(log N) time and space complexity, and that the division is likely to
 * be reasonably even.
 */
trait EfficientSubstep

/** Provides functionality for Stepper while keeping track of a more precise type of the collection.
 */
trait StepperOps[@specialized(Double, Int, Long) A, +CC] { self: CC =>
  /** Characteristics are bit flags that indicate runtime characteristics of this Stepper.
   *
   * - `Distinct` means that no duplicates exist
   * - `Immutable` means that the underlying collection is guaranteed not to change during traversal
   * - `NonNull` means that no nulls will be returned during traversal
   * - `Sized` means that the collection knows its exact size
   * - `SubSized` means that sub-Steppers created with `substep()` will also know their own size. `SubSized` steppers must also be `Sized`.
   *
   * The Java flags `CONCURRENT` and `SORTED` are not supported; modification of a concurrency-aware underlying collection is not
   * guaranteed to be any safer than modification of any generic mutable collection, and if the underlying collection is ordered by
   * virtue of sorting, `Stepper` will not keep track of that fact.
   */
  def characteristics: Int

  /** Returns the size of the collection, if known exactly, or `-1` if not. */
  def knownSize: Long

  /** `true` if there are more elements to step through, `false` if not. */
  def hasStep: Boolean

  /** The next element traversed by this Stepper.
   * `nextStep()` throws an exception if no elements exist, so check `hasStep` immediately prior
   * to calling. Note that `tryStep` also consumes an element, so the result of `hasStep` will
   * be invalid after `tryStep` is called.
   */
  def nextStep(): A

  /** If another element exists, apply `f` to it and return `true`; otherwise, return `false`. */
  def tryStep(f: A => Unit): Boolean

  /** Attempt to split this `Stepper` in half, with the new (returned) copy taking the first half
   * of the collection, and this one advancing to cover the second half. If subdivision is not
   * possible or not advisable, `substep()` will return `null`.
   */
  def substep(): CC

  /** Warns this `Stepper` that it is likely to be used in a parallel context (used for efficiency only) */
  def anticipateParallelism(): this.type = this


  ////
  // Terminal operations (do not produce another Stepper)
  ////

  /** Consumes all remaining elements in this `Stepper` and counts how many there are.
   * This is a terminal operation, though if `knownSize` is non-negative, it won't actually
   * iterate over the elements.
   */
  def count(): Long = knownSize match {
    case x if x < 0 => var n = 0L; while (hasStep) { nextStep(); n += 1 }; n
    case x => x
  }

  /** Consumes all remaining elements in this `Stepper` and counts how many satisfy condition `p`.
   * This is a terminal operation.
   */
  def count(p: A => Boolean): Long = { var n = 0L; while (hasStep) { if (p(nextStep())) n += 1 }; n }

  /** Searches for an element that satisfies condition `p`. If none are found, it returns `false`.
   * This is a terminal operation.
   */
  def exists(p: A => Boolean): Boolean = { while(hasStep) { if (p(nextStep())) return true }; false }

  /** Searches for an element that satisifes condition `p`, returning it wrapped in `Some` if one is found, or `None` otherwise.
   * This is a terminal operation.
   */
  def find(p: A => Boolean): Option[A] = { while (hasStep) { val a = nextStep(); if (p(a)) return Some(a) }; None }

  /** Repeatedly applies `op` to propagate an initial value `zero` through all elements of the collection.
   * Traversal order is left-to-right.
   * This is a terminal operation.
   */
  def fold[@specialized(Double, Int, Long) B](zero: B)(op: (B, A) => B) = { var b = zero; while (hasStep) { b = op(b, nextStep()) }; b }

  /** Repeatedly applies `op` to propagate an initial value `zero` through the collection until a condition `p` is met.
   * If `p` is never met, the result of the last operation is returned.
   * This is a terminal operation.
   */
  def foldTo[@specialized(Double, Int, Long) B](zero: B)(op: (B, A) => B)(p: B => Boolean) = { var b = zero; while (!p(b) && hasStep) { b = op(b, nextStep()) }; b }

  /** Applies `f` to every remaining element in the collection.
   * This is a terminal operation.
   */
  def foreach(f: A => Unit): Unit = { while (hasStep) f(nextStep()) }

  /** Repeatedly merges elements with `op` until only a single element remains.
   * Throws an exception if the `Stepper` is empty.
   * Merging occurs from left to right.
   * This is a terminal operation.
   */
  def reduce(op: (A, A) => A): A = { var a = nextStep(); while (hasStep) { a = op(a, nextStep()) }; a }


  ////
  // Operations that convert to another related type
  ////

  /** Returns this `Stepper` as a `java.util.Spliterator`.
   * This is a terminal operation.
   */
  def spliterator: Spliterator[A]

  /** Returns this `Stepper` as a Scala `Iterator`.
   * This is a terminal operation.
   */
  def iterator: Iterator[A] = new scala.collection.AbstractIterator[A] {
    def hasNext = self.hasStep
    def next() = self.nextStep()
  }

  /**
   * Copy the elements of this stepper into a Scala collection.
   */
  def to[C1](factory: collection.Factory[A, C1])(implicit info: AccumulatorFactoryInfo[A, C1]): C1 = {
    // special casing accumulators avoids boxing
    if (info.companion == IntAccumulator) {
      val is = this.asInstanceOf[StepperOps[Int, _]]
      val a = new IntAccumulator
      while (is.hasStep) a addOne is.nextStep()
      a.asInstanceOf[C1]
    }
    else if (info.companion == LongAccumulator) {
      val is = this.asInstanceOf[StepperOps[Long, _]]
      val a = new LongAccumulator
      while (is.hasStep) a addOne is.nextStep()
      a.asInstanceOf[C1]
    }
    else if (info.companion == DoubleAccumulator) {
      val is = this.asInstanceOf[StepperOps[Double, _]]
      val a = new DoubleAccumulator
      while (is.hasStep) a addOne is.nextStep()
      a.asInstanceOf[C1]
    }
    else factory.fromSpecific(iterator)
  }
}

/** This trait indicates that a `Stepper` will implement `tryStep` in terms of `hasNext` and `nextStep`. */
trait NextStepper[@specialized(Double, Int, Long) A] extends Stepper[A] with StepperOps[A, NextStepper[A]] {
  def tryStep(f: A => Unit) = if (hasStep) { f(nextStep()); true } else false
  def spliterator: Spliterator[A] = new ProxySpliteratorViaNext[A](this)
}
private[convert] class ProxySpliteratorViaNext[A](underlying: NextStepper[A]) extends Spliterator[A] {
  def characteristics = underlying.characteristics
  def estimateSize() = underlying.knownSize
  def tryAdvance(f: java.util.function.Consumer[_ >: A]): Boolean = if (underlying.hasStep) { f.accept(underlying.nextStep()); true } else false
  def trySplit() = underlying.substep() match { case null => null; case x => new ProxySpliteratorViaNext[A](x) }
}

/** This trait indicates that a `Stepper` will implement `hasNext` and `nextStep` by caching applications of `tryStep`.
 * Subclasses must implement `tryUncached` instead of `tryStep`, and should leave it protected, and must implement
 * `knownUncachedSize` instead of `knownSize`. For speed, `foreachUncached` may also be overridden.  It is recommended
 * that all of the `Uncached` methods be left protected.
 */
trait TryStepper[@specialized(Double, Int, Long) A] extends Stepper[A] with StepperOps[A, TryStepper[A]] {
  protected def myCache: A
  protected def myCache_=(a: A): Unit
  protected final var myCacheIsFull = false
  private def load(): Boolean = {
    myCacheIsFull = tryStep(myCache = _)
    myCacheIsFull
  }
  final def hasStep = myCacheIsFull || load()
  final def nextStep() = {
    if (!myCacheIsFull) {
      load()
      if (!myCacheIsFull) Stepper.throwNSEE()
    }
    val ans = myCache
    myCacheIsFull = false
    myCache = null.asInstanceOf[A]
    ans
  }
  final def knownSize = knownUncachedSize + (if (myCacheIsFull) 1 else 0)
  protected def knownUncachedSize: Long
  final def tryStep(f: A => Unit): Boolean = if (myCacheIsFull) { f(myCache); myCacheIsFull = false; true } else tryUncached(f)
  protected def tryUncached(f: A => Unit): Boolean
  final override def foreach(f: A => Unit): Unit = { if (myCacheIsFull) { f(myCache); myCacheIsFull = false }; foreachUncached(f) }
  protected def foreachUncached(f: A => Unit): Unit = { while (tryUncached(f)) {} }
  def spliterator: Spliterator[A] = new ProxySpliteratorViaTry[A](this)
}
private[convert] class ProxySpliteratorViaTry[A](underlying: TryStepper[A]) extends Spliterator[A] {
  def characteristics = underlying.characteristics
  def estimateSize() = underlying.knownSize
  def tryAdvance(f: java.util.function.Consumer[_ >: A]): Boolean = underlying.tryStep(a => f.accept(a))
  override def forEachRemaining(f: java.util.function.Consumer[_ >: A]): Unit = { underlying.foreach(a => f.accept(a)) }
  def trySplit() = underlying.substep() match { case null => null; case x => new ProxySpliteratorViaTry[A](x) }
}

/** Any `AnyStepper` combines the functionality of a Java `Iterator`, a Java `Spliterator`, and a `Stepper`. */
trait AnyStepper[A] extends Stepper[A] with java.util.Iterator[A] with Spliterator[A] with StepperOps[A, AnyStepper[A]] {
  override def forEachRemaining(c: java.util.function.Consumer[_ >: A]): Unit = { while (hasNext) { c.accept(next()) } }
  def hasStep = hasNext
  def knownSize = getExactSizeIfKnown
  def nextStep() = next()
  def tryAdvance(c: java.util.function.Consumer[_ >: A]): Boolean = if (hasNext) { c.accept(next()); true } else false
  def tryStep(f: A => Unit): Boolean = if (hasNext) { f(next()); true } else false
  def trySplit() = substep()
  override def spliterator: Spliterator[A] = this
}

private[convert] object AnyStepper {
  final class BoxedDoubleStepper(st: DoubleStepper) extends AnyStepper[Double] {
    def hasNext: Boolean = st.hasNext
    def next(): Double = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): AnyStepper[Double] = new BoxedDoubleStepper(st.substep())
  }

  final class BoxedIntStepper(st: IntStepper) extends AnyStepper[Int] {
    def hasNext: Boolean = st.hasNext
    def next(): Int = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): AnyStepper[Int] = new BoxedIntStepper(st.substep())
  }

  final class BoxedLongStepper(st: LongStepper) extends AnyStepper[Long] {
    def hasNext: Boolean = st.hasNext
    def next(): Long = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): AnyStepper[Long] = new BoxedLongStepper(st.substep())
  }
}

/** A `DoubleStepper` combines the functionality of a Java `PrimitiveIterator`, a Java `Spliterator`, and a `Stepper`, all specialized for `Double` values. */
trait DoubleStepper extends Stepper[Double] with java.util.PrimitiveIterator.OfDouble with Spliterator.OfDouble with StepperOps[Double, DoubleStepper] {
  override def forEachRemaining(c: java.util.function.Consumer[_ >: java.lang.Double]): Unit = { while (hasNext) { c.accept(java.lang.Double.valueOf(nextDouble)) } }
  override def forEachRemaining(c: java.util.function.DoubleConsumer): Unit = { while (hasNext) { c.accept(nextDouble) } }
  def hasStep = hasNext
  def knownSize = getExactSizeIfKnown
  def nextStep() = nextDouble
  override def tryAdvance(c: java.util.function.Consumer[_ >: java.lang.Double]): Boolean = if (hasNext) { c.accept(java.lang.Double.valueOf(nextDouble)); true } else false
  def tryAdvance(c: java.util.function.DoubleConsumer): Boolean = if (hasNext) { c.accept(nextDouble); true } else false
  def tryStep(f: Double => Unit): Boolean = if (hasNext) { f(nextDouble); true } else false
  def trySplit() = substep()
  override def spliterator: Spliterator[Double] = this.asInstanceOf[Spliterator[Double]]  // Scala and Java disagree about whether it's java.lang.Double or double
}

/** An `IntStepper` combines the functionality of a Java `PrimitiveIterator`, a Java `Spliterator`, and a `Stepper`, all specialized for `Int` values. */
trait IntStepper extends Stepper[Int] with java.util.PrimitiveIterator.OfInt with Spliterator.OfInt with StepperOps[Int, IntStepper] {
  override def forEachRemaining(c: java.util.function.Consumer[_ >: java.lang.Integer]): Unit = { while (hasNext) { c.accept(java.lang.Integer.valueOf(nextInt)) } }
  override def forEachRemaining(c: java.util.function.IntConsumer): Unit = { while (hasNext) { c.accept(nextInt) } }
  def hasStep = hasNext
  def knownSize = getExactSizeIfKnown
  def nextStep() = nextInt
  override def tryAdvance(c: java.util.function.Consumer[_ >: java.lang.Integer]): Boolean = if (hasNext) { c.accept(java.lang.Integer.valueOf(nextInt)); true } else false
  def tryAdvance(c: java.util.function.IntConsumer): Boolean = if (hasNext) { c.accept(nextInt); true } else false
  def tryStep(f: Int => Unit): Boolean = if (hasNext) { f(nextInt); true } else false
  def trySplit() = substep()
  override def spliterator: Spliterator[Int] = this.asInstanceOf[Spliterator[Int]]  // Scala and Java disagree about whether it's java.lang.Integer or int
}

/** A `LongStepper` combines the functionality of a Java `PrimitiveIterator`, a Java `Spliterator`, and a `Stepper`, all specialized for `Long` values. */
trait LongStepper extends Stepper[Long] with java.util.PrimitiveIterator.OfLong with Spliterator.OfLong with StepperOps[Long, LongStepper] {
  override def forEachRemaining(c: java.util.function.Consumer[_ >: java.lang.Long]): Unit = { while (hasNext) { c.accept(java.lang.Long.valueOf(nextLong)) } }
  override def forEachRemaining(c: java.util.function.LongConsumer): Unit = { while (hasNext) { c.accept(nextLong) } }
  def hasStep = hasNext
  def knownSize = getExactSizeIfKnown
  def nextStep() = nextLong
  override def tryAdvance(c: java.util.function.Consumer[_ >: java.lang.Long]): Boolean = if (hasNext) { c.accept(java.lang.Long.valueOf(nextLong)); true } else false
  def tryAdvance(c: java.util.function.LongConsumer): Boolean = if (hasNext) { c.accept(nextLong); true } else false
  def tryStep(f: Long => Unit): Boolean = if (hasNext) { f(nextLong); true } else false
  def trySplit() = substep()
  override def spliterator: Spliterator[Long] = this.asInstanceOf[Spliterator[Long]]  // Scala and Java disagree about whether it's java.lang.Long or long
}

object Stepper {
  /** Indicates that a Stepper delivers distinct values (e.g. is backed by a `Set`) */
  val Distinct = Spliterator.DISTINCT

  /** Indicates that a Stepper runs over an immutable collection */
  val Immutable = Spliterator.IMMUTABLE

  /** Indicates that a Stepper will not return any `null` values */
  val NonNull = Spliterator.NONNULL

  /** Indicates that a Stepper delivers elements in a particular order that should be maintained */
  val Ordered = Spliterator.ORDERED

  /** Indicates that a Stepper knows exactly how many elements it contains */
  val Sized = Spliterator.SIZED

  /** Indicates that a Stepper's children (created with substep()) will all know their size.  Steppers that are SubSized must also be Sized. */
  val SubSized = Spliterator.SUBSIZED

  private[convert] final def throwNSEE(): Nothing = throw new NoSuchElementException("Empty Stepper")


  private class OfSpliterator[A](sp: Spliterator[A])
    extends AnyStepper[A] with java.util.function.Consumer[A] {
    private var cache: A = null.asInstanceOf[A]
    private var cached: Boolean = false
    def accept(a: A): Unit = { cache = a; cached = true }

    private def loadCache: Boolean = sp.tryAdvance(this)
    private def useCache(c: java.util.function.Consumer[_ >: A]): Boolean = {
      if (cached) {
        c.accept(cache)
        cache = null.asInstanceOf[A]
        cached = false
        true
      }
      else false
    }

    def characteristics = sp.characteristics
    def estimateSize = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.Consumer[_ >: A]): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext = cached || loadCache
    def next() = {
      if (!hasNext) throwNSEE()
      val ans = cache
      cache = null.asInstanceOf[A]
      cached = false
      ans
    }
    def substep(): AnyStepper[A] = {
      val subSp = sp.trySplit()
      if (subSp eq null) null
      else {
        val sub = new OfSpliterator(subSp)
        if (cached) {
          sub.cache = cache
          sub.cached = true
          cache = null.asInstanceOf[A]
          cached = false
        }
        sub
      }
    }
    override def tryAdvance(c: java.util.function.Consumer[_ >: A]) = useCache(c) || sp.tryAdvance(c)
  }

  private class OfDoubleSpliterator(sp: Spliterator.OfDouble)
    extends DoubleStepper with java.util.function.DoubleConsumer {
    private var cache: Double = Double.NaN
    private var cached: Boolean = false
    def accept(d: Double): Unit = { cache = d; cached = true }

    private def loadCache: Boolean = sp.tryAdvance(this)
    private def useCache(c: java.util.function.DoubleConsumer): Boolean = {
      if (cached) {
        c.accept(cache)
        cached = false
        true
      }
      else false
    }

    def characteristics = sp.characteristics
    def estimateSize = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.DoubleConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext = cached || loadCache
    def nextDouble() = {
      if (!hasNext) throwNSEE()
      val ans = cache
      cached = false
      ans
    }
    def substep(): DoubleStepper = {
      val subSp = sp.trySplit()
      if (subSp eq null) null
      else {
        val sub = new OfDoubleSpliterator(subSp)
        if (cached) {
          sub.cache = cache
          sub.cached = true
          cached = false
        }
        sub
      }
    }
    override def tryAdvance(c: java.util.function.DoubleConsumer) = useCache(c) || sp.tryAdvance(c)
  }

  private class OfIntSpliterator(sp: Spliterator.OfInt)
    extends IntStepper with java.util.function.IntConsumer {
    private var cache: Int = 0
    private var cached: Boolean = false
    def accept(i: Int): Unit = { cache = i; cached = true }

    private def loadCache: Boolean = sp.tryAdvance(this)
    private def useCache(c: java.util.function.IntConsumer): Boolean = {
      if (cached) {
        c.accept(cache)
        cached = false
        true
      }
      else false
    }

    def characteristics = sp.characteristics
    def estimateSize = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.IntConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext = cached || loadCache
    def nextInt() = {
      if (!hasNext) throwNSEE()
      val ans = cache
      cached = false
      ans
    }
    def substep(): IntStepper = {
      val subSp = sp.trySplit()
      if (subSp eq null) null
      else {
        val sub = new OfIntSpliterator(subSp)
        if (cached) {
          sub.cache = cache
          sub.cached = true
          cached = false
        }
        sub
      }
    }
    override def tryAdvance(c: java.util.function.IntConsumer) = useCache(c) || sp.tryAdvance(c)
  }

  private class OfLongSpliterator(sp: Spliterator.OfLong)
    extends LongStepper with java.util.function.LongConsumer {
    private var cache: Long = 0L
    private var cached: Boolean = false
    def accept(l: Long): Unit = { cache = l; cached = true }

    private def loadCache: Boolean = sp.tryAdvance(this)
    private def useCache(c: java.util.function.LongConsumer): Boolean = {
      if (cached) {
        c.accept(cache)
        cached = false
        true
      }
      else false
    }

    def characteristics = sp.characteristics
    def estimateSize = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.LongConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext = cached || loadCache
    def nextLong() = {
      if (!hasNext) throwNSEE()
      val ans = cache
      cached = false
      ans
    }
    def substep(): LongStepper = {
      val subSp = sp.trySplit()
      if (subSp eq null) null
      else {
        val sub = new OfLongSpliterator(subSp)
        if (cached) {
          sub.cache = cache
          sub.cached = true
          cached = false
        }
        sub
      }
    }
    override def tryAdvance(c: java.util.function.LongConsumer) = useCache(c) || sp.tryAdvance(c)
  }

  /** Creates a `Stepper` over a generic `Spliterator`. */
  def ofSpliterator[A](sp: Spliterator[A]): AnyStepper[A] = sp match {
    case as: AnyStepper[A] => as
    case s: DoubleStepper => new AnyStepper.BoxedDoubleStepper(s).asInstanceOf[AnyStepper[A]]
    case s: IntStepper => new AnyStepper.BoxedIntStepper(s).asInstanceOf[AnyStepper[A]]
    case s: LongStepper => new AnyStepper.BoxedLongStepper(s).asInstanceOf[AnyStepper[A]]
    case _ => new OfSpliterator[A](sp)
  }


  /** Creates a `Stepper` over a `DoubleSpliterator`. */
  def ofSpliterator(sp: Spliterator.OfDouble): DoubleStepper = sp match {
    case ds: DoubleStepper => ds
    case _ => new OfDoubleSpliterator(sp)
  }

  /** Creates a `Stepper` over an `IntSpliterator`. */
  def ofSpliterator(sp: Spliterator.OfInt): IntStepper = sp match {
    case is: IntStepper => is
    case _ => new OfIntSpliterator(sp)
  }


  /** Creates a `Stepper` over a `LongSpliterator`. */
  def ofSpliterator(sp: Spliterator.OfLong): LongStepper = sp match {
    case ls: LongStepper => ls
    case _ => new OfLongSpliterator(sp)
  }

  /* These adapter classes can wrap an AnyStepper of anumeric type into a possibly widened primitive Stepper type.
   * This provides a basis for more efficient stream processing on unboxed values provided that the original source
   * of the data is boxed. In other cases native implementations of the primitive stepper types should be provided
   * (see for example StepsIntArray and StepsWidenedByteArray). */

  private[convert] class UnboxingDoubleStepper(st: AnyStepper[Double]) extends DoubleStepper {
    def hasNext: Boolean = st.hasNext
    def nextDouble(): Double = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): DoubleStepper = new UnboxingDoubleStepper(st.substep())
  }

  private[convert] class UnboxingIntStepper(st: AnyStepper[Int]) extends IntStepper {
    def hasNext: Boolean = st.hasNext
    def nextInt(): Int = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): IntStepper = new UnboxingIntStepper(st.substep())
  }

  private[convert] class UnboxingLongStepper(st: AnyStepper[Long]) extends LongStepper {
    def hasNext: Boolean = st.hasNext
    def nextLong(): Long = st.next()
    def characteristics: Int = st.characteristics
    def estimateSize(): Long = st.estimateSize()
    def substep(): LongStepper = new UnboxingLongStepper(st.substep())
  }

  private[convert] class UnboxingByteStepper(st: AnyStepper[Byte]) extends IntStepper {
    def hasNext: Boolean = st.hasNext
    def nextInt(): Int = st.next()
    def characteristics: Int = st.characteristics | NonNull
    def estimateSize(): Long = st.estimateSize()
    def substep(): IntStepper = new UnboxingByteStepper(st.substep())
  }

  private[convert] class UnboxingCharStepper(st: AnyStepper[Char]) extends IntStepper {
    def hasNext: Boolean = st.hasNext
    def nextInt(): Int = st.next()
    def characteristics: Int = st.characteristics | NonNull
    def estimateSize(): Long = st.estimateSize()
    def substep(): IntStepper = new UnboxingCharStepper(st.substep())
  }

  private[convert] class UnboxingShortStepper(st: AnyStepper[Short]) extends IntStepper {
    def hasNext: Boolean = st.hasNext
    def nextInt(): Int = st.next()
    def characteristics: Int = st.characteristics | NonNull
    def estimateSize(): Long = st.estimateSize()
    def substep(): IntStepper = new UnboxingShortStepper(st.substep())
  }

  private[convert] class UnboxingFloatStepper(st: AnyStepper[Float]) extends DoubleStepper {
    def hasNext: Boolean = st.hasNext
    def nextDouble(): Double = st.next()
    def characteristics: Int = st.characteristics | NonNull
    def estimateSize(): Long = st.estimateSize()
    def substep(): DoubleStepper = new UnboxingFloatStepper(st.substep())
  }
}
