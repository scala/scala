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

import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}
import java.util.{PrimitiveIterator, Spliterator, Iterator => JIterator}
import java.{lang => jl}

import scala.collection.AbstractIterator
import scala.collection.convert.impl.AccumulatorFactoryInfo

trait Stepper[@specialized(Double, Int, Long) A] {
  def hasStep: Boolean

  def nextStep(): A

  // may return null
  private[collection] def trySplit(): Stepper[A]

  private[collection] def estimateSize: Long

  private[collection] def characteristics: Int

  // Not Spliterator[A] because when A=Int, we refine the type to Spliterator.OfInt, which is a
  // subtype of Spliterator[Integer]. Could use a shape typeclass implicit argument to express it.
  def spliterator: Spliterator[_]

  def javaIterator: JIterator[_]

  def iterator: Iterator[A] = new AbstractIterator[A] {
    def hasNext: Boolean = hasStep
    def next(): A = nextStep()
  }
}

trait AnyStepper[A] extends Stepper[A] {
  private[collection] def trySplit(): AnyStepper[A]

  def spliterator: Spliterator[A] = new AnyStepper.AnyStepperSpliterator(this)

  def javaIterator: JIterator[A] = new JIterator[A] {
    def hasNext: Boolean = hasStep
    def next(): A = nextStep()
  }
}

object AnyStepper {
  class AnyStepperSpliterator[A](s: AnyStepper[A]) extends Spliterator[A] {
    def tryAdvance(c: Consumer[_ >: A]): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    def trySplit(): Spliterator[A] = s.trySplit().spliterator
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: A]): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
  }

  def ofSeqDoubleStepper(st: DoubleStepper): AnyStepper[Double] = new BoxedDoubleStepper(st)
  def ofParDoubleStepper(st: DoubleStepper with EfficientSubstep): AnyStepper[Double] with EfficientSubstep = new BoxedDoubleStepper(st) with EfficientSubstep

  def ofSeqIntStepper(st: IntStepper): AnyStepper[Int] = new BoxedIntStepper(st)
  def ofParIntStepper(st: IntStepper with EfficientSubstep): AnyStepper[Int] with EfficientSubstep = new BoxedIntStepper(st) with EfficientSubstep

  def ofSeqLongStepper(st: LongStepper): AnyStepper[Long] = new BoxedLongStepper(st)
  def ofParLongStepper(st: LongStepper with EfficientSubstep): AnyStepper[Long] with EfficientSubstep = new BoxedLongStepper(st) with EfficientSubstep

  private[convert] class BoxedDoubleStepper(st: DoubleStepper) extends AnyStepper[Double] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Double] = {
      val s = st.trySplit()
      if (s == null) null
      else new BoxedDoubleStepper(s)
    }
  }

  private[convert] class BoxedIntStepper(st: IntStepper) extends AnyStepper[Int] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Int] = {
      val s = st.trySplit()
      if (s == null) null
      else new BoxedIntStepper(s)
    }
  }

  private[convert] class BoxedLongStepper(st: LongStepper) extends AnyStepper[Long] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Long] = {
      val s = st.trySplit()
      if (s == null) null
      else new BoxedLongStepper(s)
    }
  }
}


trait IntStepper extends Stepper[Int] {
  private[collection] def trySplit(): IntStepper

  def spliterator: Spliterator.OfInt = new IntStepper.IntStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfInt = new PrimitiveIterator.OfInt {
    def hasNext: Boolean = hasStep
    def nextInt(): Int = nextStep()
  }
}
object IntStepper {
  class IntStepperSpliterator(s: IntStepper) extends Spliterator.OfInt {
    def tryAdvance(c: IntConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Integer]): Boolean = (c: AnyRef) match {
      case ic: IntConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())); true } else false
    }
    def trySplit(): Spliterator.OfInt = s.trySplit().spliterator
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: IntConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Integer]): Unit = (c: AnyRef) match {
      case ic: IntConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())) }
    }
  }
}

trait DoubleStepper extends Stepper[Double] {
  private[collection] def trySplit(): DoubleStepper

  def spliterator: Spliterator.OfDouble = new DoubleStepper.DoubleStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfDouble = new PrimitiveIterator.OfDouble {
    def hasNext: Boolean = hasStep
    def nextDouble(): Double = nextStep()
  }
}

object DoubleStepper {
  class DoubleStepperSpliterator(s: DoubleStepper) extends Spliterator.OfDouble {
    def tryAdvance(c: DoubleConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Double]): Boolean = (c: AnyRef) match {
      case ic: DoubleConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Double.valueOf(s.nextStep())); true } else false
    }
    def trySplit(): Spliterator.OfDouble = s.trySplit().spliterator
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: DoubleConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Double]): Unit = (c: AnyRef) match {
      case ic: DoubleConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Double.valueOf(s.nextStep())) }
    }
  }
}

trait LongStepper extends Stepper[Long] {
  private[collection] def trySplit(): LongStepper

  def spliterator: Spliterator.OfLong = new LongStepper.LongStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfLong = new PrimitiveIterator.OfLong {
    def hasNext: Boolean = hasStep
    def nextLong(): Long = nextStep()
  }
}

object LongStepper {
  class LongStepperSpliterator(s: LongStepper) extends Spliterator.OfLong {
    def tryAdvance(c: LongConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Long]): Boolean = (c: AnyRef) match {
      case ic: LongConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Long.valueOf(s.nextStep())); true } else false
    }
    def trySplit(): Spliterator.OfLong = s.trySplit().spliterator
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: LongConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Long]): Unit = (c: AnyRef) match {
      case ic: LongConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Long.valueOf(s.nextStep())) }
    }
  }
}


/**
  * A Stepper is a specialized collection that can step through its contents once.  It provides the
  * same test-and-get methods as `Iterator`, named `hasStep` and `nextStep` so they can coexist with
  * iterator methods.  However, like `Spliterator`, steppers provide a `tryStep` method to call a
  * closure if another element exists, a `substep()` method to split into pieces, and
  * `characteristics` and size-reporting methods that implement the subdivision and report what is
  * known about the remaining size of the `Stepper`. `Stepper` thus naturally implements both
  * `Iterator` and `Spliterator`.
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
  *
  * A `Stepper` can present itself as a Spliterator via the `spliterator` method, or as a Scala
  * `Iterator` via the `iterator` method.  The `Stepper` trait is compatible with both `Spliterator`
  * and Java's generic and primitive iterators, so a `Stepper` may already be one or both.
  *
  * Subtraits `AnyStepper`, `DoubleStepper`, `IntStepper`, and `LongStepper` implement both the
  * `Stepper` trait and the corresponding Java `Spliterator` and `Iterator`/`PrimitiveIterator`.
  *
  * The `Stepper` class provides some methods that overlap with iterator, for example,
  * `stepper.find(p)` has the same semantics as `stepper.iterator.find(p)`. The difference is that
  * the operations defined inj [[Stepper]] are specialized, so they act on primitive steppers
  * ([[IntStepper]], etc.) without boxing the elements.
  */
//trait Stepper[@specialized(Double, Int, Long) A] extends StepperOps[A, Stepper[A]]

/** An (optional) marker trait that indicates that a `Stepper` can call `substep` with
  * at worst O(log N) time and space complexity, and that the division is likely to
  * be reasonably even.
  */
trait EfficientSubstep

/*
/** Provides functionality for Stepper while keeping track of a more precise type of the collection.
  */
trait StepperOps[@specialized(Double, Int, Long) A, +CC] { self: CC =>
  /* Terminal operations (do not produce another Stepper). These operations are specialized, so they
   * operate on primitive steppers without boxing. Other than that, every operation `stepper.op`
   * has the same semantics as `stepper.iterator.op`.
   */

  /** Apply `f` to each element for its side effects.
    * This is a terminal operation.
    * Note: [U] parameter needed to help scalac's type inference.
    */
  def foreach[U](f: A => U): Unit =
    while (hasStep) f(nextStep())

  /** Tests whether a predicate holds for all elements of this Stepper.
    * This is a terminal operation.
    *
    *  @param  p      the predicate used to test elements.
    *  @return        `true` if this Stepper is empty or the given predicate `p`
    *                 holds for all elements of this Stepper, otherwise `false`.
    */
  def forall(p: A => Boolean): Boolean = {
    while(hasStep) {
      if (p(nextStep())) return true
    }
    false
  }

  /** Tests whether a predicate holds for at least one element of this Stepper.
    * This is a terminal operation.
    *
    *  @param  p      the predicate used to test elements.
    *  @return        `true` if the given predicate `p` is satisfied by at least one element of this
    *                 Stepper, otherwise `false`
    */
  def exists(p: A => Boolean): Boolean = {
    while(hasStep) {
      if (p(nextStep())) return true
    }
    false
  }

  /** Counts the number of elements in the Stepper which satisfy a predicate.
    * This is a terminal operation.
    *
    *  @param p       the predicate  used to test elements.
    *  @return        the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Long = {
    var n = 0L
    while (hasStep) {
      if (p(nextStep())) n += 1
    }
    n
  }

  /** Finds the first element of the Stepper satisfying a predicate, if any.
    * This is a terminal operation.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the Stepper
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A] = {
    while (hasStep) {
      val a = nextStep()
      if (p(a)) return Some(a)
    }
    None
  }


  /** Applies a binary operator to a start value and all elements of this Stepper,
    * going left to right.
    * This is a terminal operation.
    *
    *  @param   zero the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this Stepper,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this Stepper.
    *           Returns `z` if this Stepper is empty.
    */
  def foldLeft[@specialized(Double, Int, Long) B](zero: B)(op: (B, A) => B): B = {
    var b = zero
    while (hasStep) { b = op(b, nextStep()) }
    b
  }

  /** This method is an alias for [[foldLeft]].
    * This is a terminal operation.
    */
  def fold[@specialized(Double, Int, Long) B](zero: B)(op: (B, A) => B): B = foldLeft(zero)(op)

  /** Applies a binary operator to all elements of this Stepper, going left to right.
    * This is a terminal operation.
    *
    *  @param  op    the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this Stepper,
    *           going left to right:
    *           {{{
    *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this Stepper.
    *  @throws UnsupportedOperationException if this Stepper is empty.
    */
  def reduceLeft(op: (A, A) => A): A = {
    var a = nextStep()
    while (hasStep) {
      a = op(a, nextStep())
    }
    a
  }

  /** This method is an alias for [[reduceLeft]].
    * This is a terminal operation.
    */
  def reduce(op: (A, A) => A): A = reduceLeft(op)

  final def isEmpty: Boolean = !hasStep

  final def nonEmpty: Boolean = hasStep

    /**
    * Consumes all remaining elements in this `Stepper` and counts how many there are.
    * This is a terminal operation, though if `knownSize` is non-negative, it won't actually
    * iterate over the elements.
    */
  def size: Long = knownSize match {
    case x if x < 0 => var n = 0L; while (hasStep) { nextStep(); n += 1 }; n
    case x => x
  }

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
    def hasNext: Boolean = self.hasStep
    def next(): A = self.nextStep()
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
*/

object Stepper {
  private[convert] final def throwNSEE(): Nothing = throw new NoSuchElementException("Empty Stepper")

/*
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

    private[collection] def characteristics: Int = sp.characteristics
    private[collection] def estimateSize: Long = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.Consumer[_ >: A]): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext: Boolean = cached || loadCache
    def next(): A = {
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
    override def tryAdvance(c: java.util.function.Consumer[_ >: A]): Boolean = useCache(c) || sp.tryAdvance(c)
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

    private[collection] def characteristics: Int = sp.characteristics
    private[collection] def estimateSize: Long = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.DoubleConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext: Boolean = cached || loadCache
    def nextDouble(): Double = {
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
    override def tryAdvance(c: java.util.function.DoubleConsumer): Boolean = useCache(c) || sp.tryAdvance(c)
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

    private[collection] def characteristics: Int = sp.characteristics
    private[collection] def estimateSize: Long = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.IntConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext: Boolean = cached || loadCache
    def nextInt(): Int = {
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
    override def tryAdvance(c: java.util.function.IntConsumer): Boolean = useCache(c) || sp.tryAdvance(c)
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

    private[collection] def characteristics: Int = sp.characteristics
    private[collection] def estimateSize: Long = {
      val sz = sp.estimateSize
      if (cached && sz < Long.MaxValue && sz >= 0) sz + 1
      else sz
    }
    override def forEachRemaining(c: java.util.function.LongConsumer): Unit = {
      useCache(c)
      sp.forEachRemaining(c)
    }
    def hasNext: Boolean = cached || loadCache
    def nextLong(): Long = {
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
    override def tryAdvance(c: java.util.function.LongConsumer): Boolean = useCache(c) || sp.tryAdvance(c)
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
*/

  /* These adapter classes can wrap an AnyStepper of anumeric type into a possibly widened primitive Stepper type.
   * This provides a basis for more efficient stream processing on unboxed values provided that the original source
   * of the data is boxed. In other cases native implementations of the primitive stepper types should be provided
   * (see for example StepsIntArray and StepsWidenedByteArray). */

  private[convert] class UnboxingDoubleStepper(st: AnyStepper[Double]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingDoubleStepper(s)
    }
  }

  private[convert] class UnboxingIntStepper(st: AnyStepper[Int]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingIntStepper(s)
    }
  }

  private[convert] class UnboxingLongStepper(st: AnyStepper[Long]) extends LongStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): LongStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingLongStepper(s)
    }
  }

  private[convert] class UnboxingByteStepper(st: AnyStepper[Byte]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingByteStepper(s)
    }
  }

  private[convert] class UnboxingCharStepper(st: AnyStepper[Char]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingCharStepper(s)
    }
  }

  private[convert] class UnboxingShortStepper(st: AnyStepper[Short]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingShortStepper(s)
    }
  }

  private[convert] class UnboxingFloatStepper(st: AnyStepper[Float]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null
      else new UnboxingFloatStepper(s)
    }
  }
}
