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

package scala.jdk

import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit
import scala.collection.{Stepper, StepperShape, mutable}
import scala.language.implicitConversions

/** Accumulators are mutable sequences with two distinct features:
  *   - An accumulator can be appended efficiently to another
  *   - There are manually specialized Accumulators for `Int`, `Long` and `Double` that don't box
  *     the elements
  *
  * These two features make Accumulators a good candidate to collect the results of a parallel Java
  * stream pipeline into a Scala collection. The
  * [[scala.collection.convert.StreamExtensions.StreamHasToScala.toScala]] extension method on Java
  * streams (available by importing
  * [[scala.jdk.StreamConverters `scala.jdk.StreamConverters._`]]) is specialized for
  * Accumulators: they are built in parallel, the parts are merged efficiently.
  *
  * Building specialized Accumulators is handled transparently. As a user, using the
  * [[Accumulator]] object as a factory automatically creates an [[IntAccumulator]],
  * [[LongAccumulator]], [[DoubleAccumulator]] or [[AnyAccumulator]] depending on the element type.
  *
  * Note: to run the example, start the Scala REPL with `scala -Yrepl-class-based` to avoid
  * deadlocks, see [[https://github.com/scala/bug/issues/9076]].
  *
  * {{{
  *   scala> import scala.jdk.StreamConverters._
  *   import scala.jdk.StreamConverters._
  *
  *   scala> def isPrime(n: Int): Boolean = !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n % _ == 0))
  *   isPrime: (n: Int)Boolean
  *
  *   scala> val intAcc = (1 to 10000).asJavaParStream.filter(isPrime).toScala(scala.jdk.Accumulator)
  *   intAcc: scala.jdk.IntAccumulator = IntAccumulator(1, 3, 5, 7, 11, 13, 17, 19, ...
  *
  *   scala> val stringAcc = (1 to 100).asJavaParStream.mapToObj("<>" * _).toScala(Accumulator)
  *   stringAcc: scala.jdk.AnyAccumulator[String] = AnyAccumulator(<>, <><>, <><><>, ...
  * }}}
  *
  * There are two possibilities to process elements of a primitive Accumulator without boxing:
  * specialized operations of the Accumulator, or the Stepper interface. The most common collection
  * operations are overloaded or overridden in the primitive Accumulator classes, for example
  * [[IntAccumulator.map(f:Int=>Int)* IntAccumulator.map]] or [[IntAccumulator.exists]].
  * Thanks to Scala's function specialization,
  * `intAcc.exists(x => testOn(x))` does not incur boxing.
  *
  * The [[scala.collection.Stepper]] interface provides iterator-like `hasStep` and `nextStep` methods, and is
  * specialized for `Int`, `Long` and `Double`. The `intAccumulator.stepper` method creates an
  * [[scala.collection.IntStepper]] that yields the elements of the accumulator without boxing.
  *
  * Accumulators can hold more than `Int.MaxValue` elements. They have a [[sizeLong]] method that
  * returns the size as a `Long`. Note that certain operations defined in [[scala.collection.Seq]]
  * are implemented using [[length]], so they will not work correctly for large accumulators.
  *
  * The [[Accumulator]] class is a base class to share code between [[AnyAccumulator]] (for
  * reference types) and the manual specializations [[IntAccumulator]], [[LongAccumulator]] and
  * [[DoubleAccumulator]].
  */
abstract class Accumulator[@specialized(Double, Int, Long) A, +CC[X] <: mutable.Seq[X], +C <: mutable.Seq[A]]
  extends mutable.Seq[A]
    with mutable.Builder[A, C] {

  /**
   * Implementation Details
   *
   * Every subclass has two arrays
   *   - `current: Array[A]`
   *   - `history: Array[Array[A]]`
   *
   * Elements are added to `current` at [[index]] until it's full, then `current` is added to `history` at [[hIndex]].
   * [[nextBlockSize]] defines the size of the next `current`. See also [[cumulative]].
   */
  private[jdk] var index: Int = 0
  private[jdk] var hIndex: Int = 0
  private[jdk] var totalSize: Long = 0L

  /**
   * The total number of elements stored in the history up to `history(i)` (where `0 <= i < hIndex`).
   * This method is constant-time, the cumulative lengths are stored.
   *   - [[AnyAccumulator]] keeps a separate array to store the cumulative lengths.
   *   - [[LongAccumulator]] and [[DoubleAccumulator]] store the cumulative length at the last slot in every
   *     array in the history. Every array is allocated with 1 extra slot for this purpose. [[DoubleAccumulator]]
   *     converts the length to double for storing and back to long, which is correct for lengths that fit in the
   *     double's 52 fraction bits (so any collection that fits in memory).
   *   - [[IntAccumulator]] uses the last two slots in every array to store the cumulative length, every array is
   *     allocated with 1 extra slot. So `history(0)` has 17 slots of which the first 15 store elements.
   */
  private[jdk] def cumulative(i: Int): Long

  private[jdk] def nextBlockSize: Int = {
    if (totalSize < 32) 16
    else if (totalSize <= Int.MaxValue) {
      val bit = 64 - jl.Long.numberOfLeadingZeros(totalSize)
      1 << (bit - (bit >> 2))
    }
    else 1 << 24
  }

  protected def efficientStepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit

  final override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit =
    efficientStepper(shape)

  final override def length: Int =
    if (sizeLong < Int.MaxValue) sizeLong.toInt
    else throw new IllegalArgumentException(s"Size too large for an Int: $sizeLong")

  final override def knownSize: Int = if (sizeLong < Int.MaxValue) size else -1

  /** Size of the accumulated collection, as a `Long` */
  final def sizeLong: Long = totalSize

  /** Remove all accumulated elements from this accumulator. */
  def clear(): Unit = {
    index = 0
    hIndex = 0
    totalSize = 0L
  }

  private[jdk] def seekSlot(ix: Long): Long = {
    var lo = -1
    var hi = hIndex
    while (lo + 1 < hi) {
      val m = (lo + hi) >>> 1    // Shift allows division-as-unsigned, prevents overflow
      if (cumulative(m) > ix) hi = m
      else lo = m
    }
    (hi.toLong << 32) | (if (hi==0) ix else ix - cumulative(hi-1)).toInt
  }
}

/** Contains factory methods to build Accumulators.
  *
  * Note that the `Accumulator` object itself is not a factory, but it is implicitly convert to
  * a factory according to the element type, see [[Accumulator.toFactory]].
  *
  * This allows passing the `Accumulator` object as argument when a [[collection.Factory]], and
  * the implicit [[Accumulator.AccumulatorFactoryShape]] instance is used to build a specialized
  * Accumulator according to the element type:
  *
  * {{{
  *   scala> val intAcc = Accumulator(1,2,3)
  *   intAcc: scala.collection.convert.IntAccumulator = IntAccumulator(1, 2, 3)
  *
  *   scala> val anyAccc = Accumulator("K")
  *   anyAccc: scala.collection.convert.AnyAccumulator[String] = AnyAccumulator(K)
  *
  *   scala> val intAcc2 = List(1,2,3).to(Accumulator)
  *   intAcc2: scala.jdk.IntAccumulator = IntAccumulator(1, 2, 3)
  *
  *   scala> val anyAcc2 = List("K").to(Accumulator)
  *   anyAcc2: scala.jdk.AnyAccumulator[String] = AnyAccumulator(K)
  * }}}
  *
  * @define coll Accumulator
  * @define Coll `Accumulator`
  */
object Accumulator {
  implicit def toFactory[A, C](sa: Accumulator.type)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): collection.Factory[A, C] = canAccumulate.factory

  /** Creates a target $coll from an existing source collection
    *
    * @param source Source collection
    * @tparam A the type of the ${coll}’s elements
    * @tparam C the (inferred) specific type of the $coll
    * @return a new $coll with the elements of `source`
    */
  def from[A, C](source: IterableOnce[A])(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    source.iterator.to(canAccumulate.factory)

  /** An empty collection
    * @tparam A      the type of the ${coll}'s elements
    */
  def empty[A, C](implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    canAccumulate.empty

  /** Creates an $coll with the specified elements.
    * @tparam A     the type of the ${coll}'s elements
    * @tparam C     the (inferred) specific type of the $coll
    * @param elems  the elements of the created $coll
    * @return a new $coll with elements `elems`
    */
  def apply[A, C](elems: A*)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    canAccumulate.factory.fromSpecific(elems)

  /** Produces an $coll containing repeated applications of a function to a start value.
    *
    *  @param start the start value of the $coll
    *  @param len   the number of elements contained in the $coll
    *  @param f     the function that's repeatedly applied
    *  @return      an $coll with `len` values in the sequence `start, f(start), f(f(start)), ...`
    */
  def iterate[A, C](start: A, len: Int)(f: A => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(new collection.View.Iterate(start, len)(f))

  /** Produces an $coll that uses a function `f` to produce elements of type `A`
    * and update an internal state of type `S`.
    *
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    * @tparam C   Type (usually inferred) of the $coll
    * @return an $coll that produces elements using `f` until `f` returns `None`
    */
  def unfold[A, S, C](init: S)(f: S => Option[(A, S)])(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(new collection.View.Unfold(init)(f))

  /** Produces an $coll containing a sequence of increasing of integers.
    *
    *  @param start the first element of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @return  an $coll with values `start, start + 1, ..., end - 1`
    */
  def range[A: Integral, C](start: A, end: A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(collection.immutable.NumericRange(start, end, implicitly[Integral[A]].one))

  /** Produces an $coll containing equally spaced values in some integer interval.
    *  @param start the start value of the $coll
    *  @param end   the end value of the $coll (the first value NOT contained)
    *  @param step  the difference between successive elements of the $coll (must be positive or negative)
    *  @return      an $coll with values `start, start + step, ...` up to, but excluding `end`
    */
  def range[A: Integral, C](start: A, end: A, step: A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(collection.immutable.NumericRange(start, end, step))

  /**
    * @return A builder for $Coll objects.
    * @tparam A the type of the ${coll}’s elements
    * @tparam C the specific type of the $coll
    */
  def newBuilder[A, C](implicit canAccumulate: AccumulatorFactoryShape[A, C]): collection.mutable.Builder[A, C] =
    canAccumulate.factory.newBuilder

  /** Produces an $coll containing the results of some element computation a number of times.
    *  @param   n  the number of elements contained in the $coll.
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n` evaluations of `elem`.
    */
  def fill[A, C](n: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(new collection.View.Fill(n)(elem))

  /** Produces a two-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[C] = 
    fill(n1)(fill(n2)(elem)(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[C])

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int, n3: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[C]] =
    fill(n1)(fill(n2, n3)(elem)(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[C]])

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]] =
    fill(n1)(fill(n2, n3, n4)(elem)(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[C]]])

  /** Produces a five-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2 x n3 x n4 x n5` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]] =
    fill(n1)(fill(n2, n3, n4, n5)(elem)(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]])

  /** Produces an $coll containing values of a given function over a range of integer values starting from 0.
    *  @param  n   The number of elements in the $coll
    *  @param  f   The function computing element values
    *  @return An $coll consisting of elements `f(0), ..., f(n -1)`
    */
  def tabulate[A, C](n: Int)(f: Int => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C =
    from(new collection.View.Tabulate(n)(f))

  /** Produces a two-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   f   The function computing element values
    *  @return An $coll consisting of elements `f(i1, i2)`
    *          for `0 <= i1 < n1` and `0 <= i2 < n2`.
    */
  def tabulate[A, C](n1: Int, n2: Int)(f: (Int, Int) => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[C] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _))(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[C])

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   f   The function computing element values
    *  @return An $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A, C](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[C]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _))(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[C]])

  /** Produces a four-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   f   The function computing element values
    *  @return An $coll consisting of elements `f(i1, i2, i3, i4)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, and `0 <= i4 < n4`.
    */
  def tabulate[A, C](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _))(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[C]]])

  /** Produces a five-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   n5  the number of elements in the 5th dimension
    *  @param   f   The function computing element values
    *  @return An $coll consisting of elements `f(i1, i2, i3, i4, i5)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, `0 <= i3 < n3`, `0 <= i4 < n4`, and `0 <= i5 < n5`.
    */
  def tabulate[A, C](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]] =
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _))(canAccumulate))(AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]])

  /** Concatenates all argument collections into a single $coll.
   *
   *  @param xss the collections that are to be concatenated.
   *  @return the concatenation of all the collections.
   */
  def concat[A, C](xss: Iterable[A]*)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): C = 
    if (xss.isEmpty) canAccumulate.empty
    else {
      val b = canAccumulate.factory.newBuilder
      xss.foreach(b ++= _)
      b.result()
    }

  /** An implicit `AccumulatorFactoryShape` is used in Accumulator factory method to return
    * specialized variants according to the element type.
    */
  sealed trait AccumulatorFactoryShape[A, C] {
    def factory: collection.Factory[A, C]
    def empty: C
  }

  object AccumulatorFactoryShape extends LowPriorityAccumulatorFactoryShape {
    implicit val doubleAccumulatorFactoryShape: AccumulatorFactoryShape[Double, DoubleAccumulator] = new AccumulatorFactoryShape[Double, DoubleAccumulator] {
      def factory: collection.Factory[Double, DoubleAccumulator] = DoubleAccumulator
      def empty: DoubleAccumulator = DoubleAccumulator.empty
    }

    implicit val intAccumulatorFactoryShape: AccumulatorFactoryShape[Int, IntAccumulator] = new AccumulatorFactoryShape[Int, IntAccumulator] {
      def factory: collection.Factory[Int, IntAccumulator] = IntAccumulator
      def empty: IntAccumulator = IntAccumulator.empty
    }

    implicit val longAccumulatorFactoryShape: AccumulatorFactoryShape[Long, LongAccumulator] = new AccumulatorFactoryShape[Long, LongAccumulator] {
      def factory: collection.Factory[Long, LongAccumulator] = LongAccumulator
      def empty: LongAccumulator = LongAccumulator.empty
    }

    implicit val jDoubleAccumulatorFactoryShape: AccumulatorFactoryShape[jl.Double, DoubleAccumulator] = doubleAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[jl.Double, DoubleAccumulator]]
    implicit val jIntegerAccumulatorFactoryShape: AccumulatorFactoryShape[jl.Integer, IntAccumulator] = intAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[jl.Integer, IntAccumulator]]
    implicit val jLongAccumulatorFactoryShape: AccumulatorFactoryShape[jl.Long, LongAccumulator] = longAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[jl.Long, LongAccumulator]]
  }

  sealed trait LowPriorityAccumulatorFactoryShape {
    implicit def anyAccumulatorFactoryShape[A]: AccumulatorFactoryShape[A, AnyAccumulator[A]] = anyAccumulatorFactoryShapePrototype.asInstanceOf[AccumulatorFactoryShape[A, AnyAccumulator[A]]]

    private val anyAccumulatorFactoryShapePrototype = new AccumulatorFactoryShape[AnyRef, AnyAccumulator[AnyRef]] {
      def factory: collection.Factory[AnyRef, AnyAccumulator[AnyRef]] = collection.IterableFactory.toFactory(AnyAccumulator)
      def empty: AnyAccumulator[AnyRef] = AnyAccumulator.empty[AnyRef]
    }
  }
}
