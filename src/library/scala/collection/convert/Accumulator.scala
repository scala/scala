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

import scala.collection.convert.impl.AccumulatorFactoryShape
import scala.collection.mutable


/**
 * Base class to share code between the [[AnyAccumulator]] class (for reference types) and the manual
 * specializations [[IntAccumulator]], [[LongAccumulator]] and [[DoubleAccumulator]].
 */
trait Accumulator[@specialized(Double, Int, Long) A, +CC[_], +C]
  extends mutable.Iterable[A]
    with mutable.Builder[A, C] {
  private[convert] var index: Int = 0
  private[convert] var hIndex: Int = 0
  private[convert] var totalSize: Long = 0L
  private[convert] def cumulative(i: Int): Long

  private[convert] def nextBlockSize: Int = {
    if (totalSize < 32) 16
    else if (totalSize <= Int.MaxValue) {
      val bit = 64 - java.lang.Long.numberOfLeadingZeros(totalSize)
      1 << (bit - (bit >> 2))
    }
    else 1 << 24
  }

  final override def size: Int =
    if (longSize < Int.MaxValue) longSize.toInt
    else throw new IllegalArgumentException(s"Size too large for an Int: $longSize")

  final override def knownSize: Int = size

  /** Size of the accumulated collection, as a `Long` */
  final def longSize: Long = totalSize

  /** Remove all accumulated elements from this accumulator. */
  def clear(): Unit = {
    index = 0
    hIndex = 0
    totalSize = 0L
  }

  private[convert] def seekSlot(ix: Long): Long = {
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

/** Contains factory methods to build Accumulators. Depending on the element types, a generic
  * [[AnyAccumulator]] or a specialized primitive Accumulator like [[IntAccumulator]] is created.
  * {{{
  *   scala> val intAcc = Accumulator(1,2,3)
  *   intAcc: scala.collection.convert.IntAccumulator = IntAccumulator(1, 2, 3)
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
    fill(n1)(fill(n2)(elem)(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[C])

  /** Produces a three-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2 x n3` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int, n3: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[C]] =
    fill(n1)(fill(n2, n3)(elem)(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[C]])

  /** Produces a four-dimensional $coll containing the results of some element computation a number of times.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   n4  the number of elements in the 4th dimension
    *  @param   elem the element computation
    *  @return  An $coll that contains the results of `n1 x n2 x n3 x n4` evaluations of `elem`.
    */
  def fill[A, C](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]] =
    fill(n1)(fill(n2, n3, n4)(elem)(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[C]]])

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
    fill(n1)(fill(n2, n3, n4, n5)(elem)(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]])

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
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _))(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[C])

  /** Produces a three-dimensional $coll containing values of a given function over ranges of integer values starting from 0.
    *  @param   n1  the number of elements in the 1st dimension
    *  @param   n2  the number of elements in the 2nd dimension
    *  @param   n3  the number of elements in the 3rd dimension
    *  @param   f   The function computing element values
    *  @return An $coll consisting of elements `f(i1, i2, i3)`
    *          for `0 <= i1 < n1`, `0 <= i2 < n2`, and `0 <= i3 < n3`.
    */
  def tabulate[A, C](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => A)(implicit canAccumulate: AccumulatorFactoryShape[A, C]): AnyAccumulator[AnyAccumulator[C]] =
    tabulate(n1)(i1 => tabulate(n2, n3)(f(i1, _, _))(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[C]])

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
    tabulate(n1)(i1 => tabulate(n2, n3, n4)(f(i1, _, _, _))(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[C]]])

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
    tabulate(n1)(i1 => tabulate(n2, n3, n4, n5)(f(i1, _, _, _, _))(canAccumulate))(impl.AccumulatorFactoryShape.anyAccumulatorFactoryShape[AnyAccumulator[AnyAccumulator[AnyAccumulator[C]]]])

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
}
