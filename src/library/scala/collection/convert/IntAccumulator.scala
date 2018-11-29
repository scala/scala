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

/** A `IntAccumulator` is a low-level collection specialized for gathering
 * elements in parallel and then joining them in order by merging them.
 * This is a manually specialized variant of `Accumulator` with no actual
 * subclassing relationship with `Accumulator`.
 */
final class IntAccumulator extends AccumulatorOps[Int, IntAccumulator] { self =>
  private[convert] var current: Array[Int] = IntAccumulator.emptyIntArray
  private[convert] var history: Array[Array[Int]] = IntAccumulator.emptyIntArrayArray

  private[convert] def cumulative(i: Int) = { val x = history(i); x(x.length-2).toLong << 32 | (x(x.length-1)&0xFFFFFFFFL) }

  private def expand(): Unit = {
    if (index > 0) {
      val cuml = (if (hIndex > 0) cumulative(hIndex-1) else 0) + index
      current(current.length-2) = (cuml >>> 32).toInt
      current(current.length-1) = (cuml & 0xFFFFFFFFL).toInt
      if (hIndex >= history.length) hExpand()
      history(hIndex) = current
      hIndex += 1
    }
    current = new Array[Int](nextBlockSize+1)
    index = 0
  }

  private def hExpand(): Unit = {
    if (hIndex == 0) history = new Array[Array[Int]](4)
    else history = java.util.Arrays.copyOf(history, history.length << 1)
  }

  /** Appends an element to this `IntAccumulator`. */
  final def +=(a: Int): Unit = {
    totalSize += 1
    if (index+2 >= current.length) expand()
    current(index) = a
    index += 1
  }

  /** Removes all elements from `that` and appends them to this `IntAccumulator`. */
  final def drain(that: IntAccumulator): Unit = {
    var h = 0
    var prev = 0L
    var more = true
    while (more && h < that.hIndex) {
      val cuml = that.cumulative(h)
      val n = (cuml - prev).toInt
      if (current.length - index - 2 >= n) {
        System.arraycopy(that.history(h), 0, current, index, n)
        prev = cuml
        index += n
        h += 1
      }
      else more = false
    }
    if (h >= that.hIndex && current.length - index - 2 >= that.index) {
      if (that.index > 0) System.arraycopy(that.current, 0, current, index, that.index)
      index += that.index
    }
    else {
      val slots = (if (index > 0) 1 else 0) + that.hIndex - h
      if (hIndex + slots > history.length) {
        val n = math.max(4, 1 << (32 - java.lang.Integer.numberOfLeadingZeros(1 + hIndex + slots)))
        history = java.util.Arrays.copyOf(history, n)
      }
      var pv = if (hIndex > 0) cumulative(hIndex-1) else 0L
      if (index > 0) {
        val x =
          if (index < (current.length >>> 3) && current.length - 1 > 32) {
            val ans = java.util.Arrays.copyOf(current, index + 2)
            ans(ans.length - 2) = current(current.length - 2)
            ans(ans.length - 1) = current(current.length - 1)
            ans
          }
          else current
        pv = pv + index
        x(x.length - 2) = (pv >>> 32).toInt
        x(x.length - 1) = (pv & 0xFFFFFFFFL).toInt
        history(hIndex) = x
        hIndex += 1
      }
      while (h < that.hIndex) {
        val cuml = that.cumulative(h)
        pv = pv + cuml - prev
        prev = cuml
        val x = that.history(h)
        x(x.length - 2) = (pv >>> 32).toInt
        x(x.length - 1) = (pv & 0xFFFFFFFFL).toInt
        history(hIndex) = x
        h += 1
        hIndex += 1
      }
      index = that.index
      current = that.current
    }
    totalSize += that.totalSize
    that.clear()
  }

  override def clear(): Unit = {
    super.clear()
    current = IntAccumulator.emptyIntArray
    history = IntAccumulator.emptyIntArrayArray
  }

  /** Retrieves the `ix`th element. */
  final def apply(ix: Long): Int = {
    if (totalSize - ix <= index || hIndex == 0) current((ix - (totalSize - index)).toInt)
    else {
      val w = seekSlot(ix)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt)
    }
  }

  /** Retrieves the `ix`th element, using an `Int` index. */
  final def apply(i: Int): Int = apply(i.toLong)

  /** Returns an `IntStepper` over the contents of this `IntAccumulator` */
  final def stepper: IntStepper = new IntAccumulatorStepper(this)

  /** Returns an `Iterator` over the contents of this `IntAccumulator`. The `Iterator` is not specialized. */
  final def iterator = stepper.iterator

  /** Returns a `java.util.Spliterator.OfInt` over the contents of this `IntAccumulator`*/
  final def spliterator: java.util.Spliterator.OfInt = stepper

  /** Produces a sequential Java 8 `IntStream` over the elements of this `IntAccumulator`*/
  final def seqStream: java.util.stream.IntStream = java.util.stream.StreamSupport.intStream(spliterator, false)

  /** Produces a parallel Java 8 `IntStream` over the elements of this `IntAccumulator`*/
  final def parStream: java.util.stream.IntStream = java.util.stream.StreamSupport.intStream(spliterator, true)

  /** Copies the elements in this `IntAccumulator` into an `Array[Int]` */
  final def toArray = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for an array: "+totalSize.toString)
    val a = new Array[Int](totalSize.toInt)
    var j = 0
    var h = 0
    var pv = 0L
    while (h < hIndex) {
      val x = history(h)
      val cuml = cumulative(h)
      val n = (cuml - pv).toInt
      pv = cuml
      System.arraycopy(x, 0, a, j, n)
      j += n
      h += 1
    }
    System.arraycopy(current, 0, a, j, index)
    j += index
    a
  }

  /** Copies the elements in this `IntAccumulator` to a `List` */
  final def toList: List[Int] = {
    var ans: List[Int] = Nil
    var i = index - 1
    while (i >= 0) {
      ans = current(i) :: ans
      i -= 1
    }
    var h = hIndex - 1
    while (h >= 0) {
      val a = history(h)
      i = (cumulative(h) - (if (h == 0) 0L else cumulative(h-1))).toInt - 1
      while (i >= 0) {
        ans = a(i) :: ans
        i -= 1
      }
      h -= 1
    }
    ans
  }

  /** Copies the elements in this `IntAccumulator` to a specified collection.
   * Note that the target collection is not specialized.
   * Usage example: `acc.to[Vector]`
   */
  final def to[Coll[_]](implicit factory: collection.Factory[Int, Coll[Int]]): Coll[Int] = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for a Scala collection: "+totalSize.toString)
    val b = factory.newBuilder
    b.sizeHint(totalSize.toInt)
    var h = 0
    var pv = 0L
    while (h < hIndex) {
      val x = history(h)
      val cuml = cumulative(h)
      val n = cuml - pv
      pv = cuml
      var i = 0
      while (i < n) {
        b += x(i)
        i += 1
      }
      h += 1
    }
    var i = 0
    while (i < index) {
      b += current(i)
      i += 1
    }
    b.result()
  }
}

object IntAccumulator {
  private val emptyIntArray = new Array[Int](0)
  private val emptyIntArrayArray = new Array[Array[Int]](0)

  /** A `Supplier` of `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def supplier = new java.util.function.Supplier[IntAccumulator]{ def get: IntAccumulator = new IntAccumulator }

  /** A `BiConsumer` that adds an element to an `Accumulator`, suitable for use with `java.util.stream.IntStream`'s `collect` method. */
  def adder = new java.util.function.ObjIntConsumer[IntAccumulator]{ def accept(ac: IntAccumulator, a: Int): Unit = { ac += a } }

  /** A `BiConsumer` that adds a boxed `Int` to an `IntAccumulator`, suitable for use with `java.util.stream.Stream`'s `collect` method. */
  def boxedAdder = new java.util.function.BiConsumer[IntAccumulator, Int]{ def accept(ac: IntAccumulator, a: Int): Unit = { ac += a } }

  /** A `BiConsumer` that merges `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def merger = new java.util.function.BiConsumer[IntAccumulator, IntAccumulator]{ def accept(a1: IntAccumulator, a2: IntAccumulator): Unit = { a1 drain a2 } }

  /** Builds an `IntAccumulator` from any `Int`-valued `IterableOnce` */
  def from[A](source: IterableOnce[Int]) = {
    val a = new IntAccumulator
    source.iterator.foreach(a += _)
    a
  }
}

private[convert] class IntAccumulatorStepper(private val acc: IntAccumulator) extends IntStepper {
  import java.util.Spliterator._

  private var h = 0
  private var i = 0
  private var a = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N = acc.totalSize

  private def duplicateSelf(limit: Long): IntAccumulatorStepper = {
    val ans = new IntAccumulatorStepper(acc)
    ans.h = h
    ans.i = i
    ans.a = a
    ans.n = n
    ans.N = limit
    ans
  }

  private def loadMore(): Unit = {
    h += 1
    if (h < acc.hIndex) { a = acc.history(h); n = acc.cumulative(h) - acc.cumulative(h-1) }
    else { a = acc.current; n = acc.index }
    i = 0
  }

  def characteristics = ORDERED | SIZED | SUBSIZED | NONNULL

  def estimateSize = N

  def hasNext = N > 0

  def nextInt(): Int =
    if (N <= 0) throw new NoSuchElementException("next on empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i)
      i += 1
      N -= 1
      ans
    }

  // Overridden for efficiency
  override def tryStep(f: Int => Unit): Boolean =
    if (N <= 0) false
    else {
      if (i >= n) loadMore()
      f(a(i))
      i += 1
      N -= 1
      true
    }

  // Overridden for efficiency
  override def tryAdvance(f: java.util.function.IntConsumer): Boolean =
    if (N <= 0) false
    else {
      if (i >= n) loadMore()
      f.accept(a(i))
      i += 1
      N -= 1
      true
    }

  // Overridden for efficiency
  override def foreach(f: Int => Unit): Unit = {
    while (N > 0) {
      if (i >= n) loadMore()
      val i0 = i
      if ((n-i) > N) n = i + N.toInt
      while (i < n) {
        f(a(i))
        i += 1
      }
      N -= (n - i0)
    }
  }

  // Overridden for efficiency
  override def forEachRemaining(f: java.util.function.IntConsumer): Unit = {
    while (N > 0) {
      if (i >= n) loadMore()
      val i0 = i
      if ((n-i) > N) n = i + N.toInt
      while (i < n) {
        f.accept(a(i))
        i += 1
      }
      N -= (n - i0)
    }
  }

  def substep(): IntStepper =
    if (N <= 1) null
    else {
      val half = N >> 1
      val M = (if (h <= 0) 0L else acc.cumulative(h-1)) + i
      val R = M + half
      val ans = duplicateSelf(half)
      if (h < acc.hIndex) {
        val w = acc.seekSlot(R)
        h = (w >>> 32).toInt
        if (h < acc.hIndex) {
          a = acc.history(h)
          n = acc.cumulative(h) - (if (h > 0) acc.cumulative(h-1) else 0)
        }
        else {
          a = acc.current
          n = acc.index
        }
        i = (w & 0xFFFFFFFFL).toInt
      }
      else i += half.toInt
      N -= half
      ans
    }
}
