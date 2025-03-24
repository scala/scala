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

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.util.Spliterator
import java.util.function.{Consumer, DoubleConsumer}
import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit
import scala.collection.{AnyStepper, DoubleStepper, Factory, SeqFactory, Stepper, StepperShape, mutable}
import scala.language.implicitConversions

/** A specialized Accumulator that holds `Double`s without boxing, see [[Accumulator]]. */
final class DoubleAccumulator
  extends Accumulator[Double, AnyAccumulator, DoubleAccumulator]
    with mutable.SeqOps[Double, AnyAccumulator, DoubleAccumulator]
    with Serializable {
  private[jdk] var current: Array[Double] = DoubleAccumulator.emptyDoubleArray
  private[jdk] var history: Array[Array[Double]] = DoubleAccumulator.emptyDoubleArrayArray

  private[jdk] def cumulative(i: Int) = { val x = history(i); x(x.length-1).toLong }

  override protected[this] def className: String = "DoubleAccumulator"

  def efficientStepper[S <: Stepper[_]](implicit shape: StepperShape[Double, S]): S with EfficientSplit = {
    val st = new DoubleAccumulatorStepper(this)
    val r =
      if (shape.shape == StepperShape.DoubleShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParDoubleStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
  }

  private def expand(): Unit = {
    if (index > 0) {
      current(current.length-1) = (if (hIndex > 0) { val x = history(hIndex-1); x(x.length-1) } else 0) + index
      if (hIndex >= history.length) hExpand()
      history(hIndex) = current
      hIndex += 1
    }
    current = new Array[Double](nextBlockSize+1)
    index = 0
  }

  private def hExpand(): Unit = {
    if (hIndex == 0) history = new Array[Array[Double]](4)
    else history = java.util.Arrays.copyOf(history, history.length << 1)
  }

  /** Appends an element to this `DoubleAccumulator`. */
  def addOne(a: Double): this.type = {
    totalSize += 1
    if (index+1 >= current.length) expand()
    current(index) = a
    index += 1
    this
  }

  /** Result collection consisting of all elements appended so far. */
  override def result(): DoubleAccumulator = this

  /** Removes all elements from `that` and appends them to this `DoubleAccumulator`. */
  def drain(that: DoubleAccumulator): Unit = {
    var h = 0
    var prev = 0L
    var more = true
    while (more && h < that.hIndex) {
      val cuml = that.cumulative(h)
      val n = (cuml - prev).toInt
      if (current.length - index - 1 >= n) {
        System.arraycopy(that.history(h), 0, current, index, n)
        prev = cuml
        index += n
        h += 1
      }
      else more = false
    }
    if (h >= that.hIndex && current.length - index - 1>= that.index) {
      if (that.index > 0) System.arraycopy(that.current, 0, current, index, that.index)
      index += that.index
    }
    else {
      val slots = (if (index > 0) 1 else 0) + that.hIndex - h
      if (hIndex + slots > history.length) {
        val n = math.max(4, 1 << (32 - jl.Integer.numberOfLeadingZeros(1 + hIndex + slots)))
        history = java.util.Arrays.copyOf(history, n)
      }
      var pv = if (hIndex > 0) cumulative(hIndex-1) else 0L
      if (index > 0) {
        val x =
          if (index < (current.length >>> 3) && current.length - 1 > 32) {
            val ans = java.util.Arrays.copyOf(current, index + 1)
            ans(ans.length - 1) = current(current.length - 1)
            ans
          }
          else current
        pv = pv + index
        x(x.length - 1) = pv.toDouble // see comment on Accumulator.cumulative
        history(hIndex) = x
        hIndex += 1
      }
      while (h < that.hIndex) {
        val cuml = that.cumulative(h)
        pv = pv + cuml - prev
        prev = cuml
        val x = that.history(h)
        x(x.length - 1) = pv.toDouble // see comment on Accumulator.cumulative
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
    current = DoubleAccumulator.emptyDoubleArray
    history = DoubleAccumulator.emptyDoubleArrayArray
  }

  /** Retrieves the `ix`th element. */
  def apply(ix: Long): Double = {
    if (totalSize - ix <= index || hIndex == 0) current((ix - (totalSize - index)).toInt)
    else {
      val w = seekSlot(ix)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt)
    }
  }

  /** Retrieves the `ix`th element, using an `Int` index. */
  def apply(i: Int): Double = apply(i.toLong)

  def update(idx: Long, elem: Double): Unit = {
    if (totalSize - idx <= index || hIndex == 0) current((idx - (totalSize - index)).toInt) = elem
    else {
      val w = seekSlot(idx)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt) = elem
    }
  }

  def update(idx: Int, elem: Double): Unit = update(idx.toLong, elem)

  /** Returns an `Iterator` over the contents of this `DoubleAccumulator`. The `Iterator` is not specialized. */
  def iterator: Iterator[Double] = stepper.iterator

  override def foreach[U](f: Double => U): Unit = {
    val s = stepper
    while (s.hasStep) f(s.nextStep())
  }

  def map(f: Double => Double): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addOne(f(s.nextStep()))
    b.result()
  }

  def flatMap(f: Double => IterableOnce[Double]): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addAll(f(s.nextStep()))
    b.result()
  }

  def collect(pf: PartialFunction[Double, Double]): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      pf.runWith(b.addOne)(n)
    }
    b.result()
  }

  private def filterAccImpl(pred: Double => Boolean, not: Boolean): DoubleAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      if (pred(n) != not) b.addOne(n)
    }
    b.result()
  }

  override def filter(pred: Double => Boolean): DoubleAccumulator = filterAccImpl(pred, not = false)

  override def filterNot(pred: Double => Boolean): DoubleAccumulator = filterAccImpl(pred, not = true)

  override def forall(p: Double => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (!p(s.nextStep())) return false
    true
  }

  override def exists(p: Double => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) return true
    false
  }

  override def count(p: Double => Boolean): Int = {
    var r = 0
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  def countLong(p: Double => Boolean): Long = {
    var r = 0L
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Copies the elements in this `DoubleAccumulator` into an `Array[Double]` */
  def toArray: Array[Double] = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for an array: "+totalSize.toString)
    val a = new Array[Double](totalSize.toInt)
    var j = 0
    var h = 0
    var pv = 0L
    while (h < hIndex) {
      val x = history(h)
      val cuml = x(x.length-1).toLong
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

  /** Copies the elements in this `DoubleAccumulator` to a `List` */
  override def toList: List[Double] = {
    var ans: List[Double] = Nil
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

  /**
   * Copy the elements in this `DoubleAccumulator` to a specified collection.
   * Note that the target collection is not specialized.
   * Usage example: `acc.to(Vector)`
   */
  override def to[C1](factory: Factory[Double, C1]): C1 = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for a Scala collection: "+totalSize.toString)
    factory.fromSpecific(iterator)
  }

  override protected def fromSpecific(coll: IterableOnce[Double]): DoubleAccumulator = DoubleAccumulator.fromSpecific(coll)
  override protected def newSpecificBuilder: DoubleAccumulator = DoubleAccumulator.newBuilder
  override def iterableFactory: SeqFactory[AnyAccumulator] = AnyAccumulator

  override def empty: DoubleAccumulator = DoubleAccumulator.empty

  private def writeReplace(): AnyRef = new DoubleAccumulator.SerializationProxy(this)
}

object DoubleAccumulator extends collection.SpecificIterableFactory[Double, DoubleAccumulator] {
  private val emptyDoubleArray = new Array[Double](0)
  private val emptyDoubleArrayArray = new Array[Array[Double]](0)

  implicit def toJavaDoubleAccumulator(ia: DoubleAccumulator.type): collection.SpecificIterableFactory[jl.Double, DoubleAccumulator] = DoubleAccumulator.asInstanceOf[collection.SpecificIterableFactory[jl.Double, DoubleAccumulator]]

  import java.util.{function => jf}

  /** A `Supplier` of `DoubleAccumulator`s, suitable for use with `java.util.stream.DoubleStream`'s `collect` method.  Suitable for `Stream[Double]` also. */
  def supplier: jf.Supplier[DoubleAccumulator]  = () => new DoubleAccumulator

  /** A `BiConsumer` that adds an element to an `DoubleAccumulator`, suitable for use with `java.util.stream.DoubleStream`'s `collect` method. */
  def adder: jf.ObjDoubleConsumer[DoubleAccumulator] = (ac: DoubleAccumulator, a: Double) => ac addOne a

  /** A `BiConsumer` that adds a boxed `Double` to an `DoubleAccumulator`, suitable for use with `java.util.stream.Stream`'s `collect` method. */
  def boxedAdder: jf.BiConsumer[DoubleAccumulator, Double] = (ac: DoubleAccumulator, a: Double) => ac addOne a

  /** A `BiConsumer` that merges `DoubleAccumulator`s, suitable for use with `java.util.stream.DoubleStream`'s `collect` method.  Suitable for `Stream[Double]` also. */
  def merger: jf.BiConsumer[DoubleAccumulator, DoubleAccumulator] = (a1: DoubleAccumulator, a2: DoubleAccumulator) => a1 drain a2

  private def fromArray(a: Array[Double]): DoubleAccumulator = {
    val r = new DoubleAccumulator
    var i = 0
    while (i < a.length) { r addOne a(i); i += 1 }
    r
  }

  override def fromSpecific(it: IterableOnce[Double]): DoubleAccumulator = it match {
    case acc: DoubleAccumulator => acc
    case as: collection.immutable.ArraySeq.ofDouble => fromArray(as.unsafeArray)
    case as: collection.mutable.ArraySeq.ofDouble => fromArray(as.array) // this case ensures Array(1).to(Accumulator) doesn't box
    case _ => (new DoubleAccumulator).addAll(it)
  }

  override def empty: DoubleAccumulator = new DoubleAccumulator

  override def newBuilder: DoubleAccumulator = new DoubleAccumulator

  class SerializationProxy[A](@transient private val acc: DoubleAccumulator) extends Serializable {
    @transient private var result: DoubleAccumulator = _

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val size = acc.sizeLong
      out.writeLong(size)
      val st = acc.stepper
      while (st.hasStep)
        out.writeDouble(st.nextStep())
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val res = new DoubleAccumulator()
      var elems = in.readLong()
      while (elems > 0) {
        res += in.readDouble()
        elems -= 1L
      }
      result = res
    }

    private def readResolve(): AnyRef = result
  }
}

private[jdk] class DoubleAccumulatorStepper(private val acc: DoubleAccumulator) extends DoubleStepper with EfficientSplit {
  import java.util.Spliterator._

  private var h: Int = 0
  private var i: Int = 0
  private var a: Array[Double] = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n: Long = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N: Long = acc.totalSize

  private def duplicateSelf(limit: Long): DoubleAccumulatorStepper = {
    val ans = new DoubleAccumulatorStepper(acc)
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

  def characteristics: Int = ORDERED | SIZED | SUBSIZED | NONNULL

  def estimateSize: Long = N

  def hasStep: Boolean = N > 0

  def nextStep(): Double =
    if (n <= 0) throw new NoSuchElementException("next on empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i)
      i += 1
      N -= 1
      ans
    }

  def trySplit(): DoubleStepper =
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

  override def spliterator[B >: Double]: Spliterator.OfDouble = new DoubleStepper.DoubleStepperSpliterator(this) {
    // Overridden for efficiency
    override def tryAdvance(c: DoubleConsumer): Boolean =
      if (N <= 0) false
      else {
        if (i >= n) loadMore()
        c.accept(a(i))
        i += 1
        N -= 1
        true
      }

    // Overridden for efficiency
    override def tryAdvance(c: Consumer[_ >: jl.Double]): Boolean = (c: AnyRef) match {
      case ic: DoubleConsumer => tryAdvance(ic)
      case _ =>
        if (N <= 0) false
        else {
          if (i >= n) loadMore()
          c.accept(a(i))
          i += 1
          N -= 1
          true
        }
    }

    // Overridden for efficiency
    override def forEachRemaining(c: DoubleConsumer): Unit =
      while (N > 0) {
        if (i >= n) loadMore()
        val i0 = i
        if ((n-i) > N) n = i + N.toInt
        while (i < n) {
          c.accept(a(i))
          i += 1
        }
        N -= (n - i0)
      }

    // Overridden for efficiency
    override def forEachRemaining(c: Consumer[_ >: jl.Double]): Unit = (c: AnyRef) match {
      case ic: DoubleConsumer => forEachRemaining(ic)
      case _ =>
        while (N > 0) {
          if (i >= n) loadMore()
          val i0 = i
          if ((n-i) > N) n = i + N.toInt
          while (i < n) {
            c.accept(a(i))
            i += 1
          }
          N -= (n - i0)
        }
    }
  }
}
