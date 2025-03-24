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
import java.util.function.{Consumer, IntConsumer}
import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit
import scala.collection.{AnyStepper, Factory, IntStepper, SeqFactory, Stepper, StepperShape, mutable}
import scala.language.implicitConversions

/** A specialized Accumulator that holds `Int`s without boxing, see [[Accumulator]]. */
final class IntAccumulator
  extends Accumulator[Int, AnyAccumulator, IntAccumulator]
    with mutable.SeqOps[Int, AnyAccumulator, IntAccumulator]
    with Serializable {
  private[jdk] var current: Array[Int] = IntAccumulator.emptyIntArray
  private[jdk] var history: Array[Array[Int]] = IntAccumulator.emptyIntArrayArray

  private[jdk] def cumulative(i: Int) = { val x = history(i); x(x.length-2).toLong << 32 | (x(x.length-1)&0xFFFFFFFFL) }

  override protected[this] def className: String = "IntAccumulator"

  def efficientStepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S with EfficientSplit = {
    val st = new IntAccumulatorStepper(this)
    val r =
      if (shape.shape == StepperShape.IntShape) st
      else {
        assert(shape.shape == StepperShape.ReferenceShape, s"unexpected StepperShape: $shape")
        AnyStepper.ofParIntStepper(st)
      }
    r.asInstanceOf[S with EfficientSplit]
  }

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
  def addOne(a: Int): this.type = {
    totalSize += 1
    if (index+2 >= current.length) expand()
    current(index) = a
    index += 1
    this
  }

  /** Result collection consisting of all elements appended so far. */
  override def result(): IntAccumulator = this

  /** Removes all elements from `that` and appends them to this `IntAccumulator`. */
  def drain(that: IntAccumulator): Unit = {
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
        val n = math.max(4, 1 << (32 - jl.Integer.numberOfLeadingZeros(1 + hIndex + slots)))
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
  def apply(ix: Long): Int = {
    if (totalSize - ix <= index || hIndex == 0) current((ix - (totalSize - index)).toInt)
    else {
      val w = seekSlot(ix)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt)
    }
  }

  /** Retrieves the `ix`th element, using an `Int` index. */
  def apply(i: Int): Int = apply(i.toLong)

  def update(idx: Long, elem: Int): Unit = {
    if (totalSize - idx <= index || hIndex == 0) current((idx - (totalSize - index)).toInt) = elem
    else {
      val w = seekSlot(idx)
      history((w >>> 32).toInt)((w & 0xFFFFFFFFL).toInt) = elem
    }
  }

  def update(idx: Int, elem: Int): Unit = update(idx.toLong, elem)

  /** Returns an `Iterator` over the contents of this `IntAccumulator`. The `Iterator` is not specialized. */
  def iterator: Iterator[Int] = stepper.iterator

  override def foreach[U](f: Int => U): Unit = {
    val s = stepper
    while (s.hasStep) f(s.nextStep())
  }

  def map(f: Int => Int): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addOne(f(s.nextStep()))
    b.result()
  }

  def flatMap(f: Int => IterableOnce[Int]): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep)
      b.addAll(f(s.nextStep()))
    b.result()
  }

  def collect(pf: PartialFunction[Int, Int]): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      pf.runWith(b.addOne)(n)
    }
    b.result()
  }

  private def filterAccImpl(pred: Int => Boolean, not: Boolean): IntAccumulator = {
    val b = newSpecificBuilder
    val s = stepper
    while (s.hasStep) {
      val n = s.nextStep()
      if (pred(n) != not) b.addOne(n)
    }
    b.result()
  }

  override def filter(pred: Int => Boolean): IntAccumulator = filterAccImpl(pred, not = false)

  override def filterNot(pred: Int => Boolean): IntAccumulator = filterAccImpl(pred, not = true)

  override def forall(p: Int => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (!p(s.nextStep())) return false
    true
  }

  override def exists(p: Int => Boolean): Boolean = {
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) return true
    false
  }

  override def count(p: Int => Boolean): Int = {
    var r = 0
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  def countLong(p: Int => Boolean): Long = {
    var r = 0L
    val s = stepper
    while (s.hasStep)
      if (p(s.nextStep())) r += 1
    r
  }

  /** Copies the elements in this `IntAccumulator` into an `Array[Int]` */
  def toArray: Array[Int] = {
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
  override def toList: List[Int] = {
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

  /**
   * Copy the elements in this `IntAccumulator` to a specified collection.
   * Note that the target collection is not specialized.
   * Usage example: `acc.to(Vector)`
   */
  override def to[C1](factory: Factory[Int, C1]): C1 = {
    if (totalSize > Int.MaxValue) throw new IllegalArgumentException("Too many elements accumulated for a Scala collection: "+totalSize.toString)
    factory.fromSpecific(iterator)
  }

  override protected def fromSpecific(coll: IterableOnce[Int]): IntAccumulator = IntAccumulator.fromSpecific(coll)
  override protected def newSpecificBuilder: IntAccumulator = IntAccumulator.newBuilder
  override def iterableFactory: SeqFactory[AnyAccumulator] = AnyAccumulator

  override def empty: IntAccumulator = IntAccumulator.empty

  private def writeReplace(): AnyRef = new IntAccumulator.SerializationProxy(this)
}

object IntAccumulator extends collection.SpecificIterableFactory[Int, IntAccumulator] {
  private val emptyIntArray = new Array[Int](0)
  private val emptyIntArrayArray = new Array[Array[Int]](0)

  implicit def toJavaIntegerAccumulator(ia: IntAccumulator.type): collection.SpecificIterableFactory[jl.Integer, IntAccumulator] = IntAccumulator.asInstanceOf[collection.SpecificIterableFactory[jl.Integer, IntAccumulator]]

  import java.util.{function => jf}

  /** A `Supplier` of `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def supplier: jf.Supplier[IntAccumulator]  = () => new IntAccumulator

  /** A `BiConsumer` that adds an element to an `IntAccumulator`, suitable for use with `java.util.stream.IntStream`'s `collect` method. */
  def adder: jf.ObjIntConsumer[IntAccumulator] = (ac: IntAccumulator, a: Int) => ac addOne a

  /** A `BiConsumer` that adds a boxed `Int` to an `IntAccumulator`, suitable for use with `java.util.stream.Stream`'s `collect` method. */
  def boxedAdder: jf.BiConsumer[IntAccumulator, Int] = (ac: IntAccumulator, a: Int) => ac addOne a

  /** A `BiConsumer` that merges `IntAccumulator`s, suitable for use with `java.util.stream.IntStream`'s `collect` method.  Suitable for `Stream[Int]` also. */
  def merger: jf.BiConsumer[IntAccumulator, IntAccumulator] = (a1: IntAccumulator, a2: IntAccumulator) => a1 drain a2

  private def fromArray(a: Array[Int]): IntAccumulator = {
    val r = new IntAccumulator
    var i = 0
    while (i < a.length) { r addOne a(i); i += 1 }
    r
  }

  override def fromSpecific(it: IterableOnce[Int]): IntAccumulator = it match {
    case acc: IntAccumulator => acc
    case as: collection.immutable.ArraySeq.ofInt => fromArray(as.unsafeArray)
    case as: collection.mutable.ArraySeq.ofInt => fromArray(as.array) // this case ensures Array(1).to(Accumulator) doesn't box
    case _ => (new IntAccumulator).addAll(it)
  }

  override def empty: IntAccumulator = new IntAccumulator

  override def newBuilder: IntAccumulator = new IntAccumulator

  class SerializationProxy[A](@transient private val acc: IntAccumulator) extends Serializable {
    @transient private var result: IntAccumulator = _

    private def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      val size = acc.sizeLong
      out.writeLong(size)
      val st = acc.stepper
      while (st.hasStep)
        out.writeInt(st.nextStep())
    }

    private def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val res = new IntAccumulator()
      var elems = in.readLong()
      while (elems > 0) {
        res += in.readInt()
        elems -= 1L
      }
      result = res
    }

    private def readResolve(): AnyRef = result
  }
}

private[jdk] class IntAccumulatorStepper(private val acc: IntAccumulator) extends IntStepper with EfficientSplit {
  import java.util.Spliterator._

  private var h: Int = 0
  private var i: Int = 0
  private var a: Array[Int] = if (acc.hIndex > 0) acc.history(0) else acc.current
  private var n: Long = if (acc.hIndex > 0) acc.cumulative(0) else acc.index
  private var N: Long = acc.totalSize

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

  def characteristics: Int = ORDERED | SIZED | SUBSIZED | NONNULL

  def estimateSize: Long = N

  def hasStep: Boolean = N > 0

  def nextStep(): Int =
    if (N <= 0) throw new NoSuchElementException("next on empty Stepper")
    else {
      if (i >= n) loadMore()
      val ans = a(i)
      i += 1
      N -= 1
      ans
    }

  def trySplit(): IntStepper =
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

  override def spliterator[B >: Int]: Spliterator.OfInt = new IntStepper.IntStepperSpliterator(this) {
    // Overridden for efficiency
    override def tryAdvance(c: IntConsumer): Boolean =
      if (N <= 0) false
      else {
        if (i >= n) loadMore()
        c.accept(a(i))
        i += 1
        N -= 1
        true
      }

    // Overridden for efficiency
    override def tryAdvance(c: Consumer[_ >: jl.Integer]): Boolean = (c: AnyRef) match {
      case ic: IntConsumer => tryAdvance(ic)
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
    override def forEachRemaining(c: IntConsumer): Unit =
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
    override def forEachRemaining(c: Consumer[_ >: jl.Integer]): Unit = (c: AnyRef) match {
      case ic: IntConsumer => forEachRemaining(ic)
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
