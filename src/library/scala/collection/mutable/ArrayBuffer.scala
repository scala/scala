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

package scala
package collection
package mutable

import java.util.Arrays

import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializable

/** An implementation of the `Buffer` class using an array to
  *  represent the assembled sequence internally. Append, update and random
  *  access take constant time (amortized time). Prepends and removes are
  *  linear in the buffer size.
  *
  *  @since   1
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#array-buffers "Scala's Collection Library overview"]]
  *  section on `Array Buffers` for more information.

  *
  *  @tparam A    the type of this arraybuffer's elements.
  *
  *  @define Coll `mutable.ArrayBuffer`
  *  @define coll array buffer
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
class ArrayBuffer[A] private (initialElements: Array[AnyRef], initialSize: Int)
  extends AbstractBuffer[A]
    with IndexedBuffer[A]
    with IndexedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with StrictOptimizedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with IterableFactoryDefaults[A, ArrayBuffer]
    with DefaultSerializable {

  def this() = this(new Array[AnyRef](ArrayBuffer.DefaultInitialSize), 0)

  def this(initialSize: Int) = this(new Array[AnyRef](initialSize max 1), 0)

  protected[collection] var array: Array[AnyRef] = initialElements
  protected var size0 = initialSize

  override def stepper[B >: A, S <: Stepper[_]](implicit shape: StepperShape[B, S]): S with EfficientSplit = {
    import scala.collection.convert.impl._
    shape.parUnbox(new ObjectArrayStepper(array, 0, length).asInstanceOf[AnyStepper[B] with EfficientSplit])
  }

  override def knownSize: Int = super[IndexedSeqOps].knownSize

  /** Ensure that the internal array has at least `n` cells. */
  protected def ensureSize(n: Int): Unit =
    array = ArrayBuffer.ensureSize(array, size0, n)

  def sizeHint(size: Int): Unit =
    if(size > length && size >= 1) ensureSize(size)

  /** Reduce length to `n`, nulling out all dropped elements */
  private def reduceToSize(n: Int): Unit = {
    Arrays.fill(array, n, size0, null)
    size0 = n
  }

  /** Trims the ArrayBuffer to an appropriate size for the current number of elements (rounding up to the next
    * natural size), which may replace the array by a shorter one. This allows releasing some unused memory. */
  def trimToSize(): Unit = resize(length)

  /** Trims the `array` buffer size down to either a power of 2
    * or Int.MaxValue while keeping first `requiredLength` elements. */
  private[this] def resize(requiredLength: Int): Unit = {
    var newSize: Long = array.length
    if (newSize == Int.MaxValue) {
      newSize += 1 // ensure that newSize is a power of 2
    }
    val minLength = ArrayBuffer.DefaultInitialSize max requiredLength
    while (newSize / 2 >= minLength) newSize /= 2
    if (newSize != array.length && newSize < Int.MaxValue) {
      val newArray: Array[AnyRef] = new Array(newSize.toInt)
      Array.copy(array, 0, newArray, 0, requiredLength)
      array = newArray
    }
  }

  @inline private def checkWithinBounds(lo: Int, hi: Int) = {
    if (lo < 0) throw new IndexOutOfBoundsException(s"$lo is out of bounds (min 0, max ${size0-1})")
    if (hi > size0) throw new IndexOutOfBoundsException(s"$hi is out of bounds (min 0, max ${size0 - 1})")
  }

  def apply(n: Int) = {
    checkWithinBounds(n, n + 1)
    array(n).asInstanceOf[A]
  }

  def update(@deprecatedName("n", "2.13.0") index: Int, elem: A): Unit = {
    checkWithinBounds(index, index + 1)
    array(index) = elem.asInstanceOf[AnyRef]
  }

  def length = size0

  override def view: ArrayBufferView[A] = new ArrayBufferView(array, size0)

  override def iterableFactory: SeqFactory[ArrayBuffer] = ArrayBuffer

  /** Note: This does not actually resize the internal representation.
    * See clearAndShrink if you want to also resize internally
    */
  def clear(): Unit = reduceToSize(0)

  /**
    * Clears this buffer and shrinks to @param size (rounding up to the next
    * natural size)
    * @param size
    */
  def clearAndShrink(size: Int = ArrayBuffer.DefaultInitialSize): this.type = {
    clear()
    resize(size)
    this
  }

  def addOne(elem: A): this.type = {
    val i = size0
    ensureSize(size0 + 1)
    size0 += 1
    this(i) = elem
    this
  }

  // Overridden to use array copying for efficiency where possible.
  override def addAll(elems: IterableOnce[A]): this.type = {
    elems match {
      case elems: ArrayBuffer[_] =>
        ensureSize(length + elems.length)
        Array.copy(elems.array, 0, array, length, elems.length)
        size0 = length + elems.length
      case _ => super.addAll(elems)
    }
    this
  }

  def insert(@deprecatedName("n", "2.13.0") index: Int, elem: A): Unit = {
    checkWithinBounds(index, index)
    ensureSize(size0 + 1)
    Array.copy(array, index, array, index + 1, size0 - index)
    size0 += 1
    this(index) = elem
  }

  def prepend(elem: A): this.type = {
    insert(0, elem)
    this
  }

  def insertAll(@deprecatedName("n", "2.13.0") index: Int, elems: IterableOnce[A]): Unit = {
    checkWithinBounds(index, index)
    elems match {
      case elems: collection.Iterable[A] =>
        val elemsLength = elems.size
        ensureSize(length + elemsLength)
        Array.copy(array, index, array, index + elemsLength, size0 - index)
        size0 = size0 + elemsLength
        elems match {
          case elems: ArrayBuffer[_] =>
            Array.copy(elems.array, 0, array, index, elemsLength)
          case _ =>
            var i = 0
            val it = elems.iterator
            while (i < elemsLength) {
              this(index + i) = it.next()
              i += 1
            }
        }
      case _ =>
        insertAll(index, ArrayBuffer.from(elems))
    }
  }

  /** Note: This does not actually resize the internal representation.
    * See trimToSize if you want to also resize internally
    */
  def remove(@deprecatedName("n", "2.13.0") index: Int): A = {
    checkWithinBounds(index, index + 1)
    val res = this(index)
    Array.copy(array, index + 1, array, index, size0 - (index + 1))
    reduceToSize(size0 - 1)
    res
  }

  /** Note: This does not actually resize the internal representation.
    * See trimToSize if you want to also resize internally
    */
  def remove(@deprecatedName("n", "2.13.0") index: Int, count: Int): Unit =
    if (count > 0) {
      checkWithinBounds(index, index + count)
      Array.copy(array, index + count, array, index, size0 - (index + count))
      reduceToSize(size0 - count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count)
    }

  @deprecated("Use 'this' instance instead", "2.13.0")
  @deprecatedOverriding("ArrayBuffer[A] no longer extends Builder[A, ArrayBuffer[A]]", "2.13.0")
  @inline def result(): this.type = this

  @deprecated("Use 'new GrowableBuilder(this).mapResult(f)' instead", "2.13.0")
  @deprecatedOverriding("ArrayBuffer[A] no longer extends Builder[A, ArrayBuffer[A]]", "2.13.0")
  @inline def mapResult[NewTo](f: (ArrayBuffer[A]) => NewTo): Builder[A, NewTo] = new GrowableBuilder[A, ArrayBuffer[A]](this).mapResult(f)

  @deprecatedOverriding("Compatibility override", since="2.13.0")
  override protected[this] def stringPrefix = "ArrayBuffer"

  override def copyToArray[B >: A](xs: Array[B], start: Int): Int = copyToArray[B](xs, start, length)

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, xs.length, start, len)
    if(copied > 0) {
      Array.copy(array, 0, xs, start, copied)
    }
    copied
  }

  /** Sorts this $coll in place according to an Ordering.
    *
    * @see [[scala.collection.mutable.IndexedSeqOps.sortInPlace]]
    * @param  ord the ordering to be used to compare elements.
    * @return modified input $coll sorted according to the ordering `ord`.
    */
  override def sortInPlace[B >: A]()(implicit ord: Ordering[B]): this.type = {
    if (length > 1) scala.util.Sorting.stableSort(array.asInstanceOf[Array[B]], 0, length)
    this
  }
}

/**
  * Factory object for the `ArrayBuffer` class.
  *
  * $factoryInfo
  *
  * @define coll array buffer
  * @define Coll `mutable.ArrayBuffer`
  */
@SerialVersionUID(3L)
object ArrayBuffer extends StrictOptimizedSeqFactory[ArrayBuffer] {
  final val DefaultInitialSize = 16

  // Avoid reallocation of buffer if length is known.
  def from[B](coll: collection.IterableOnce[B]): ArrayBuffer[B] = {
    val k = coll.knownSize
    if (k >= 0) {
      val array = new Array[AnyRef](k max DefaultInitialSize)
      val it = coll.iterator
      for (i <- 0 until k) array(i) = it.next().asInstanceOf[AnyRef]
      new ArrayBuffer[B](array, k)
    }
    else new ArrayBuffer[B] ++= coll
  }

  def newBuilder[A]: Builder[A, ArrayBuffer[A]] =
    new GrowableBuilder[A, ArrayBuffer[A]](empty) {
      override def sizeHint(size: Int): Unit = elems.ensureSize(size)
    }

  def empty[A]: ArrayBuffer[A] = new ArrayBuffer[A]()

  private def ensureSize(array: Array[AnyRef], end: Int, n: Int): Array[AnyRef] = {
    // Use a Long to prevent overflows
    val arrayLength: Long = array.length
    def growArray = {
      var newSize: Long = math.max(arrayLength * 2, DefaultInitialSize)
      while (n > newSize)
        newSize = newSize * 2
      // Clamp newSize to Int.MaxValue
      if (newSize > Int.MaxValue) {
        if (end == Int.MaxValue) throw new Exception(s"Collections can not have more than ${Int.MaxValue} elements")
        newSize = Int.MaxValue
      }

      val newArray: Array[AnyRef] = new Array(newSize.toInt)
      Array.copy(array, 0, newArray, 0, end)
      newArray
    }
    if (n <= arrayLength) array else growArray
  }
}

final class ArrayBufferView[A](val array: Array[AnyRef], val length: Int) extends AbstractIndexedSeqView[A] {
  @throws[ArrayIndexOutOfBoundsException]
  def apply(n: Int) = if (n < length) array(n).asInstanceOf[A] else throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max ${length - 1})")
  override protected[this] def className = "ArrayBufferView"
}
