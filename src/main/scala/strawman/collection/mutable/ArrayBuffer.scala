package strawman
package collection
package mutable

import java.lang.{IndexOutOfBoundsException, IllegalArgumentException}

import scala.{AnyRef, Array, ArrayIndexOutOfBoundsException, Boolean, Exception, Int, Long, StringContext, Unit, math, Any, throws}
import scala.Predef.intWrapper

/** Concrete collection type: ArrayBuffer */
class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int)
  extends IndexedSeq[A]
    with IndexedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with IndexedOptimizedSeq[A]
    with StrictOptimizedSeqOps[A, ArrayBuffer, ArrayBuffer[A]] {

  def this() = this(new Array[AnyRef](16), 0)

  def this(initLength: Int) = this(new Array[AnyRef](initLength), 0)

  private var array: Array[AnyRef] = initElems
  private var end = initLength

  /** Ensure that the internal array has at least `n` cells. */
  private def ensureSize(n: Int): Unit =
    array = RefArrayUtils.ensureSize(array, end, n)

  /** Reduce length to `n`, nulling out all dropped elements */
  private def reduceToSize(n: Int): Unit = {
    RefArrayUtils.nullElems(array, n, end)
    end = n
  }

  private def checkWithinBounds(lo: Int, hi: Int) = {
    if (lo < 0) throw new IndexOutOfBoundsException(lo.toString)
    if (hi > end) throw new IndexOutOfBoundsException(hi.toString)
  }

  @throws[ArrayIndexOutOfBoundsException]
  def apply(n: Int) = array(n).asInstanceOf[A]

  def update(n: Int, elem: A): Unit = array(n) = elem.asInstanceOf[AnyRef]

  def length = end
  override def knownSize = length

  override def view: ArrayBufferView[A] = new ArrayBufferView(array, end)

  def iterableFactory: SeqFactory[ArrayBuffer] = ArrayBuffer

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): ArrayBuffer[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, ArrayBuffer[A]] = ArrayBuffer.newBuilder()

  def clear(): Unit =
    end = 0

  def add(elem: A): this.type = {
    ensureSize(end + 1)
    this(end) = elem
    end += 1
    this
  }

  def subtract(elem: A): this.type = {
    val i = indexOf(elem)
    if (i != -1) remove(i)
    this
  }

  /** Overridden to use array copying for efficiency where possible. */
  override def addAll(elems: IterableOnce[A]): this.type = {
    elems match {
      case elems: ArrayBuffer[_] =>
        ensureSize(length + elems.length)
        Array.copy(elems.array, 0, array, length, elems.length)
        end = length + elems.length
      case _ => super.addAll(elems)
    }
    this
  }

  def insert(idx: Int, elem: A): Unit = {
    checkWithinBounds(idx, idx)
    ensureSize(end + 1)
    Array.copy(array, idx, array, idx + 1, end - idx)
    this(idx) = elem
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    checkWithinBounds(idx, idx)
    elems match {
      case elems: collection.Iterable[A] =>
        val elemsLength = elems.size
        ensureSize(length + elemsLength)
        Array.copy(array, idx, array, idx + elemsLength, end - idx)
        end = end + elemsLength
        elems match {
          case elems: ArrayBuffer[_] =>
            Array.copy(elems.array, 0, array, idx, elemsLength)
          case _ =>
            var i = 0
            val it = elems.iterator()
            while (i < elemsLength) {
              this(idx + i) = it.next()
              i += 1
            }
        }
      case _ =>
        insertAll(idx, ArrayBuffer.fromIterable(View.fromIterator(elems.iterator())))
    }
  }

  def remove(idx: Int): A = {
    checkWithinBounds(idx, idx + 1)
    val res = this(idx)
    Array.copy(array, idx + 1, array, idx, end - (idx + 1))
    reduceToSize(end - 1)
    res
  }

  def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      checkWithinBounds(idx, idx + count)
      Array.copy(array, idx + count, array, idx, end - (idx + count))
      reduceToSize(end - count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count.toString)
    }

  override def className = "ArrayBuffer"

}

object ArrayBuffer extends SeqFactory[ArrayBuffer] {

  /** Avoid reallocation of buffer if length is known. */
  def fromIterable[B](coll: collection.Iterable[B]): ArrayBuffer[B] =
    if (coll.knownSize >= 0) {
      val array = new Array[AnyRef](coll.knownSize)
      val it = coll.iterator()
      for (i <- 0 until array.length) array(i) = it.next().asInstanceOf[AnyRef]
      new ArrayBuffer[B](array, array.length)
    }
    else new ArrayBuffer[B] ++= coll

  def newBuilder[A](): Builder[A, ArrayBuffer[A]] =
    new GrowableBuilder[A, ArrayBuffer[A]](empty) {
      override def sizeHint(size: Int): Unit = elems.ensureSize(size)
    }

  def empty[A]: ArrayBuffer[A] = new ArrayBuffer[A]()
}

class ArrayBufferView[A](val array: Array[AnyRef], val length: Int) extends IndexedView[A] {
  @throws[ArrayIndexOutOfBoundsException]
  def apply(n: Int) = array(n).asInstanceOf[A]
  override def className = "ArrayBufferView"
}

/** An object used internally by collections backed by an extensible Array[AnyRef] */
object RefArrayUtils {

  def ensureSize(array: Array[AnyRef], end: Int, n: Int): Array[AnyRef] = {
    // Use a Long to prevent overflows
    val arrayLength: Long = array.length
    def growArray = {
      var newSize: Long = math.max(arrayLength * 2, 8)
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

  /** Remove elements of this array at indices after `sz`.
   */
  def nullElems(array: Array[AnyRef], start: Int, end: Int): Unit = {
    // Maybe use `fill` instead?
    var i = start
    while (i < end) {
      array(i) = null
      i += 1
    }
  }
}
