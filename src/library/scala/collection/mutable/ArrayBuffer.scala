package scala
package collection
package mutable

import java.lang.{IndexOutOfBoundsException, IllegalArgumentException}


/** An implementation of the `Buffer` class using an array to
  *  represent the assembled sequence internally. Append, update and random
  *  access take constant time (amortized time). Prepends and removes are
  *  linear in the buffer size.
  *
  *  @author  Matthias Zenger
  *  @author  Martin Odersky
  *  @version 2.8
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
@SerialVersionUID(3L)
class ArrayBuffer[A] private (initElems: Array[AnyRef], initSize: Int)
  extends AbstractBuffer[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with IndexedOptimizedBuffer[A]
    with StrictOptimizedSeqOps[A, ArrayBuffer, ArrayBuffer[A]]
    with Serializable {

  def this() = this(new Array[AnyRef](16), 0)

  def this(initSize: Int) = this(new Array[AnyRef](initSize), 0)

  protected var array: Array[AnyRef] = initElems
  protected var size0 = initSize

  /** Ensure that the internal array has at least `n` cells. */
  protected def ensureSize(n: Int): Unit =
    array = RefArrayUtils.ensureSize(array, size0, n)

  def sizeHint(size: Int): Unit =
    if(size > length && size >= 1) ensureSize(size)

  /** Reduce length to `n`, nulling out all dropped elements */
  private def reduceToSize(n: Int): Unit = {
    RefArrayUtils.nullElems(array, n, size0)
    size0 = n
  }

  @inline private def checkWithinBounds(lo: Int, hi: Int) = {
    if (lo < 0) throw new IndexOutOfBoundsException(lo.toString)
    if (hi > size0) throw new IndexOutOfBoundsException(hi.toString)
  }

  def apply(n: Int) = {
    checkWithinBounds(n, n + 1)
    array(n).asInstanceOf[A]
  }

  def update(n: Int, elem: A): Unit = {
    checkWithinBounds(n, n + 1)
    array(n) = elem.asInstanceOf[AnyRef]
  }

  def length = size0

  override def view: ArrayBufferView[A] = new ArrayBufferView(array, size0)

  override def iterableFactory: SeqFactory[ArrayBuffer] = ArrayBuffer

  def clear(): Unit = reduceToSize(0)

  def addOne(elem: A): this.type = {
    val i = size0
    ensureSize(size0 + 1)
    size0 += 1
    this(i) = elem
    this
  }

  def subtractOne(elem: A): this.type = {
    val i = indexOf(elem)
    if (i != -1) remove(i)
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

  def insert(idx: Int, elem: A): Unit = {
    checkWithinBounds(idx, idx)
    ensureSize(size0 + 1)
    Array.copy(array, idx, array, idx + 1, size0 - idx)
    size0 += 1
    this(idx) = elem
  }

  def prepend(elem: A): this.type = {
    insert(0, elem)
    this
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    checkWithinBounds(idx, idx)
    elems match {
      case elems: collection.Iterable[A] =>
        val elemsLength = elems.size
        ensureSize(length + elemsLength)
        Array.copy(array, idx, array, idx + elemsLength, size0 - idx)
        size0 = size0 + elemsLength
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
        insertAll(idx, ArrayBuffer.from(elems))
    }
  }

  def remove(idx: Int): A = {
    checkWithinBounds(idx, idx + 1)
    val res = this(idx)
    Array.copy(array, idx + 1, array, idx, size0 - (idx + 1))
    reduceToSize(size0 - 1)
    res
  }

  def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      checkWithinBounds(idx, idx + count)
      Array.copy(array, idx + count, array, idx, size0 - (idx + count))
      reduceToSize(size0 - count)
    } else if (count < 0) {
      throw new IllegalArgumentException("removing negative number of elements: " + count)
    }

  override def className = "ArrayBuffer"

  override def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = copyToArray[B](xs, start, length)

  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): xs.type = {
    val l = scala.math.min(scala.math.min(len, length), xs.length-start)
    if(l > 0) Array.copy(array, 0, xs, start, l)
    xs
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
object ArrayBuffer extends StrictOptimizedSeqFactory[ArrayBuffer] {

  // Avoid reallocation of buffer if length is known.
  def from[B](coll: collection.IterableOnce[B]): ArrayBuffer[B] =
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

class ArrayBufferView[A](val array: Array[AnyRef], val length: Int) extends AbstractIndexedView[A] {
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
