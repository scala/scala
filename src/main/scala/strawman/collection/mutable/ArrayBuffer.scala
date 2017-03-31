package strawman.collection.mutable

import java.lang.IndexOutOfBoundsException

import scala.{AnyRef, Array, Boolean, Exception, Int, Long, StringContext, Unit, math, Any}
import strawman.collection
import strawman.collection.{IndexedView, IterableFactory, IterableOnce, SeqLike, MonoBuildable, PolyBuildable}

import scala.Predef.intWrapper

/** Concrete collection type: ArrayBuffer */
class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int)
  extends IndexedOptimizedGrowableSeq[A]
    with SeqLike[A, ArrayBuffer]
    with MonoBuildable[A, ArrayBuffer[A]]
    with PolyBuildable[A, ArrayBuffer]
    with Builder[A, ArrayBuffer[A]] {

  def this() = this(new Array[AnyRef](16), 0)

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

  def apply(n: Int) = array(n).asInstanceOf[A]

  def update(n: Int, elem: A): Unit = array(n) = elem.asInstanceOf[AnyRef]

  def length = end
  override def knownSize = length

  override def view: ArrayBufferView[A] = new ArrayBufferView(array, end)

  def iterator() = view.iterator()

  def fromIterable[B](it: collection.Iterable[B]): ArrayBuffer[B] =
    ArrayBuffer.fromIterable(it)

  protected[this] def newBuilderWithSameElemType = new ArrayBuffer[A]
  def newBuilder[E] = new ArrayBuffer[E]

  def clear() =
    end = 0

  def +=(elem: A): this.type = {
    ensureSize(end + 1)
    this(end) = elem
    end += 1
    this
  }

  /** Overridden to use array copying for efficiency where possible. */
  override def ++=(elems: IterableOnce[A]): this.type = {
    elems match {
      case elems: ArrayBuffer[_] =>
        ensureSize(length + elems.length)
        Array.copy(elems.array, 0, array, length, elems.length)
        end = length + elems.length
      case _ => super.++=(elems)
    }
    this
  }

  def result = this

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
        val buf = new ArrayBuffer() ++= elems
        insertAll(idx, buf)
    }
  }

  def remove(idx: Int): A = {
    checkWithinBounds(idx, idx + 1)
    val res = this(idx)
    Array.copy(array, idx + 1, array, idx, end - (idx + 1))
    reduceToSize(end - 1)
    res
  }

  def remove(from: Int, n: Int): Unit =
    if (n > 0) {
      checkWithinBounds(from, from + n)
      Array.copy(array, from + n, array, from, end - (from + n))
      reduceToSize(end - n)
    }

  override def className = "ArrayBuffer"
}

object ArrayBuffer extends IterableFactory[ArrayBuffer] {

  /** Avoid reallocation of buffer if length is known. */
  def fromIterable[B](coll: collection.Iterable[B]): ArrayBuffer[B] =
    if (coll.knownSize >= 0) {
      val array = new Array[AnyRef](coll.knownSize)
      val it = coll.iterator()
      for (i <- 0 until array.length) array(i) = it.next().asInstanceOf[AnyRef]
      new ArrayBuffer[B](array, array.length)
    }
    else new ArrayBuffer[B] ++= coll

  def newBuilder[A]: Builder[A, ArrayBuffer[A]] = new ArrayBuffer[A]()

  def empty[A <: Any]: ArrayBuffer[A] = new ArrayBuffer[A]()

}

class ArrayBufferView[A](val array: Array[AnyRef], val length: Int) extends IndexedView[A] {
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
