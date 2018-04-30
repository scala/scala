package scala
package collection
package mutable

import scala.reflect.ClassTag

import scala.collection.{Iterator, StrictOptimizedSeqOps}

import java.lang.Math
import java.util.NoSuchElementException

/** An implementation of a double-ended queue that internally uses a resizable circular buffer
  *  Append, prepend, removeFirst, removeLast and random-access (indexed-lookup and indexed-replacement)
  *  take amortized constant time. In general, removals and insertions at i-th index are O(min(i, n-i))
  *  and thus insertions and removals from end/beginning are fast.
  *
  *  @note Subclasses ''must'' override the `ofArray` protected method to return a more specific type.
  *
  *  @author  Pathikrit Bhowmick
  *  @version 2.13
  *  @since   2.13
  *
  *  @tparam A  the type of this ArrayDeque's elements.
  *
  *  @define Coll `mutable.ArrayDeque`
  *  @define coll array deque
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@SerialVersionUID(3L)
class ArrayDeque[A] protected (
    private[ArrayDeque] var array: Array[AnyRef],
    private[ArrayDeque] var start: Int,
    private[ArrayDeque] var end: Int
) extends AbstractBuffer[A]
    with IndexedSeq[A]
    with IndexedSeqOps[A, ArrayDeque, ArrayDeque[A]]
    with IndexedOptimizedBuffer[A]
    with StrictOptimizedSeqOps[A, ArrayDeque, ArrayDeque[A]]
    with Serializable {

  reset(array, start, end)

  private[this] def reset(array: Array[AnyRef], start: Int, end: Int) = {
    assert((array.length & (array.length - 1)) == 0, s"Array.length must be power of 2")
    requireBounds(idx = start, until = array.length)
    requireBounds(idx = end, until = array.length)
    this.array = array
    this.start = start
    this.end = end
  }

  def this(initialSize: Int = ArrayDeque.DefaultInitialSize) = this(ArrayDeque.alloc(initialSize), start = 0, end = 0)

  def apply(idx: Int) = {
    requireBounds(idx)
    _get(idx)
  }

  def update(idx: Int, elem: A) = {
    requireBounds(idx)
    _set(idx, elem)
  }

  def addOne(elem: A) = {
    ensureSize(length + 1)
    appendAssumingCapacity(elem)
  }

  def prepend(elem: A) = {
    ensureSize(length + 1)
    prependAssumingCapacity(elem)
  }

  @inline private[ArrayDeque] def appendAssumingCapacity(elem: A): this.type = {
    array(end) = elem.asInstanceOf[AnyRef]
    end = end_+(1)
    this
  }

  @inline private[ArrayDeque] def prependAssumingCapacity(elem: A): this.type = {
    start = start_-(1)
    array(start) = elem.asInstanceOf[AnyRef]
    this
  }

  override def prependAll(elems: IterableOnce[A]): this.type = {
    val it = elems.iterator()
    if (it.nonEmpty) {
      val n = length
      // The following code resizes the current collection atmost once and traverses elems atmost twice
      elems.knownSize match {
        // Size is too expensive to compute AND we can traverse it only once - can't do much but retry with an IndexedSeq
        case srcLength if srcLength < 0 => prependAll(it.to(IndexedSeq: Factory[A, IndexedSeq[A]] /* type ascription needed by Dotty */))

        // We know for sure we need to resize to hold everything, might as well resize and memcopy upfront
        case srcLength if srcLength > 0 && isResizeNecessary(srcLength + n) =>
          val finalLength = srcLength + n
          val array2 = ArrayDeque.alloc(finalLength)
          it.copyToArray(array2.asInstanceOf[Array[A]])
          copySliceToArray(srcStart = 0, dest = array2, destStart = srcLength, maxItems = n)
          reset(array = array2, start = 0, end = finalLength)

        // Just fill up from (start - srcLength) to (start - 1) and move back start
        case srcLength =>
          ensureSize(srcLength + n)
          // Optimized version of `elems.zipWithIndex.foreach((elem, i) => _set(i - srcLength, elem))`
          var i = 0
          while(it.hasNext) {
            _set(i - srcLength, it.next())
            i += 1
          }
          start = start_-(srcLength)
      }
    }
    this
  }

  override def addAll(elems: IterableOnce[A]): this.type = {
    elems.knownSize match {
      case srcLength if srcLength > 0 =>
        ensureSize(srcLength + length)
        elems.iterator().foreach(appendAssumingCapacity)
      case _ => elems.iterator().foreach(+=)
    }
    this
  }

  def insert(idx: Int, elem: A): Unit = {
    requireBounds(idx, length+1)
    val n = length
    if (idx == 0) {
      prepend(elem)
    } else if (idx == n) {
      addOne(elem)
    } else {
      val finalLength = n + 1
      if (isResizeNecessary(finalLength)) {
        val array2 = ArrayDeque.alloc(finalLength)
        copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
        array2(idx) = elem.asInstanceOf[AnyRef]
        copySliceToArray(srcStart = idx, dest = array2, destStart = idx + 1, maxItems = n)
      } else if (n <= idx * 2) {
        var i = n - 1
        while(i >= idx) {
          _set(i + 1, _get(i))
          i -= 1
        }
        end = end_+(1)
        i += 1
        _set(i, elem)
      } else {
        var i = 0
        while(i < idx) {
          _set(i - 1, _get(i))
          i += 1
        }
        start = start_-(1)
        _set(i, elem)
      }
    }
  }

  def insertAll(idx: Int, elems: IterableOnce[A]): Unit = {
    requireBounds(idx, length+1)
    val n = length
    if (idx == 0) {
      prependAll(elems)
    } else if (idx == n - 1) {
      addAll(elems)
    } else {
      // Get both an iterator and the length of the source (by copying the source to an IndexedSeq if needed)
      val (it, srcLength) = {
        val _srcLength = elems.knownSize
        if (_srcLength >= 0) (elems.iterator(), _srcLength)
        else {
          val indexed = IndexedSeq.from(elems)
          (indexed.iterator(), indexed.size)
        }
      }
      if (it.nonEmpty) {
        val finalLength = srcLength + n
        // Either we resize right away or move prefix left or suffix right
        if (isResizeNecessary(finalLength)) {
          val array2 = ArrayDeque.alloc(finalLength)
          copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
          it.copyToArray(array2.asInstanceOf[Array[A]], idx)
          copySliceToArray(srcStart = idx, dest = array2, destStart = idx + srcLength, maxItems = n)
          reset(array = array2, start = 0, end = finalLength)
        } else if (2*idx >= n) { // Cheaper to shift the suffix right
          var i = n - 1
          while(i >= idx) {
            _set(i + srcLength, _get(i))
            i -= 1
          }
          end = end_+(srcLength)
          while(it.hasNext) {
            i += 1
            _set(i, it.next())
          }
        } else {  // Cheaper to shift prefix left
          var i = 0
          while(i < idx) {
            _set(i - srcLength, _get(i))
            i += 1
          }
          start = start_-(srcLength)
          while(it.hasNext) {
            _set(i, it.next())
            i += 1
          }
        }
      }
    }
  }

  def remove(idx: Int, count: Int): Unit = {
    if (count > 0) {
      requireBounds(idx)
      val n = length
      val removals = Math.min(n - idx, count)
      val finalLength = n - removals
      val suffixStart = idx + removals
      // If we know we can resize after removing, do it right away using arrayCopy
      // Else, choose the shorter: either move the prefix (0 until idx) right OR the suffix (idx+removals until n) left
      if (isResizeNecessary(finalLength)) {
        val array2 = ArrayDeque.alloc(finalLength)
        copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
        copySliceToArray(srcStart = suffixStart, dest = array2, destStart = idx, maxItems = n)
        reset(array = array2, start = 0, end = finalLength)
      } else if (2*idx <= finalLength) { // Cheaper to move the prefix right
        var i = suffixStart
        while(i >= 0) {
          i -= 1
          _set(i, if (i >= removals) _get(i - removals) else null.asInstanceOf[A])
        }
        start = start_+(removals)
      } else {  // Cheaper to move the suffix left
        var i = idx
        while(i < n) {
          _set(i, if (i < finalLength) _get(i + removals) else null.asInstanceOf[A])
          i += 1
        }
        end = end_-(removals)
      }
    } else {
      require(count == 0, s"removing negative number of elements: $count")
    }
  }

  def remove(idx: Int): A = {
    val elem = this(idx)
    remove(idx, 1)
    elem
  }

  def subtractOne(elem: A): this.type = {
    val idx = indexOf(elem)
    if (idx >= 0) remove(idx, 1) //TODO: SeqOps should be fluent API
    this
  }

  /**
    *
    * @param resizeInternalRepr If this is set, resize the internal representation to reclaim space once in a while
    * @return
    */
  def removeHeadOption(resizeInternalRepr: Boolean = false): Option[A] =
    if (isEmpty) None else Some(removeHeadAssumingNonEmpty(resizeInternalRepr))

  /**
    * Unsafely remove the first element (throws exception when empty)
    * See also removeHeadOption()
    *
    * @param resizeInternalRepr If this is set, resize the internal representation to reclaim space once in a while
    * @throws NoSuchElementException when empty
    * @return
    */
  def removeHead(resizeInternalRepr: Boolean = false): A =
    if (isEmpty) throw new NoSuchElementException(s"empty collection") else removeHeadAssumingNonEmpty(resizeInternalRepr)

  @inline private[this] def removeHeadAssumingNonEmpty(resizeInternalRepr: Boolean = false): A = {
    val elem = array(start)
    array(start) = null
    start = start_+(1)
    if (resizeInternalRepr) resize(length)
    elem.asInstanceOf[A]
  }

  /**
    *
    * @param resizeInternalRepr If this is set, resize the internal representation to reclaim space once in a while
    * @return
    */
  def removeLastOption(resizeInternalRepr: Boolean = false): Option[A] =
    if (isEmpty) None else Some(removeLastAssumingNonEmpty(resizeInternalRepr))

  /**
    * Unsafely remove the last element (throws exception when empty)
    * See also removeLastOption()
    *
    * @param resizeInternalRepr If this is set, resize the internal representation to reclaim space once in a while
    * @throws NoSuchElementException when empty
    * @return
    */
  def removeLast(resizeInternalRepr: Boolean = false): A =
    if (isEmpty) throw new NoSuchElementException(s"empty collection") else removeLastAssumingNonEmpty(resizeInternalRepr)

  @`inline` private[this] def removeLastAssumingNonEmpty(resizeInternalRepr: Boolean = false): A = {
    end = end_-(1)
    val elem = array(end)
    array(end) = null
    if (resizeInternalRepr) resize(length)
    elem.asInstanceOf[A]
  }

  /**
    * Remove all elements from this collection and return the elements while emptying this data structure
    * @return
    */
  def removeAll(): scala.collection.immutable.Seq[A] = {
    val elems = scala.collection.immutable.Seq.newBuilder[A]()
    elems.sizeHint(length)
    while(nonEmpty) {
      elems += removeHeadAssumingNonEmpty()
    }
    elems.result()
  }

  /**
    * Remove all elements from this collection and return the elements in reverse while emptying this data structure
    * @return
    */
  def removeAllReverse(): scala.collection.immutable.Seq[A] = {
    val elems = scala.collection.immutable.Seq.newBuilder[A]()
    elems.sizeHint(length)
    while(nonEmpty) {
      elems += removeLastAssumingNonEmpty()
    }
    elems.result()
  }

  /**
    * Returns and removes all elements from the left of this queue which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return
    */
  def removeHeadWhile(f: A => Boolean): scala.collection.immutable.Seq[A] = {
    val elems = scala.collection.immutable.Seq.newBuilder[A]()
    while(headOption.exists(f)) {
      elems += removeHeadAssumingNonEmpty()
    }
    elems.result()
  }

  /**
    * Returns and removes all elements from the right of this queue which satisfy the given predicate
    *
    *  @param f   the predicate used for choosing elements
    *  @return
    */
  def removeLastWhile(f: A => Boolean): scala.collection.immutable.Seq[A] = {
    val elems = scala.collection.immutable.Seq.newBuilder[A]()
    while(lastOption.exists(f)) {
      elems += removeLastAssumingNonEmpty()
    }
    elems.result()
  }

  override def reverse: IterableCC[A] = {
    val n = length
    val arr = ArrayDeque.alloc(n)
    var i = 0
    while(i < n) {
      arr(i) = this(n - i - 1).asInstanceOf[AnyRef]
      i += 1
    }
    ofArray(arr, n)
  }

  @inline def ensureSize(hint: Int) = if (hint > length && isResizeNecessary(hint)) resize(hint + 1)

  def length = end_-(start)

  override def isEmpty = start == end

  override def clone(): ArrayDeque[A] = new ArrayDeque(array.clone(), start = start, end = end)

  override def iterableFactory: SeqFactory[ArrayDeque] = ArrayDeque

  /**
    * Note: This does not actually resize the internal representation.
    * See clearAndShrink if you want to also resize internally
    */
  def clear(): Unit = {
    while(nonEmpty) {
      removeHeadAssumingNonEmpty()
    }
  }

  /**
    * clears this buffer and shrinks to @param size
    *
    * @param size
    * @return
    */
  def clearAndShrink(size: Int = ArrayDeque.DefaultInitialSize): this.type = {
    reset(array = ArrayDeque.alloc(size), start = 0, end = 0)
    this
  }

  override def slice(from: Int, until: Int): IterableCC[A] = {
    val n = length
    val left = Math.max(0, Math.min(n, from))
    val right = Math.max(0, Math.min(n, until))
    val len = right - left
    if (len <= 0) {
      iterableFactory.empty[A]
    } else if (len >= n) {
      clone()
    } else {
      val array2 = copySliceToArray(srcStart = left, dest = ArrayDeque.alloc(len), destStart = 0, maxItems = len)
      ofArray(array2, len)
    }
  }

  protected def ofArray(array: Array[AnyRef], end: Int): ArrayDeque[A] =
    new ArrayDeque[A](array, start = 0, end)

  override def sliding(window: Int, step: Int): Iterator[IterableCC[A]] = {
    require(window > 0 && step > 0, s"window=$window and step=$step, but both must be positive")
    val lag = if (window > step) window - step else 0
    Iterator.range(start = 0, end = length - lag, step = step).map(i => slice(i, i + window))
  }

  override def grouped(n: Int): Iterator[IterableCC[A]] = sliding(n, n)

  override def copyToArray[B >: A](dest: Array[B], destStart: Int, len: Int): dest.type =
    copySliceToArray(srcStart = 0, dest = dest, destStart = destStart, maxItems = len)

  override def toArray[B >: A: ClassTag]: Array[B] =
    copySliceToArray(srcStart = 0, dest = new Array[B](length), destStart = 0, maxItems = length)

  /**
    * This is a more general version of copyToArray - this also accepts a srcStart unlike copyToArray
    * This copies maxItems elements from this collections srcStart to dest's destStart
    * If we reach the end of either collections before we could copy maxItems, we simply stop copying
    *
    * @param dest
    * @param srcStart
    * @param destStart
    * @param maxItems
    */
  def copySliceToArray(srcStart: Int, dest: Array[_], destStart: Int, maxItems: Int): dest.type = {
    requireBounds(destStart, dest.length+1)
    val toCopy = Math.min(maxItems, Math.min(length - srcStart, dest.length - destStart))
    if (toCopy > 0) {
      requireBounds(srcStart)
      val startIdx = start_+(srcStart)
      val block1 = Math.min(toCopy, array.length - startIdx)
      Array.copy(src = array, srcPos = startIdx, dest = dest, destPos = destStart, length = block1)
      val block2 = toCopy - block1
      if (block2 > 0) Array.copy(src = array, srcPos = 0, dest = dest, destPos = destStart + block1, length = block2)
    }
    dest
  }

  /**
    * Trims the capacity of this ArrayDeque's instance to be the current size
    */
  def trimToSize(): Unit = resize(length - 1)

  // Utils for common modular arithmetic:
  @inline private[this] def start_+(idx: Int) = (start + idx) & (array.length - 1)
  @inline private[this] def start_-(idx: Int) = (start - idx) & (array.length - 1)
  @inline private[this] def end_+(idx: Int) = (end + idx) & (array.length - 1)
  @inline private[this] def end_-(idx: Int) = (end - idx) & (array.length - 1)

  @inline private[this] def isResizeNecessary(len: Int) = {
    // Either resize if we need more cells OR we need to downsize BUT not a good idea to repeatedly resize small arrays
    len >= (array.length - 1) || (2*len < array.length && array.length >= ArrayDeque.StableSize)
  }

  @inline private[this] def _get(idx: Int): A = array(start_+(idx)).asInstanceOf[A]

  @inline private[this] def _set(idx: Int, elem: A) = array(start_+(idx)) = elem.asInstanceOf[AnyRef]

  private[this] def resize(len: Int) = if (isResizeNecessary(len)) {
    val n = length
    val array2 = copySliceToArray(srcStart = 0, dest = ArrayDeque.alloc(len), destStart = 0, maxItems = n)
    reset(array = array2, start = 0, end = n)
  }

  @inline private[this] def requireBounds(idx: Int, until: Int = length) =
    if (idx < 0 || idx >= until) throw new IndexOutOfBoundsException(idx.toString)
}

/**
  * $factoryInfo
  * @define coll array deque
  * @define Coll `ArrayDeque`
  */
object ArrayDeque extends StrictOptimizedSeqFactory[ArrayDeque] {

  def from[B](coll: collection.IterableOnce[B]): ArrayDeque[B] = {
    val s = coll.knownSize
    if (s >= 0) {
      val array = alloc(s)
      val it = coll.iterator()
      var i = 0
      while (it.hasNext) {
        array(i) = it.next().asInstanceOf[AnyRef]
        i += 1
      }
      new ArrayDeque[B](array, start = 0, end = s)
    } else empty[B] ++= coll
  }

  def newBuilder[A](): Builder[A, ArrayDeque[A]] =
    new GrowableBuilder[A, ArrayDeque[A]](empty) {
      override def sizeHint(size: Int): Unit = {
        elems.ensureSize(size)
      }
    }

  def empty[A]: ArrayDeque[A] = new ArrayDeque[A]()

  final val DefaultInitialSize = 16

  /**
    * We try to not repeatedly resize arrays smaller than this
    */
  private[ArrayDeque] final val StableSize = 256

  /**
    * Allocates an array whose size is next power of 2 > $len
    * Largest possible len is 1<<30 - 1
    *
    * @param len
    * @return
    */
  private[mutable] def alloc(len: Int) = {
    require(len >= 0, s"Non-negative array size required")
    val size = (1 << 31) >>> java.lang.Integer.numberOfLeadingZeros(len) << 1
    require(size >= 0, s"ArrayDeque too big - cannot allocate ArrayDeque of length $len")
    new Array[AnyRef](Math.max(size, DefaultInitialSize))
  }
}
