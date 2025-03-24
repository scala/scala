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

package scala
package collection
package mutable

import scala.annotation.nowarn
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.reflect.ClassTag

/** An implementation of a double-ended queue that internally uses a resizable circular buffer.
  *
  *  Append, prepend, removeHead, removeLast and random-access (indexed-lookup and indexed-replacement)
  *  take amortized constant time. In general, removals and insertions at i-th index are O(min(i, n-i))
  *  and thus insertions and removals from end/beginning are fast.
  *
  *  @note Subclasses ''must'' override the `ofArray` protected method to return a more specific type.
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
class ArrayDeque[A] protected (
    protected var array: Array[AnyRef],
    private[ArrayDeque] var start: Int,
    private[ArrayDeque] var end: Int
) extends AbstractBuffer[A]
    with IndexedBuffer[A]
    with IndexedSeqOps[A, ArrayDeque, ArrayDeque[A]]
    with StrictOptimizedSeqOps[A, ArrayDeque, ArrayDeque[A]]
    with IterableFactoryDefaults[A, ArrayDeque]
    with ArrayDequeOps[A, ArrayDeque, ArrayDeque[A]]
    with Cloneable[ArrayDeque[A]]
    with DefaultSerializable {

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

  override def knownSize: Int = super[IndexedSeqOps].knownSize

  // No-Op override to allow for more efficient stepper in a minor release.
  override def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S with EfficientSplit = super.stepper(shape)

  def apply(idx: Int): A = {
    requireBounds(idx)
    _get(idx)
  }

  def update(idx: Int, elem: A): Unit = {
    requireBounds(idx)
    _set(idx, elem)
  }

  def addOne(elem: A): this.type = {
    ensureSize(length + 1)
    appendAssumingCapacity(elem)
  }

  def prepend(elem: A): this.type = {
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
    val it = elems.iterator
    if (it.nonEmpty) {
      val n = length
      // The following code resizes the current collection at most once and traverses elems at most twice
      elems.knownSize match {
        // Size is too expensive to compute AND we can traverse it only once - can't do much but retry with an IndexedSeq
        case srcLength if srcLength < 0 => prependAll(it.to(IndexedSeq: Factory[A, IndexedSeq[A]] /* type ascription needed by Dotty */))

        // We know for sure we need to resize to hold everything, might as well resize and memcopy upfront
        case srcLength if mustGrow(srcLength + n) =>
          val finalLength = srcLength + n
          val array2 = ArrayDeque.alloc(finalLength)
          @annotation.unused val copied = it.copyToArray(array2.asInstanceOf[Array[A]])
          //assert(copied == srcLength)
          copySliceToArray(srcStart = 0, dest = array2, destStart = srcLength, maxItems = n)
          reset(array = array2, start = 0, end = finalLength)

        // Just fill up from (start - srcLength) to (start - 1) and move back start
        case srcLength =>
          // Optimized version of `elems.zipWithIndex.foreach((elem, i) => _set(i - srcLength, elem))`
          var i = 0
          while(i < srcLength) {
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
        elems.iterator.foreach(appendAssumingCapacity)
      case _ => elems.iterator.foreach(+=)
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
      if (mustGrow(finalLength)) {
        val array2 = ArrayDeque.alloc(finalLength)
        copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
        array2(idx) = elem.asInstanceOf[AnyRef]
        copySliceToArray(srcStart = idx, dest = array2, destStart = idx + 1, maxItems = n)
        reset(array = array2, start = 0, end = finalLength)
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
    } else if (idx == n) {
      addAll(elems)
    } else {
      // Get both an iterator and the length of the source (by copying the source to an IndexedSeq if needed)
      val (it, srcLength) = {
        val _srcLength = elems.knownSize
        if (_srcLength >= 0) (elems.iterator, _srcLength)
        else {
          val indexed = IndexedSeq.from(elems)
          (indexed.iterator, indexed.size)
        }
      }
      if (it.nonEmpty) {
        val finalLength = srcLength + n
        // Either we resize right away or move prefix left or suffix right
        if (mustGrow(finalLength)) {
          val array2 = ArrayDeque.alloc(finalLength)
          copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
          @annotation.unused val copied = it.copyToArray(array2.asInstanceOf[Array[A]], idx)
          //assert(copied == srcLength)
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
      if (shouldShrink(finalLength)) {
        val array2 = ArrayDeque.alloc(finalLength)
        copySliceToArray(srcStart = 0, dest = array2, destStart = 0, maxItems = idx)
        copySliceToArray(srcStart = suffixStart, dest = array2, destStart = idx, maxItems = n)
        reset(array = array2, start = 0, end = finalLength)
      } else if (2*idx <= finalLength) { // Cheaper to move the prefix right
        var i = suffixStart - 1
        while(i >= removals) {
          _set(i, _get(i - removals))
          i -= 1
        }
        while(i >= 0) {
          _set(i, null.asInstanceOf[A])
          i -= 1
        }
        start = start_+(removals)
      } else {  // Cheaper to move the suffix left
        var i = idx
        while(i < finalLength) {
          _set(i, _get(i + removals))
          i += 1
        }
        while(i < n) {
          _set(i, null.asInstanceOf[A])
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

  override def subtractOne(elem: A): this.type = {
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
    val elems = scala.collection.immutable.Seq.newBuilder[A]
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
    val elems = scala.collection.immutable.Seq.newBuilder[A]
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
    val elems = scala.collection.immutable.Seq.newBuilder[A]
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
    val elems = scala.collection.immutable.Seq.newBuilder[A]
    while(lastOption.exists(f)) {
      elems += removeLastAssumingNonEmpty()
    }
    elems.result()
  }

  /** Returns the first element which satisfies the given predicate after or at some start index
    * and removes this element from the collections
    *
    *  @param p   the predicate used for choosing the first element
    *  @param from the start index
    *  @return the first element of the queue for which p yields true
    */
  def removeFirst(p: A => Boolean, from: Int = 0): Option[A] = {
    val i = indexWhere(p, from)
    if (i < 0) None else Some(remove(i))
  }

  /** Returns all elements in this collection which satisfy the given predicate
    * and removes those elements from this collections.
    *
    *  @param p   the predicate used for choosing elements
    *  @return    a sequence of all elements in the queue for which
    *             p yields true.
    */
  def removeAll(p: A => Boolean): scala.collection.immutable.Seq[A] = {
    val res = scala.collection.immutable.Seq.newBuilder[A]
    var i, j = 0
    while (i < size) {
      if (p(this(i))) {
        res += this(i)
      } else {
        if (i != j) {
          this(j) = this(i)
        }
        j += 1
      }
      i += 1
    }
    if (i != j) takeInPlace(j)
    res.result()
  }

  @inline def ensureSize(hint: Int) = if (hint > length && mustGrow(hint)) resize(hint)

  def length = end_-(start)

  override def isEmpty = start == end

  override protected def klone(): ArrayDeque[A] = new ArrayDeque(array.clone(), start = start, end = end)

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
    * Clears this buffer and shrinks to @param size
    *
    * @param size
    * @return
    */
  def clearAndShrink(size: Int = ArrayDeque.DefaultInitialSize): this.type = {
    reset(array = ArrayDeque.alloc(size), start = 0, end = 0)
    this
  }

  protected def ofArray(array: Array[AnyRef], end: Int): ArrayDeque[A] =
    new ArrayDeque[A](array, start = 0, end)

  override def copyToArray[B >: A](dest: Array[B], destStart: Int, len: Int): Int = {
    val copied = IterableOnce.elemsToCopyToArray(length, dest.length, destStart, len)
    if (copied > 0) {
      copySliceToArray(srcStart = 0, dest = dest, destStart = destStart, maxItems = len)
    }
    copied
  }

  override def toArray[B >: A: ClassTag]: Array[B] =
    copySliceToArray(srcStart = 0, dest = new Array[B](length), destStart = 0, maxItems = length)

  /**
    * Trims the capacity of this ArrayDeque's instance to be the current size
    */
  def trimToSize(): Unit = resize(length)

  // Utils for common modular arithmetic:
  @inline protected def start_+(idx: Int) = (start + idx) & (array.length - 1)
  @inline private[this] def start_-(idx: Int) = (start - idx) & (array.length - 1)
  @inline private[this] def end_+(idx: Int) = (end + idx) & (array.length - 1)
  @inline private[this] def end_-(idx: Int) = (end - idx) & (array.length - 1)

  // Note: here be overflow dragons! This is used for int overflow
  // assumptions in resize(). Use caution changing.
  @inline private[this] def mustGrow(len: Int) = {
    len >= array.length
  }

  // Assumes that 0 <= len < array.length!
  @inline private[this] def shouldShrink(len: Int) = {
    // To avoid allocation churn, only shrink when array is large
    // and less than 2/5 filled.
    array.length > ArrayDeque.StableSize && array.length - len - (len >> 1) > len
  }

  // Assumes that 0 <= len < array.length!
  @inline private[this] def canShrink(len: Int) = {
    array.length > ArrayDeque.DefaultInitialSize && array.length - len > len
  }

  @inline private[this] def _get(idx: Int): A = array(start_+(idx)).asInstanceOf[A]

  @inline private[this] def _set(idx: Int, elem: A) = array(start_+(idx)) = elem.asInstanceOf[AnyRef]

  // Assumes that 0 <= len.
  private[this] def resize(len: Int) = if (mustGrow(len) || canShrink(len)) {
    val n = length
    val array2 = copySliceToArray(srcStart = 0, dest = ArrayDeque.alloc(len), destStart = 0, maxItems = n)
    reset(array = array2, start = 0, end = n)
  }

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "ArrayDeque"
}

/**
  * $factoryInfo
  * @define coll array deque
  * @define Coll `ArrayDeque`
  */
@SerialVersionUID(3L)
object ArrayDeque extends StrictOptimizedSeqFactory[ArrayDeque] {

  def from[B](coll: collection.IterableOnce[B]): ArrayDeque[B] = {
    val s = coll.knownSize
    if (s >= 0) {
      val array = alloc(s)
      val actual = IterableOnce.copyElemsToArray(coll, array.asInstanceOf[Array[Any]])
      if (actual != s) throw new IllegalStateException(s"Copied $actual of $s")
      new ArrayDeque[B](array, start = 0, end = s)
    } else new ArrayDeque[B]() ++= coll
  }

  def newBuilder[A]: Builder[A, ArrayDeque[A]] =
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
  private[ArrayDeque] final val StableSize = 128

  /**
    * Allocates an array whose size is next power of 2 > `len`
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

trait ArrayDequeOps[A, +CC[_], +C <: AnyRef] extends StrictOptimizedSeqOps[A, CC, C] {
  protected def array: Array[AnyRef]

  final override def clone(): C = klone()

  protected def klone(): C

  protected def ofArray(array: Array[AnyRef], end: Int): C

  protected def start_+(idx: Int): Int

  @inline protected final def requireBounds(idx: Int, until: Int = length): Unit =
    if (idx < 0 || idx >= until)
      throw CommonErrors.indexOutOfBounds(index = idx, max = until - 1)

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

  override def reverse: C = {
    val n = length
    val arr = ArrayDeque.alloc(n)
    var i = 0
    while(i < n) {
      arr(i) = this(n - i - 1).asInstanceOf[AnyRef]
      i += 1
    }
    ofArray(arr, n)
  }

  override def slice(from: Int, until: Int): C = {
    val n = length
    val left = Math.max(0, Math.min(n, from))
    val right = Math.max(0, Math.min(n, until))
    val len = right - left
    if (len <= 0) {
      empty
    } else if (len >= n) {
      klone()
    } else {
      val array2 = copySliceToArray(srcStart = left, dest = ArrayDeque.alloc(len), destStart = 0, maxItems = len)
      ofArray(array2, len)
    }
  }

  override def sliding(window: Int, step: Int): Iterator[C] = {
    require(window > 0 && step > 0, s"window=$window and step=$step, but both must be positive")
    length match {
      case 0 => Iterator.empty
      case n if n <= window => Iterator.single(slice(0, length))
      case n =>
        val lag = if (window > step) window - step else 0
        Iterator.range(start = 0, end = n - lag, step = step).map(i => slice(i, i + window))
    }
  }

  override def grouped(n: Int): Iterator[C] = sliding(n, n)
}
