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

package scala.collection
package mutable

import scala.annotation.tailrec
import scala.collection.generic.{CommonErrors, DefaultSerializable}
import scala.reflect.ClassTag
import scala.collection.immutable.Nil

/** A buffer that stores elements in an unrolled linked list.
  *
  *  Unrolled linked lists store elements in linked fixed size
  *  arrays.
  *
  *  Unrolled buffers retain locality and low memory overhead
  *  properties of array buffers, but offer much more efficient
  *  element addition, since they never reallocate and copy the
  *  internal array.
  *
  *  However, they provide `O(n/m)` complexity random access,
  *  where `n` is the number of elements, and `m` the size of
  *  internal array chunks.
  *
  *  Ideal to use when:
  *  - elements are added to the buffer and then all of the
  *    elements are traversed sequentially
  *  - two unrolled buffers need to be concatenated (see `concat`)
  *
  *  Better than singly linked lists for random access, but
  *  should still be avoided for such a purpose.
  *
  *  @define coll unrolled buffer
  *  @define Coll `UnrolledBuffer`
  *
  */
@SerialVersionUID(3L)
sealed class UnrolledBuffer[T](implicit val tag: ClassTag[T])
  extends AbstractBuffer[T]
    with Buffer[T]
    with Seq[T]
    with SeqOps[T, UnrolledBuffer, UnrolledBuffer[T]]
    with StrictOptimizedSeqOps[T, UnrolledBuffer, UnrolledBuffer[T]]
    with EvidenceIterableFactoryDefaults[T, UnrolledBuffer, ClassTag]
    with Builder[T, UnrolledBuffer[T]]
    with DefaultSerializable {

  import UnrolledBuffer.Unrolled

  @transient private var headptr = newUnrolled
  @transient private var lastptr = headptr
  @transient private var sz = 0

  private[collection] def headPtr = headptr
  private[collection] def headPtr_=(head: Unrolled[T]) = headptr = head
  private[collection] def lastPtr = lastptr
  private[collection] def lastPtr_=(last: Unrolled[T]) = lastptr = last
  private[collection] def size_=(s: Int) = sz = s

  protected def evidenceIterableFactory: UnrolledBuffer.type = UnrolledBuffer
  protected def iterableEvidence: ClassTag[T] = tag

  override def iterableFactory: SeqFactory[UnrolledBuffer] = UnrolledBuffer.untagged

  protected def newUnrolled = new Unrolled[T](this)

  // The below would allow more flexible behavior without requiring inheritance
  // that is risky because all the important internals are private.
  // private var myLengthPolicy: Int => Int = x => x
  //
  // /** Specifies how the array lengths should vary.
  //   *
  //   *  By default,  `UnrolledBuffer` uses arrays of a fixed size.  A length
  //   *  policy can be given that changes this scheme to, for instance, an
  //   *  exponential growth.
  //   *
  //   *  @param nextLength   computes the length of the next array from the length of the latest one
  //   */
  // def setLengthPolicy(nextLength: Int => Int): Unit = { myLengthPolicy = nextLength }
  private[collection] def calcNextLength(sz: Int) = sz // myLengthPolicy(sz)

  def classTagCompanion: UnrolledBuffer.type = UnrolledBuffer

  /** Concatenates the target unrolled buffer to this unrolled buffer.
    *
    *  The specified buffer `that` is cleared after this operation. This is
    *  an O(1) operation.
    *
    *  @param that    the unrolled buffer whose elements are added to this buffer
    */
  def concat(that: UnrolledBuffer[T]) = {
    // bind the two together
    if (!lastptr.bind(that.headptr)) lastptr = that.lastPtr

    // update size
    sz += that.sz

    // `that` is no longer usable, so clear it
    // here we rely on the fact that `clear` allocates
    // new nodes instead of modifying the previous ones
    that.clear()

    // return a reference to this
    this
  }

  def addOne(elem: T) = {
    lastptr = lastptr.append(elem)
    sz += 1
    this
  }

  def clear(): Unit = {
    headptr = newUnrolled
    lastptr = headptr
    sz = 0
  }

  def iterator: Iterator[T] = new AbstractIterator[T] {
    var pos: Int = -1
    var node: Unrolled[T] = headptr
    scan()

    private def scan(): Unit = {
      pos += 1
      while (pos >= node.size) {
        pos = 0
        node = node.next
        if (node eq null) return
      }
    }
    def hasNext = node ne null
    def next() = if (hasNext) {
      val r = node.array(pos)
      scan()
      r
    } else Iterator.empty.next()
  }

  // this should be faster than the iterator
  override def foreach[U](f: T => U) = headptr.foreach(f)

  def result() = this

  def length = sz

  override def knownSize: Int = sz

  def apply(idx: Int) =
    if (idx >= 0 && idx < sz) headptr(idx)
    else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  def update(idx: Int, newelem: T) =
    if (idx >= 0 && idx < sz) headptr(idx) = newelem
    else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  /** Replace the contents of this $coll with the mapped result.
   *
   *  @param f the mapping function
   *  @return this $coll
   */
  def mapInPlace(f: T => T): this.type = {
    headptr.mapInPlace(f)
    this
  }

  def remove(idx: Int) =
    if (idx >= 0 && idx < sz) {
      sz -= 1
      headptr.remove(idx, this)
    } else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx)
      remove(idx, count-1)
    }

  def prepend(elem: T) = {
    headptr = headptr prepend elem
    sz += 1
    this
  }

  def insert(idx: Int, elem: T): Unit =
    insertAll(idx, elem :: Nil)

  def insertAll(idx: Int, elems: IterableOnce[T]): Unit =
    if (idx >= 0 && idx <= sz) {
      sz += headptr.insertAll(idx, elems, this)
    } else throw CommonErrors.indexOutOfBounds(index = idx, max = sz - 1)

  override def subtractOne(elem: T): this.type = {
    if (headptr.subtractOne(elem, this)) {
      sz -= 1
    }
    this
  }

  def patchInPlace(from: Int, patch: collection.IterableOnce[T], replaced: Int): this.type = {
    remove(from, replaced)
    insertAll(from, patch)
    this
  }

  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.defaultWriteObject
    out writeInt sz
    for (elem <- this) out writeObject elem
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    in.defaultReadObject

    val num = in.readInt

    headPtr = newUnrolled
    lastPtr = headPtr
    sz = 0
    var i = 0
    while (i < num) {
      this += in.readObject.asInstanceOf[T]
      i += 1
    }
  }

  override def clone(): UnrolledBuffer[T] = new UnrolledBuffer[T] ++= this

  override protected[this] def className = "UnrolledBuffer"
}


@SerialVersionUID(3L)
object UnrolledBuffer extends StrictOptimizedClassTagSeqFactory[UnrolledBuffer] { self =>

  val untagged: SeqFactory[UnrolledBuffer] = new ClassTagSeqFactory.AnySeqDelegate(self)

  def empty[A : ClassTag]: UnrolledBuffer[A] = new UnrolledBuffer[A]

  def from[A : ClassTag](source: scala.collection.IterableOnce[A]): UnrolledBuffer[A] = newBuilder[A].addAll(source)

  def newBuilder[A : ClassTag]: UnrolledBuffer[A] = new UnrolledBuffer[A]

  final val waterline: Int = 50

  final def waterlineDenom: Int = 100

  @deprecated("Use waterlineDenom instead.", "2.13.0")
  final val waterlineDelim: Int = waterlineDenom

  private[collection] val unrolledlength = 32

  /** Unrolled buffer node.
    */
  class Unrolled[T: ClassTag] private[collection] (var size: Int, var array: Array[T], var next: Unrolled[T], val buff: UnrolledBuffer[T] = null) {
    private[collection] def this() = this(0, new Array[T](unrolledlength), null, null)
    private[collection] def this(b: UnrolledBuffer[T]) = this(0, new Array[T](unrolledlength), null, b)

    private def nextlength = if (buff eq null) unrolledlength else buff.calcNextLength(array.length)

    // adds and returns itself or the new unrolled if full
    @tailrec final def append(elem: T): Unrolled[T] = if (size < array.length) {
      array(size) = elem
      size += 1
      this
    } else {
      next = new Unrolled[T](0, new Array[T](nextlength), null, buff)
      next append elem
    }
    def foreach[U](f: T => U): Unit = {
      var unrolled = this
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          f(elem)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
    }
    def mapInPlace(f: T => T): Unit = {
      var unrolled = this
      var i = 0
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          chunkarr(i) = f(elem)
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
    }
    @tailrec final def apply(idx: Int): T =
      if (idx < size) array(idx) else next.apply(idx - size)
    @tailrec final def update(idx: Int, newelem: T): Unit =
      if (idx < size) array(idx) = newelem else next.update(idx - size, newelem)
    @tailrec final def locate(idx: Int): Unrolled[T] =
      if (idx < size) this else next.locate(idx - size)
    def prepend(elem: T) = if (size < array.length) {
      // shift the elements of the array right
      // then insert the element
      shiftright()
      array(0) = elem
      size += 1
      this
    } else {
      // allocate a new node and store element
      // then make it point to this
      val newhead = new Unrolled[T](buff)
      newhead append elem
      newhead.next = this
      newhead
    }
    // shifts right assuming enough space
    private def shiftright(): Unit = {
      var i = size - 1
      while (i >= 0) {
        array(i + 1) = array(i)
        i -= 1
      }
    }
    // returns pointer to new last if changed
    @tailrec final def remove(idx: Int, buffer: UnrolledBuffer[T]): T =
      if (idx < size) {
        // remove the element
        // then try to merge with the next bucket
        val r = array(idx)
        shiftleft(idx)
        size -= 1
        if (tryMergeWithNext()) buffer.lastPtr = this
        r
      } else next.remove(idx - size, buffer)

    @tailrec final def subtractOne(elem: T, buffer: UnrolledBuffer[T]): Boolean = {
      var i = 0
      while (i < size) {
        if(array(i) == elem) {
          remove(i, buffer)
          return true
        }
        i += 1
      }
      if(next ne null) next.subtractOne(elem, buffer) else false
    }

    // shifts left elements after `leftb` (overwrites `leftb`)
    private def shiftleft(leftb: Int): Unit = {
      var i = leftb
      while (i < (size - 1)) {
        array(i) = array(i + 1)
        i += 1
      }
      nullout(i, i + 1)
    }
    protected def tryMergeWithNext() = if (next != null && (size + next.size) < (array.length * waterline / waterlineDenom)) {
      // copy the next array, then discard the next node
      Array.copy(next.array, 0, array, size, next.size)
      size = size + next.size
      next = next.next
      if (next eq null) true else false // checks if last node was thrown out
    } else false

    @tailrec final def insertAll(idx: Int, t: scala.collection.IterableOnce[T], buffer: UnrolledBuffer[T]): Int = {
      if (idx < size) {
        // divide this node at the appropriate position and insert all into head
        // update new next
        val newnextnode = new Unrolled[T](0, new Array(array.length), null, buff)
        Array.copy(array, idx, newnextnode.array, 0, size - idx)
        newnextnode.size = size - idx
        newnextnode.next = next

        // update this
        nullout(idx, size)
        size = idx
        next = null

        // insert everything from iterable to this
        var curr = this
        var appended = 0
        for (elem <- t.iterator) {
          curr = curr append elem
          appended += 1
        }
        curr.next = newnextnode

        // try to merge the last node of this with the newnextnode and fix tail pointer if needed
        if (curr.tryMergeWithNext()) buffer.lastPtr = curr
        else if (newnextnode.next eq null) buffer.lastPtr = newnextnode
        appended
      }
      else if (idx == size || (next eq null)) {
        var curr = this
        var appended = 0
        for (elem <- t.iterator) {
          curr = curr append elem
          appended += 1
        }
        appended
      }
      else next.insertAll(idx - size, t, buffer)
    }

    private def nullout(from: Int, until: Int): Unit = {
      var idx = from
      while (idx < until) {
        array(idx) = null.asInstanceOf[T] // TODO find a way to assign a default here!!
        idx += 1
      }
    }

    // assumes this is the last node
    // `thathead` and `thatlast` are head and last node
    // of the other unrolled list, respectively
    def bind(thathead: Unrolled[T]) = {
      assert(next eq null)
      next = thathead
      tryMergeWithNext()
    }

    override def toString: String =
      array.take(size).mkString("Unrolled@%08x".format(System.identityHashCode(this)) + "[" + size + "/" + array.length + "](", ", ", ")") + " -> " + (if (next ne null) next.toString else "")
  }
}

// This is used by scala.collection.parallel.mutable.UnrolledParArrayCombiner:
// Todo -- revisit whether inheritance is the best way to achieve this functionality
private[collection] class DoublingUnrolledBuffer[T](implicit t: ClassTag[T]) extends UnrolledBuffer[T]()(t) {
  override def calcNextLength(sz: Int) = if (sz < 10000) sz * 2 else sz
  override protected def newUnrolled = new UnrolledBuffer.Unrolled[T](0, new Array[T](4), null, this)
}
