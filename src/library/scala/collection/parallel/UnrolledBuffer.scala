package scala.collection.parallel



import collection.generic._
import collection.mutable.Builder

import annotation.tailrec





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
 *  @author Aleksandar Prokopec
 *
 *  @coll unrolled buffer
 *  @Coll UnrolledBuffer
 */
class UnrolledBuffer[T](implicit val manifest: ClassManifest[T])
extends collection.mutable.Buffer[T]
   with collection.mutable.BufferLike[T, UnrolledBuffer[T]]
   with GenericClassManifestTraversableTemplate[T, UnrolledBuffer]
   with collection.mutable.Builder[T, UnrolledBuffer[T]]
{
  import UnrolledBuffer.Unrolled

  private var headptr = newUnrolled
  private var lastptr = headptr
  private var sz = 0

  private[parallel] def headPtr = headptr
  private[parallel] def headPtr_=(head: Unrolled[T]) = headptr = head
  private[parallel] def lastPtr = lastptr
  private[parallel] def lastPtr_=(last: Unrolled[T]) = lastptr = last
  private[parallel] def size_=(s: Int) = sz = s

  protected[this] override def newBuilder = new UnrolledBuffer[T]

  protected def newUnrolled = new Unrolled[T](this)

  private[collection] def calcNextLength(sz: Int) = sz

  def classManifestCompanion = UnrolledBuffer

  def concat(that: UnrolledBuffer[T]) = {
    // bind the two together
    if (!lastptr.bind(that.headptr)) lastptr = that.lastPtr

    // update size
    sz += that.sz

    // `that` is no longer usable, so clear it
    // here we rely on the fact that `clear` allocates
    // new nodes instead of modifying the previous ones
    that.clear

    // return a reference to this
    this
  }

  def +=(elem: T) = {
    lastptr = lastptr.append(elem)
    sz += 1
    this
  }

  def clear() {
    headptr = newUnrolled
    lastptr = headptr
    sz = 0
  }

  def iterator = new Iterator[T] {
    var pos: Int = -1
    var node: Unrolled[T] = headptr
    scan()

    private def scan() {
      pos += 1
      while (pos >= node.size) {
        pos = 0
        node = node.next
        if (node eq null) return
      }
    }
    def hasNext = node ne null
    def next = if (hasNext) {
      val r = node.array(pos)
      scan()
      r
    } else Iterator.empty.next
  }

  // this should be faster than the iterator
  override def foreach[U](f: T => U) = headptr.foreach(f)

  def result = this

  def length = sz

  def apply(idx: Int) =
    if (idx >= 0 && idx < sz) headptr(idx)
    else outofbounds(idx)

  def update(idx: Int, newelem: T) =
    if (idx >= 0 && idx < sz) headptr(idx) = newelem
    else outofbounds(idx)

  def remove(idx: Int) =
    if (idx >= 0 && idx < sz) {
      sz -= 1
      headptr.remove(idx, this)
    } else outofbounds(idx)

  def +=:(elem: T) = {
    headptr = headptr.prepend(elem)
    sz += 1
    this
  }

  def insertAll(idx: Int, elems: Traversable[T]) =
    if (idx >= 0 && idx <= sz) {
      headptr.insertAll(idx, elems, this)
      sz += elems.size
    } else outofbounds(idx)

  override def stringPrefix = "UnrolledBuffer"
}


object UnrolledBuffer extends ClassManifestTraversableFactory[UnrolledBuffer] {
  /** $genericCanBuildFromInfo */
  implicit def canBuildFrom[T](implicit m: ClassManifest[T]): CanBuildFrom[Coll, T, UnrolledBuffer[T]] =
    new GenericCanBuildFrom[T]
  def newBuilder[T](implicit m: ClassManifest[T]): Builder[T, UnrolledBuffer[T]] = new UnrolledBuffer[T]

  val waterline = 50
  val waterlineDelim = 100
  private[parallel] val unrolledlength = 32

  /** Unrolled buffer node.
   */
  class Unrolled[T: ClassManifest] private[parallel] (var size: Int, var array: Array[T], var next: Unrolled[T], val buff: UnrolledBuffer[T] = null) {
    private[parallel] def this() = this(0, new Array[T](unrolledlength), null, null)
    private[parallel] def this(b: UnrolledBuffer[T]) = this(0, new Array[T](unrolledlength), null, b)

    private def nextlength = if (buff eq null) unrolledlength else buff.calcNextLength(array.length)

    // adds and returns itself or the new unrolled if full
    @tailrec final def append(elem: T): Unrolled[T] = if (size < array.length) {
      array(size) = elem
      size += 1
      this
    } else {
      next = new Unrolled[T](0, new Array[T](nextlength), null, buff)
      next.append(elem)
    }
    def foreach[U](f: T => U) {
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
      newhead.append(elem)
      newhead.next = this
      newhead
    }
    // shifts right assuming enough space
    private def shiftright() {
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
    // shifts left elements after `leftb` (overwrites `leftb`)
    private def shiftleft(leftb: Int) {
      var i = leftb
      while (i < (size - 1)) {
        array(i) = array(i + 1)
        i += 1
      }
      nullout(i, i + 1)
    }
    protected def tryMergeWithNext() = if (next != null && (size + next.size) < (array.length * waterline / waterlineDelim)) {
      // copy the next array, then discard the next node
      Array.copy(next.array, 0, array, size, next.size)
      size = size + next.size
      next = next.next
      if (next eq null) true else false // checks if last node was thrown out
    } else false

    @tailrec final def insertAll(idx: Int, t: Traversable[T], buffer: UnrolledBuffer[T]): Unit = if (idx < size) {
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
      for (elem <- t) curr = curr append elem
      curr.next = newnextnode

      // try to merge the last node of this with the newnextnode
      if (curr.tryMergeWithNext()) buffer.lastPtr = curr
    } else insertAll(idx - size, t, buffer)
    private def nullout(from: Int, until: Int) {
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

    override def toString = array.take(size).mkString("Unrolled[" + array.length + "](", ", ", ")") + " -> " + (if (next ne null) next.toString else "")
  }

}

