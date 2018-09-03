package scala.collection
package mutable


/** A `Buffer` is a growable and shrinkable `Seq`. */
trait Buffer[A]
  extends Seq[A]
    with SeqOps[A, Buffer, Buffer[A]]
    with Growable[A]
    with Shrinkable[A] {

  override def iterableFactory: SeqFactory[Buffer] = Buffer

  //TODO Prepend is a logical choice for a readable name of `+=:` but it conflicts with the renaming of `append` to `add`
  /** Prepends a single element at the front of this $coll.
    *
    *  @param elem  the element to $add.
    *  @return the $coll itself
    */
  def prepend(elem: A): this.type

  /** Appends the given elements to this buffer.
    *
    *  @param elem  the element to append.
    */
  @`inline` final def append(elem: A): this.type = addOne(elem)

  @deprecated("Use appendAll instead", "2.13.0")
  @`inline` final def append(elems: A*): this.type = addAll(elems)

  /** Appends the elements contained in a iterable object to this buffer.
    *  @param xs  the iterable object containing the elements to append.
    */
  @`inline` final def appendAll(xs: IterableOnce[A]): this.type = addAll(xs)


  /** Alias for `prepend` */
  @`inline` final def +=: (elem: A): this.type = prepend(elem)

  def prependAll(elems: IterableOnce[A]): this.type = { insertAll(0, elems); this }

  @deprecated("Use prependAll instead", "2.13.0")
  @`inline` final def prepend(elems: A*): this.type = prependAll(elems)

  /** Alias for `prependAll` */
  @inline final def ++=:(elems: IterableOnce[A]): this.type = prependAll(elems)

  /** Inserts a new element at a given index into this buffer.
    *
    *  @param idx    the index where the new elements is inserted.
    *  @param elem   the element to insert.
    *  @throws   IndexOutOfBoundsException if the index `idx` is not in the valid range
    *            `0 <= idx <= length`.
    */
  @throws[IndexOutOfBoundsException]
  def insert(idx: Int, elem: A): Unit

  /** Inserts new elements at the index `idx`. Opposed to method
    *  `update`, this method will not replace an element with a new
    *  one. Instead, it will insert a new element at index `idx`.
    *
    *  @param idx     the index where a new element will be inserted.
    *  @param elems   the iterable object providing all elements to insert.
    *  @throws IndexOutOfBoundsException if `idx` is out of bounds.
    */
  @throws[IndexOutOfBoundsException]
  def insertAll(idx: Int, elems: IterableOnce[A]): Unit

  /** Removes the element at a given index position.
    *
    *  @param idx  the index which refers to the element to delete.
    *  @return   the element that was formerly at index `idx`.
    */
  @throws[IndexOutOfBoundsException]
  def remove(idx: Int): A

  /** Removes the element on a given index position. It takes time linear in
    *  the buffer size.
    *
    *  @param idx       the index which refers to the first element to remove.
    *  @param count   the number of elements to remove.
    *  @throws   IndexOutOfBoundsException if the index `idx` is not in the valid range
    *            `0 <= idx <= length - count` (with `count > 0`).
    *  @throws   IllegalArgumentException if `count < 0`.
    */
  @throws[IndexOutOfBoundsException]
  @throws[IllegalArgumentException]
  def remove(idx: Int, count: Int): Unit
  
  /** Removes a single element from this buffer, at its first occurrence.
    *  If the buffer does not contain that element, it is unchanged.
    *
    *  @param x  the element to remove.
    *  @return   the buffer itself
    */
  def subtractOne (x: A): this.type = {
    val i = indexOf(x)
    if (i != -1) remove(i)
    this
  }

  /** Removes the first ''n'' elements of this buffer.
    *
    *  @param n  the number of elements to remove from the beginning
    *            of this buffer.
    */
  def trimStart(n: Int): Unit = remove(0, normalized(n))

  /** Removes the last ''n'' elements of this buffer.
    *
    *  @param n  the number of elements to remove from the end
    *            of this buffer.
    */
  def trimEnd(n: Int): Unit = {
    val norm = normalized(n)
    remove(length - norm, norm)
  }

  def patchInPlace(from: Int, patch: scala.collection.Seq[A], replaced: Int): this.type

  // +=, ++=, clear inherited from Growable
  // Per remark of @ichoran, we should preferably not have these:
  //
  // def +=:(elem: A): this.type = { insert(0, elem); this }
  // def +=:(elem1: A, elem2: A, elems: A*): this.type = elem1 +=: elem2 +=: elems ++=: this
  // def ++=:(elems: IterableOnce[A]): this.type = { insertAll(0, elems); this }

  def dropInPlace(n: Int): this.type = { remove(0, normalized(n)); this }
  def dropRightInPlace(n: Int): this.type = {
    val norm = normalized(n)
    remove(length - norm, norm)
    this
  }
  def takeInPlace(n: Int): this.type = {
    val norm = normalized(n)
    remove(norm, length - norm)
    this
  }
  def takeRightInPlace(n: Int): this.type = { remove(0, length - normalized(n)); this }
  def sliceInPlace(start: Int, end: Int): this.type = takeInPlace(end).dropInPlace(start)
  private def normalized(n: Int): Int = math.min(math.max(n, 0), length)

  def dropWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) { clear(); this } else dropInPlace(idx)
  }
  def takeWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) this else takeInPlace(idx)
  }
  def padToInPlace(len: Int, elem: A): this.type = {
    while (length < len) +=(elem)
    this
  }

  override protected[this] def stringPrefix = "Buffer"
}

trait IndexedBuffer[A] extends IndexedSeq[A]
  with IndexedSeqOps[A, IndexedBuffer, IndexedBuffer[A]]
  with Buffer[A] {

  override def iterableFactory: SeqFactory[IndexedBuffer] = IndexedBuffer

  def flatMapInPlace(f: A => IterableOnce[A]): this.type = {
    // There's scope for a better implementation which copies elements in place.
    var i = 0
    val s = size
    val newElems = new Array[IterableOnce[A]](s)
    while (i < s) { newElems(i) = f(this(i)); i += 1 }
    clear()
    i = 0
    while (i < s) { ++=(newElems(i)); i += 1 }
    this
  }

  def filterInPlace(p: A => Boolean): this.type = {
    var i, j = 0
    while (i < size) {
      if (p(apply(i))) {
        if (i != j) {
          this(j) = this(i)
        }
        j += 1
      }
      i += 1
    }

    if (i == j) this else takeInPlace(j)
  }

  def patchInPlace(from: Int, patch: scala.collection.Seq[A], replaced: Int): this.type = {
    val replaced0 = math.min(math.max(replaced, 0), length)
    val i = math.min(math.max(from, 0), length)
    var j = 0
    val n = math.min(patch.length, replaced0)
    while (j < n && i + j < length) {
      update(i + j, patch(j))
      j += 1
    }
    if (j < patch.length) insertAll(i + j, patch.iterator.drop(j))
    else if (j < replaced0) remove(i + j, math.min(replaced0 - j, length - i - j))
    this
  }
}

@SerialVersionUID(3L)
object Buffer extends SeqFactory.Delegate[Buffer](ArrayBuffer)

@SerialVersionUID(3L)
object IndexedBuffer extends SeqFactory.Delegate[IndexedBuffer](ArrayBuffer)

/** Explicit instantiation of the `Buffer` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractBuffer[A] extends AbstractSeq[A] with Buffer[A]
