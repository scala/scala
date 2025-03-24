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

import scala.annotation.nowarn


/** A `Buffer` is a growable and shrinkable `Seq`. */
trait Buffer[A]
  extends Seq[A]
    with SeqOps[A, Buffer, Buffer[A]]
    with Growable[A]
    with Shrinkable[A]
    with IterableFactoryDefaults[A, Buffer] {

  override def iterableFactory: SeqFactory[Buffer] = Buffer

  override def knownSize: Int = super[Seq].knownSize

  //TODO Prepend is a logical choice for a readable name of `+=:` but it conflicts with the renaming of `append` to `add`
  /** Prepends a single element at the front of this $coll.
    *
    *  @param elem  the element to $add.
    *  @return the $coll itself
    */
  def prepend(elem: A): this.type

  /** Appends the given elements to this buffer.
   *
   *  @param elem the element to append.
   *  @return     this $coll
   */
  @`inline` final def append(elem: A): this.type = addOne(elem)

  @deprecated("Use appendAll instead", "2.13.0")
  @`inline` final def append(elems: A*): this.type = addAll(elems)

  /** Appends the elements contained in a iterable object to this buffer.
   *  @param elems  the iterable object containing the elements to append.
   *  @return       this $coll
   */
  @`inline` final def appendAll(@deprecatedName("xs") elems: IterableOnce[A]): this.type = addAll(elems)

  /** Alias for `prepend` */
  @`inline` final def +=: (elem: A): this.type = prepend(elem)

  /** Prepends the elements contained in a iterable object to this buffer.
   *  @param elems  the iterable object containing the elements to append.
   *  @return       this $coll
   */
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
  @deprecated("use dropInPlace instead", since = "2.13.4")
  def trimStart(n: Int): Unit = dropInPlace(n)

  /** Removes the last ''n'' elements of this buffer.
    *
    *  @param n  the number of elements to remove from the end
    *            of this buffer.
    */
  @deprecated("use dropRightInPlace instead", since = "2.13.4")
  def trimEnd(n: Int): Unit = dropRightInPlace(n)

  /** Replaces a slice of elements in this $coll by another sequence of elements.
   *
   *  Patching at negative indices is the same as patching starting at 0.
   *  Patching at indices at or larger than the length of the original $coll appends the patch to the end.
   *  If the `replaced` count would exceed the available elements, the difference in excess is ignored.
   *
   *  @param  from     the index of the first replaced element
   *  @param  patch    the replacement sequence
   *  @param  replaced the number of elements to drop in the original $coll
   *  @return          this $coll
   */
  def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A], replaced: Int): this.type

  // +=, ++=, clear inherited from Growable
  // Per remark of @ichoran, we should preferably not have these:
  //
  // def +=:(elem: A): this.type = { insert(0, elem); this }
  // def +=:(elem1: A, elem2: A, elems: A*): this.type = elem1 +=: elem2 +=: elems ++=: this
  // def ++=:(elems: IterableOnce[A]): this.type = { insertAll(0, elems); this }

  /** Removes the first `n` elements from this $coll.
   *
   *  @param  n the number of elements to remove
   *  @return this $coll
   *
   */
  def dropInPlace(n: Int): this.type = { remove(0, normalized(n)); this }

  /** Removes the last `n` elements from this $coll.
   *
   *  @param  n the number of elements to remove
   *  @return this $coll
   *
   */
  def dropRightInPlace(n: Int): this.type = {
    val norm = normalized(n)
    remove(length - norm, norm)
    this
  }

  /** Retains the first `n` elements from this $coll and removes the rest.
   *
   *  @param  n the number of elements to retain
   *  @return this $coll
   *
   */
  def takeInPlace(n: Int): this.type = {
    val norm = normalized(n)
    remove(norm, length - norm)
    this
  }

  /** Retains the last `n` elements from this $coll and removes the rest.
   *
   *  @param  n the number of elements to retain
   *  @return this $coll
   *
   */
  def takeRightInPlace(n: Int): this.type = { remove(0, length - normalized(n)); this }

  /** Retains the specified slice from this $coll and removes the rest.
   *
   *  @param  start the lowest index to include
   *  @param  end   the lowest index to exclude
   *  @return this $coll
   *
   */
  def sliceInPlace(start: Int, end: Int): this.type = takeInPlace(end).dropInPlace(start)

  private def normalized(n: Int): Int = math.min(math.max(n, 0), length)

  /** Drops the longest prefix of elements that satisfy a predicate.
   *
   *  @param   p  The predicate used to test elements.
   *  @return this $coll
   *  @see [[dropWhile]]
   */
  def dropWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) { clear(); this } else dropInPlace(idx)
  }

  /** Retains the longest prefix of elements that satisfy a predicate.
   *
   *  @param   p  The predicate used to test elements.
   *  @return this $coll
   *  @see [[takeWhile]]
   */
  def takeWhileInPlace(p: A => Boolean): this.type = {
    val idx = indexWhere(!p(_))
    if (idx < 0) this else takeInPlace(idx)
  }

  /** Append the given element to this $coll until a target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @return this $coll
   */
  def padToInPlace(len: Int, elem: A): this.type = {
    while (length < len) +=(elem)
    this
  }

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "Buffer"
}

trait IndexedBuffer[A] extends IndexedSeq[A]
  with IndexedSeqOps[A, IndexedBuffer, IndexedBuffer[A]]
  with Buffer[A]
  with IterableFactoryDefaults[A, IndexedBuffer] {

  override def iterableFactory: SeqFactory[IndexedBuffer] = IndexedBuffer

  /** Replace the contents of this $coll with the flatmapped result.
   *
   *  @param f the mapping function
   *  @return this $coll
   */
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

  /** Replace the contents of this $coll with the filtered result.
   *
   *  @param f the filtering function
   *  @return this $coll
   */
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

  def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A], replaced: Int): this.type = {
    val replaced0 = math.min(math.max(replaced, 0), length)
    val i = math.min(math.max(from, 0), length)
    var j = 0
    val iter = patch.iterator
    while (iter.hasNext && j < replaced0 && i + j < length) {
      update(i + j, iter.next())
      j += 1
    }
    if (iter.hasNext) insertAll(i + j, iter)
    else if (j < replaced0) remove(i + j, math.min(replaced0 - j, length - i - j))
    this
  }
}

@SerialVersionUID(3L)
object Buffer extends SeqFactory.Delegate[Buffer](ArrayBuffer)

@SerialVersionUID(3L)
object IndexedBuffer extends SeqFactory.Delegate[IndexedBuffer](ArrayBuffer)

/** Explicit instantiation of the `Buffer` trait to reduce class file size in subclasses. */
abstract class AbstractBuffer[A] extends AbstractSeq[A] with Buffer[A]
