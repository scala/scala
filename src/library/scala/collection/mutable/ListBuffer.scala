/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable;


import Predef._;

/**
 */
final class ListBuffer[A] extends Buffer[A] {
  private var start: List[A] = Nil;
  private var last: ::[A] = _;
  private var exported: boolean = false;

  /** Prepend a single element to this buffer.
   *
   *  @param x  the element to prepend.
   */
  def +: (x: A): Buffer[A] = {
    if (exported) copy()
    val newElem = new scala.:: (x, start)
    if (start.isEmpty) last = newElem
    start = newElem
    this
  }

  /** Append a single element to this buffer.
   *
   *  @param x  the element to append.
   */
  override def += (x: A): unit = {
    if (exported) copy()
    if (start.isEmpty) {
      last = new scala.:: (x, Nil)
      start = last
    } else {
      val last1 = last
      last = new scala.:: (x, Nil)
      last1.tl = last
    }
  }

  /** Remove a single element from the buffer and return
   *  the identity of the buffer. Same as ``this -= x; this''
   *
   *  @param x  the element to remove.
   */
  def - (x: A): Buffer[A] = { this -= x; this }

  /** Remove a single element from this buffer.
   *
   *  @param x  the element to remove.
   */
  def -= (x: A): unit = {
    if (exported) copy()
    if (start.isEmpty) {}
    else if (start.head == x) start = start.tail
    else {
      var cursor = start
      while (!cursor.tail.isEmpty && cursor.tail.head != x) { cursor = cursor.tail }
      if (!cursor.tail.isEmpty) cursor.asInstanceOf[scala.::[A]].tl = cursor.tail.tail
    }
  }

  /** Converts this buffer to a list
   */
  override def toList: List[A] = {
    exported = !start.isEmpty
    start
  }

  /** Prepends the elements of this buffer to a given list
   *
   *  @param xs   the list to which elements are prepended
   */
  def prependToList(xs: List[A]): List[A] =
    if (start.isEmpty) xs
    else { last.tl = xs; toList }

  /** Clears the buffer contents.
   */
  def clear: unit = {
    start = Nil
    exported = false
  }

  /** Copy contents of this buffer */
  private def copy() = {
    var cursor = start
    val limit = last.tail
    clear
    while (cursor ne limit) {
      this += cursor.head
      cursor = cursor.tail
    }
  }

  /** Returns the length of this buffer.
   */
  def length: int = start.length

  private def noElem(n: int): All = error("element " + n + " does not exist in buffer");

  /** Returns the <code>n</code>th element of this list. This method
   *  yields an error if the element does not exist.
   */
  def apply(n: Int): A = try {
    start(n)
  } catch {
    case ex: Error => noElem(n)
  }

  /** Replace element at index <code>n</code> with the new element
   *  <code>newelem</code>.
   *
   *  @param n       the index of the element to replace.
   *  @param x       the new element.
   */
  def update(n: Int, x: A): unit = try {
    if (exported) copy()
    if (n == 0) {
      val newElem = new scala.:: (x, start.tail);
      if (last eq start) last = newElem
      start = newElem
    } else {
      var cursor = start;
      var i = 1;
      while (i < n) {
        cursor = cursor.tail
        i = i + 1
      }
      val newElem = new scala.:: (x, cursor.tail.tail)
      if (last eq cursor.tail) last = newElem
      cursor.asInstanceOf[scala.::[A]].tl = newElem
    }
  } catch {
    case ex: Error => noElem(n)
  }

  /** Inserts new elements at the index <code>n</code>. Opposed to method
   *  <code>update</code>, this method will not replace an element with a new
   *  one. Instead, it will insert a new element at index <code>n</code>.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param iter  the iterable object providing all elements to insert.
   */
  def insertAll(n: Int, iter: Iterable[A]): unit = try {
    if (exported) copy()
    var elems = iter.elements.toList.reverse;
    if (n == 0) {
      while (!elems.isEmpty) {
        val newElem = new scala.:: (elems.head, start);
        if (start.isEmpty) last = newElem
        start = newElem
        elems = elems.tail
      }
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i = i + 1
      }
      while (!elems.isEmpty) {
        val newElem = new scala.:: (elems.head, cursor.tail);
        if (cursor.tail.isEmpty) last = newElem
        cursor.asInstanceOf[scala.::[A]].tl = newElem
        elems = elems.tail
      }
    }
  } catch {
    case ex: Error => noElem(n)
  }

  /** Removes the element on a given index position.
   *
   *  @param n       the index which refers to the element to delete.
   *  @return n      the element that was formerly at posiition `n'
   *  @pre           an element exists at position `n'
   */
  def remove(n: Int): A = try {
    if (exported) copy()
    var old = start.head;
    if (n == 0) {
      start = start.tail
    } else {
      var cursor = start;
      var i = 1;
      while (i < n) {
        cursor = cursor.tail
        i = i + 1
      }
      old = cursor.tail.head
      if (last eq cursor.tail) last = cursor.asInstanceOf[scala.::[A]]
      cursor.asInstanceOf[scala.::[A]].tl = cursor.tail.tail
    }
    old
  } catch {
    case ex: Error => noElem(n)
  }

  /** Returns an iterator over all elements of this list.
   *  Note: the iterator can be affected by insertions, updates and deletions
   *  that are performed afterwards on the buffer. To get iterator an over the current
   *  buffer snapshot, use  toList.elements
   */
  override def elements = new Iterator[A] {
    var cursor: List[A] = null;
    def hasNext: Boolean = !start.isEmpty && cursor != last;
    def next: A =
      if (!hasNext) {
        error("next on empty Iterator")
      } else {
        if (cursor == null) cursor = start else cursor = cursor.tail;
        cursor.head
      }
  }

  /** Return a clone of this buffer.
   *
   *  @return a <code>ListBuffer</code> with the same elements.
   */
  override def clone(): Buffer[A] = (new ListBuffer[A]) ++ this

  /** Checks if two buffers are structurally identical.
   *
   *  @return true, iff both buffers contain the same sequence of elements.
   */
  override def equals(obj: Any): Boolean = obj match {
    case that: ListBuffer[A] =>
      (this.length == that.length &&
       elements.zip(that.elements).forall {
         case Pair(thiselem, thatelem) => thiselem == thatelem;
       })
    case _ => false
  }

  /** Defines the prefix of the string representation.
   */
  override protected def stringPrefix: String = "ListBuffer";
}

