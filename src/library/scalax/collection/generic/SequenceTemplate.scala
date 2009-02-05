/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scalax.collection.generic

import util.control.Breaks._
import scalax.collection.immutable.{List, Nil, ::}

import Sequence._

trait SequenceTemplate[+CC[/*+*/B] <: SequenceTemplate[CC, B] with Sequence[B], /*+*/A] extends PartialFunction[Int, A] with OrderedIterableTemplate[CC, A] {
self /*: CC[A]*/ =>

  /** Returns the length of the sequence.
   *
   *  @return the sequence length.
   */
  def length: Int

  /** Result of comparing <code>length</code> with operand <code>len</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this.length &lt; len</code>
   *  <code>x == 0</code>   iff    <code>this.length == len</code>
   *  <code>x &gt; 0</code>    iff    <code>this.length &gt; len</code>.
   *
   *  The method as implemented here does not call length directly; its running time
   *  is O(length min len) instead of O(length). The method should be overwritten
   *  if computing length is cheap.
   */
  def lengthCompare(len: Int): Int =  {
    var i = 0
    breakable {
      for (_ <- this) {
        i += 1
        if (i > len) break
      }
    }
    i - len
  }

  /** Should always be <code>length</code> */
  def size = length

  /** Is this partial function defined for the index <code>x</code>?
   */
  def isDefinedAt(x: Int): Boolean = (x >= 0) && (x < length)

  /** Returns length of longest segment starting from a start index `from`
   *  such that every element of the segment satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  def segmentLength(p: A => Boolean, from: Int): Int = {
    var result = 0
    var i = from
    breakable {
      for (x <- this) {
        if (i >= from && !p(x)) { result = i - from; break }
        else i += 1
      }
    }
    result
  }

  /** Returns length of longest prefix of this seqence
   *  such that every element of the prefix satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   */
  def prefixLength(p: A => Boolean) = segmentLength(p, 0)

  /** Returns index of the first element satisfying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Returns index of the first element starting from a start index
   *  satisying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var result = -1
    var i = from
    breakable {
      for (x <- this) {
        if (i >= from && p(x)) { result = i; break }
        else i += 1
      }
    }
    result
  }

  /** Returns index of the first element satisying a predicate, or -1.
   *
   *  @deprecated  Use `indexWhere` instead
   */
  @deprecated def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   *  @return the index in this sequence of the first occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object,  starting from a start index, or
   *  -1, if none exists.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   */
  def indexOf[B >: A](elem: B, from: Int): Int = indexWhere(elem ==, from)

 /** Returns the index of the last occurence of the specified element
   *  in this sequence, or -1 if the sequence does not contain this element.
   *
   *  @param  elem   element to search for.
   *  @return the index in this sequence of the last occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def lastIndexOf[B >: A](elem: B): Int = lastIndexOf(elem, length - 1)

  /** Returns the index of the last
    *  occurence of the specified element in this sequence
    *  before or at a given end index,
    *  or -1 if the sequence does not contain this element.
    *
    *  @param  elem   element to search for.
    *  @param  end    the end index
    */
  def lastIndexOf[B >: A](elem: B, end: Int): Int = {
    var i = end
    val it = reversedElements
    while (it.hasNext && it.next != elem) i -= 1
    i
  }

  /** Returns index of the last element satisying a predicate, or -1, if none exists.
   *
   *  @param  p the predicate
   *  @return   the index of the last element satisfying <code>p</code>,
   *            or -1 if such an element does not exist
   */
  def lastIndexWhere(p: A => Boolean): Int = lastIndexWhere(p, length - 1)

  /** Returns index of the last element not exceeding a given end index
   *  and satisying a predicate, or -1 if none exists.
   *
   *  @param  end the end index
   *  @param  p the predicate
   */
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = end
    val it = reversedElements
    while (it.hasNext && (i > end || !p(it.next))) i -= 1
    i
  }

  /** A sequence of type <code>C</code> consisting of all elements of
   *  this sequence in reverse order.
   *  @note  the operation is implemented by building a new sequence
   *         from <code>this(length - 1), ..., this(0)</code>
   *  If random access is inefficient for the given sequence implementation,
   *  this operation should be overridden.
   */
  def reverse: CC[A] = {
    var xs: List[A] = List()
    for (x <- this)
      xs = x :: xs
    val b = newBuilder[A]
    for (x <- xs)
      b += x
    b.result
  }

  /** The elements of this sequence in reversed order
   */
  def reversedElements: Iterator[A] = reverse.elements

  /**
   * Checks whether the argument sequence is contained at the
   * specified index within the receiver object.
   *
   * If the both the receiver object, <code>this</code> and
   * the argument, <code>that</code> are infinite sequences
   * this method may not terminate.
   *
   * @return true if <code>that</code> is contained in
   * <code>this</code>, at the specified index, otherwise false
   *
   * @see String.startsWith
   */
  def startsWith[B](that: Sequence[B], offset: Int): Boolean = {
    val i = elements.drop(offset)
    val j = that.elements
    while (j.hasNext && i.hasNext && i.next == j.next) {}
    !j.hasNext
  }

  /**
   * Check whether the receiver object starts with the argument sequence.
   *
   * @return true if <code>that</code> is a prefix of <code>this</code>,
   * otherwise false
   *
   * @see Sequence.startsWith
   */
  def startsWith[B](that: Sequence[B]): Boolean = startsWith(that, 0)

  /** @return true if this sequence end with that sequence
   *  @see String.endsWith
   */
  def endsWith[B](that: Sequence[B]): Boolean = {
    val i = this.elements.drop(length - that.length)
    val j = that.elements
    while (i.hasNext && j.hasNext && i.next == j.next) ()
    !j.hasNext
  }

  /** @return -1 if <code>that</code> not contained in this, otherwise the
   *  index where <code>that</code> is contained
   *  @see String.indexOf
   */
  def indexOf[B >: A](that: Sequence[B]): Int = {
    var i = 0
    var s: Sequence[A] = thisCC
    while (!s.isEmpty && !(s startsWith that)) {
      i += 1
      s = s drop 1
    }
    if (!s.isEmpty || that.isEmpty) i else -1
  }

  /** Tests if the given value <code>elem</code> is a member of this
   *  sequence.
   *
   *  @param elem element whose membership has to be tested.
   *  @return     <code>true</code> iff there is an element of this sequence
   *              which is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists (_ == elem)

  /** Computes the union of this sequence and the given sequence
   *  <code>that</code>.
   *
   *  @param that the sequence of elements to add to the sequence.
   *  @return     an sequence containing the elements of this
   *              sequence and those of the given sequence <code>that</code>
   *              which are not contained in this sequence.
   */
  def union[B >: A](that: Sequence[B]): CC[B] = this ++ (that diff thisCC)

  /** Computes the difference between this sequence and the given sequence
   *  <code>that</code>.
   *
   *  @param that the sequence of elements to remove from this sequence.
   *  @return     this sequence without the elements of the given sequence
   *              <code>that</code>.
   */
  def diff[B >: A](that: Sequence[B]): CC[A] = this remove (that contains _)

  /** Computes the intersection between this sequence and the given sequence
   *  <code>that</code>.
   *
   *  @param that the sequence to intersect.
   *  @return     the sequence of elements contained both in this sequence and
   *              in the given sequence <code>that</code>.
   */
  def intersect[B >: A](that: Sequence[B]): CC[A] = this filter(that contains _)

  /** Builds a new sequence from this sequence in which any duplicates (wrt to ==) removed.
   *  Among duplicate elements, only the first one is retained in the result sequence
   */
  def removeDuplicates: CC[A] = {
    val b = newBuilder[A]
    var seen = Set[A]()
    for (x <- this) {
      if (!(seen contains x)) {
        b += x
        seen += x
      }
    }
    b.result
  }

  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements.
   */
  def padTo[B >: A](len: Int, elem: B): CC[B] = {
    var diff = len - length
    val b = newBuilder[B] //!!! drop [B] and get surprising results!
    b ++= thisCC
    while (diff > 0) {
      b += elem
      diff -=1
    }
    b.result
  }

  /**
   *  Overridden for efficiency.
   *
   *  @return  the sequence itself
   */
  override def toSequence: Sequence[A] = thisCC

  /** Force toString from Iterable, not from Function */
  override def toString = super[OrderedIterableTemplate].toString

  def indices: Range = 0 until length

  /** Creates a view of this iterable @see OrderedIterable.View
   */
  override def view: SequenceView[CC, A] = new SequenceView[CC, A] { // !!! Martin: We should maybe infer the type parameters here?
    val origin: Sequence[_] = thisCC
    val elements: Iterator[A] = self.elements
    val length: Int = self.length
    def apply(idx: Int): A = self.apply(idx)
  }

  /** A sub-sequence view  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `view` and `slice` is that `view` produces
   *         a view of the current sequence, whereas `slice` produces a new sequence.
   *
   *  @note view(from, to)  is equivalent to view.slice(from, to)
   */
  override def view(from: Int, until: Int): SequenceView[CC, A] = view.slice(from, until)

  /** Returns index of the last element satisying a predicate, or -1.
   *  @deprecated use `lastIndexWhere` instead
   */
  @deprecated def findLastIndexOf(p: A => Boolean): Int = lastIndexWhere(p)

  /** A sub-sequence starting at index <code>from</code>
    *  and extending up to the length of the current sequence
    *
    *  @param from   The index of the first element of the slice
    *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
    *  @deprecated   use <code>drop</code> instead
    */
  @deprecated def slice(from: Int): Sequence[A] = slice(from, length)

  /** @deprecated Should be replaced by
   *
   *   <code>(s1, s2) forall { case (x, y) => f(x, y) }</code>
   */
  @deprecated def equalsWith[B](that: Sequence[B])(f: (A,B) => Boolean): Boolean = {
    val i = this.elements
    val j = that.elements
    while (i.hasNext && j.hasNext && f(i.next, j.next)) ()
    !i.hasNext && !j.hasNext
  }

  /** Is <code>that</code> a slice in this?
   *  @deprecated  Should be repaced by <code>indexOf(that) != -1</code>
   */
  @deprecated def containsSlice[B](that: Sequence[B]): Boolean = indexOf(that) != -1

}
