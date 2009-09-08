/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import mutable.ListBuffer
// import immutable.{List, Nil, ::}
import generic._
import scala.util.control.Breaks._

/** Class <code>Linear[A]</code> represents linear sequences of elements.
 *  For such sequences `isEmpty`, `head` and `tail` are guaranteed to be
 *  efficient constant time (or near so) operations.
 *  It does not add any methods to <code>Sequence</code> but overrides
 *  several methods with optimized implementations.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 */
trait LinearSequenceTemplate[+A, +This <: LinearSequenceTemplate[A, This] with LinearSequence[A]] extends SequenceTemplate[A, This] { self =>

  /** Abstract method to be implemented in a subclass */
  def isEmpty: Boolean

  /** Abstract method to be implemented in a subclass */
  def head: A

  /** Abstract method to be implemented in a subclass */
  def tail: This

   /** Returns the number of elements in the linear sequence.
    */
  def length: Int = {
    var these = self
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Returns the <code>n</code>-th element of this linear sequence. The first element
   *  (head of the linear sequence) is at position 0.
   *
   *  @param n index of the element to return
   *  @return  the element at position <code>n</code> in this linear sequence.
   *  @throws Predef.NoSuchElementException if the linear sequence is too short.
   */
  def apply(n: Int): A = drop(n).head

  /** Returns the elements in the sequence as an iterator
   */
  override def iterator: Iterator[A] = new Iterator[A] {
    var these = self
    def hasNext: Boolean = !these.isEmpty
    def next: A =
      if (hasNext) {
        val result = these.head; these = these.tail; result
      } else Iterator.empty.next
    override def toList: List[A] = these.toList
  }

  /** Apply the given function <code>f</code> to each element of this linear sequence
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   */
  override def foreach[B](f: A => B) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  /** Tests if the predicate <code>p</code> is satisfied by all elements
   *  in this list.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff all elements of this list satisfy the
   *           predicate <code>p</code>.
   */
  override def forall(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  /** Tests the existence in this list of an element that satisfies the
   *  predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff there exists an element in this list that
   *           satisfies the predicate <code>p</code>.
   */
  override def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  /** Count the number of elements in the iterable which satisfy a predicate.
   *
   *  @param p the predicate for which to count
   *  @return  the number of elements satisfying the predicate <code>p</code>.
   */
  override def count(p: A => Boolean): Int = {
    var these = this
    var cnt = 0
    while (!these.isEmpty) {
      if (p(these.head)) cnt += 1
      these = these.tail
    }
    cnt
  }

  /** Find and return the first element of the list satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return the first element in the list satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  override def find(p: A => Boolean): Option[A] = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  override def foldRight[B](z: B)(f: (A, B) => B): B =
    if (this.isEmpty) z
    else f(head, tail.foldRight(z)(f))

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from left to right.
   *
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the list has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  override def reduceLeft[B >: A](f: (B, A) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else tail.foldLeft[B](head)(f)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  override def reduceRight[B >: A](op: (A, B) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("Nil.reduceRight")
    else if (tail.isEmpty) head
    else op(head, tail.reduceRight(op))

  /** The last element of this linear sequence.
   *
   *  @throws Predef.NoSuchElementException if the linear sequence is empty.
   */
  override def last: A = {
    if (isEmpty) throw new NoSuchElementException
    var these = this
    var nx = these.tail
    while (!nx.isEmpty) {
      these = nx
      nx = nx.tail
    }
    these.head
  }

  override def take(n: Int): This = {
    val b = newBuilder
    var i = 0
    var these = this
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    b.result
  }

  override def drop(n: Int): This = {
    var these: This = thisCollection
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *  @param n the number of elements to take
   */
  override def dropRight(n: Int): This = {
    val b = newBuilder
    var these = this
    var lead = this drop n
    while (!lead.isEmpty) {
      b += these.head
      these = these.tail
      lead = lead.tail
    }
    b.result
  }

  /** Returns a pair consisting of the longest prefix of the linear sequence whose
   *  elements all satisfy the given predicate, and the rest of the linear sequence.
   *
   *  @param p the test predicate
   */
  override def span(p: A => Boolean): (This, This) = {
    var these: This = thisCollection
    val b = newBuilder
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.result, these)
  }

  /** Returns true iff the other linear sequence contains the same elements as this one.
   *
   *  @note will not terminate for two infinite-sized linear sequences.
   *  @param that  the other linear sequence
   */
  override def sameElements[B >: A](that: Iterable[B]): Boolean = that match {
    case that1: LinearSequence[_] =>
      // !!! todo: do the LinearSequence methods need to assume null might be
      // used to indicate termination or not? The fact that null is checked for
      // here would seem to indicate "yes", but the comment in LinkedListTemplate is
      //   !!! todo: integrate with LinearSequence, need to drop null then.
      // which contradicts the need for null checking here in two different ways:
      //   1) LinkedList does not currently inherit from LinearSequenceTemplate
      //   2) According to that comment, if it does it will stop using null
      //      (but what is the alternative?)
      def isEmpty(xs: LinearSequence[_]) = xs == null || xs.isEmpty
      var these = thisCollection
      var those = that1

      while (!isEmpty(these) && !isEmpty(those) && these.head == those.head) {
        these = these.tail
        those = those.tail
      }
      isEmpty(these) && isEmpty(those)
    case _ => super.sameElements(that)
  }

  // Overridden methods from Sequence

  /** Result of comparing <code>length</code> with operand <code>len</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this.length &lt; len</code>
   *  <code>x == 0</code>   iff    <code>this.length == len</code>
   *  <code>x &gt; 0</code>    iff    <code>this.length &gt; len</code>.
   */
  override def lengthCompare(len: Int): Int =  {
    var i = 0
    var these = self
    while (!these.isEmpty && i <= len) {
      i += 1
      these = these.tail
    }
    i - len
  }

  /** Is this partial function defined for the index <code>x</code>?
   */
  override def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) > 0

  /** Returns length of longest segment starting from a start index `from`
   *  such that every element of the segment satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  override def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var these = this drop from
    while (!these.isEmpty && p(these.head)) {
      i += 1
      these = these.tail
    }
    i
  }

  /** Returns index of the first element starting from a start index
   *  satisying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized linear sequences.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  override def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (!these.isEmpty && !p(these.head)) {
      i += 1
      these = these.tail
    }
    if (these.isEmpty) -1 else i
  }

  /** Returns index of the last element satisying a predicate, or -1, if none exists.
   *
   *  @param  p the predicate
   *  @return   the index of the last element satisfying <code>p</code>,
   *            or -1 if such an element does not exist
   */
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = 0
    var these = this
    var last = -1
    while (!these.isEmpty && i <= end) {
      if (p(these.head)) last = i
      these = these.tail
      i += 1
    }
    last
  }

  override def equals(that: Any): Boolean = that match {
    case that: LinearSequence[_]  => this sameElements that
    case _                        => super.equals(that)
  }
}
