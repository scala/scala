/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import mutable.ListBuffer
import immutable.List
import scala.util.control.Breaks._

/** A template trait for linear sequences of type `LinearSeq[A]`  which optimizes
 *  the implementation of several methods under the assumption of fast linear access.
 *
 *  $linearSeqInfo
 */
trait LinearSeqOptimized[+A, +Repr <: LinearSeqOptimized[A, Repr]] extends LinearSeqLike[A, Repr] { self: Repr =>

  def isEmpty: Boolean

  def head: A

  def tail: Repr

  /** The length of the $coll.
   *
   *  $willNotTerminateInf
   *
   *  Note: the execution of `length` may take time proportial to the length of the sequence.
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

  /** Selects an element by its index in the $coll.
   *  Note: the execution of `apply` may take time proportial to the index value.
   *  @throws `IndexOutOfBoundsException` if `idx` does not satisfy `0 <= idx < length`.
   */
  def apply(n: Int): A = {
    val rest = drop(n)
    if (n < 0 || rest.isEmpty) throw new IndexOutOfBoundsException("" + n)
    rest.head
  }

  override /*IterableLike*/
  def foreach[B](f: A => B) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }


  override /*IterableLike*/
  def forall(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  override /*IterableLike*/
  def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  override /*SeqLike*/
  def contains[A1 >: A](elem: A1): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }
  
  override /*IterableLike*/
  def find(p: A => Boolean): Option[A] = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  override /*TraversableLike*/
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }
  
  override /*IterableLike*/
  def foldRight[B](z: B)(f: (A, B) => B): B =
    if (this.isEmpty) z
    else f(head, tail.foldRight(z)(f))

  override /*TraversableLike*/
  def reduceLeft[B >: A](f: (B, A) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else tail.foldLeft[B](head)(f)

  override /*IterableLike*/
  def reduceRight[B >: A](op: (A, B) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("Nil.reduceRight")
    else if (tail.isEmpty) head
    else op(head, tail.reduceRight(op))

  override /*TraversableLike*/
  def last: A = {
    if (isEmpty) throw new NoSuchElementException
    var these = this
    var nx = these.tail
    while (!nx.isEmpty) {
      these = nx
      nx = nx.tail
    }
    these.head
  }

  override /*IterableLike*/
  def take(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    var these = repr
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    b.result
  }

  override /*TraversableLike*/
  def drop(n: Int): Repr = {
    var these: Repr = repr
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    // !!! This line should actually be something like:
    //   newBuilder ++= these result
    // since we are in collection.*, not immutable.*.
    // However making that change will pessimize all the
    // immutable linear seqs (like list) which surely expect
    // drop to share.  (Or at least it would penalize List if
    // it didn't override drop.  It would be a lot better if
    // the leaf collections didn't override so many methods.)
    //
    // Upshot: MutableList is broken and passes part of the
    // original list as the result of drop.
    these
  }

  override /*IterableLike*/
  def dropRight(n: Int): Repr = {
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

  override /*IterableLike*/
  def slice(from: Int, until: Int): Repr = {
    var these: Repr = repr
    var count = from max 0
    if (until <= count)
      return newBuilder.result

    val b = newBuilder
    var sliceElems = until - count
    while (these.nonEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    while (these.nonEmpty && sliceElems > 0) {
      sliceElems -= 1
      b += these.head
      these = these.tail
    }
    b.result
  }

  override /*IterableLike*/
  def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.result
  }

  override /*TraversableLike*/
  def span(p: A => Boolean): (Repr, Repr) = {
    var these: Repr = repr
    val b = newBuilder
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.result, these)
  }

  override /*IterableLike*/
  def sameElements[B >: A](that: GenIterable[B]): Boolean = that match {
    case that1: LinearSeq[_] =>
      var these = this
      var those = that1
      while (!these.isEmpty && !those.isEmpty && these.head == those.head) {
        these = these.tail
        those = those.tail
      }
      these.isEmpty && those.isEmpty
    case _ =>
      super.sameElements(that)
  }

  override /*SeqLike*/
  def lengthCompare(len: Int): Int =  {
    var i = 0
    var these = self
    while (!these.isEmpty && i <= len) {
      i += 1
      these = these.tail
    }
    i - len
  }

  override /*SeqLike*/
  def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) > 0

  override /*SeqLike*/
  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var these = this drop from
    while (!these.isEmpty && p(these.head)) {
      i += 1
      these = these.tail
    }
    i
  }

  override /*SeqLike*/
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (these.nonEmpty) {
      if (p(these.head))
        return i

      i += 1
      these = these.tail
    }
    -1
  }

  override /*SeqLike*/
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
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
}
