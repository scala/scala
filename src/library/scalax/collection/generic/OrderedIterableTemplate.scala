/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection.generic

import OrderedIterable._

import util.control.Breaks._

/** Ordered iterables are iterables where the `elements` method always returns elements in the same
 *  order (namely the order in which elements were appended to the iterable). In particular, one has
 *  for every two ordered iterables `xs` and `ys`:
 *
 *  `(xs ++ ys).elements = xs.elements ++ ys.elements
 */
trait OrderedIterableTemplate[+CC[/*+*/B] <: OrderedIterableTemplate[CC, B] with OrderedIterable[B], /*+*/A]
  extends IterableTemplate[CC, A] {

  /** The last element of this iterable.
   *
   *  @throws Predef.NoSuchElementException if the iterable is empty.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Returns as an option the last element of this iterable or
   *  <code>None</code> if iterable is empty.
   *
   *  @return the last element as an option.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** An iterable consisting of all elements of this iterable except the last one.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def init: CC[A] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    var lst = head
    val b = newBuilder[A]
    for (x <- this) {
      b += lst
      lst = x
    }
    b.result
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeRight(n: Int): CC[A] = {
    val b = newBuilder[A]
    val lead = elements drop n
    var go = false
    for (x <- this) {
      if (go) b += x
      else if (lead.hasNext) lead.next
      else go = true
    }
    b.result
  }

  /** Returns the iterable wihtout its rightmost <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def dropRight(n: Int): CC[A] = {
    val b = newBuilder[A]
    val lead = elements drop n
    breakable {
      for (x <- this) {
        if (!lead.hasNext) break
        lead.next
        b += x
      }
    }
    b.result
  }

  /** Returns the longest prefix of this iterable whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeWhile(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    breakable {
      for (x <- this) {
        if (!p(x)) break
        b += x
      }
    }
    b.result
  }

  /** Returns the longest suffix of this iterable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def dropWhile(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    var go = false
    for (x <- this) {
      if (go) b += x
      else if (!p(x)) { go = true; b += x }
    }
    b.result
  }

 /** Returns a pair consisting of the longest prefix of the iterable whose
   *  elements all satisfy the given predicate, and the rest of the iterable.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the iterable whose
   *           elements all satisfy <code>p</code>, and the rest of the iterable.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def span(p: A => Boolean): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  /** Checks if the other iterable object contains the same elements as this one.
   *
   *  @note will not terminate for infinite-sized iterables.
   *  @param that  the other iterable
   *  @return true, iff both iterables contain the same elements.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def sameElements[B >: A](that: OrderedIterable[B]): Boolean = {
    val these = this.elements
    val those = that.elements
    while (these.hasNext && those.hasNext && these.next() == those.next()) {}
    !these.hasNext && !those.hasNext
  }
}
