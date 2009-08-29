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
import annotation.unchecked.uncheckedVariance

import util.control.Breaks._
// import immutable.Stream // !!!

/** <p>
 *    A template trait for iterable collections.
 *  </p>
 *  <p>
 *    Collection classes mixing in this trait provide a method
 *    <code>iterator</code> which returns an iterator over all the
 *    elements contained in the collection. They also provide a method
 *    <code>newBuilder</code> which creates a builder for collections of the
 *    same kind.
 *  </p>
 *  <p>
 *    This trait implements <code>Traversable</code>'s <code>foreach</code>
 *    method by stepping through all elements. Subclasses of <code>Iterable</code>
 *    should re-implement <code>foreach</code> with something more efficient,
 *    if possible.
 *  </p>
 *  <p>
 *    This trait adds methods <code>iterator</code>, <code>sameElements</code>,
 *    <code>takeRight</code>, <code>dropRight</code> to the methods inherited
 *    from trait <a href="../Traversable.html" target="ContentFrame">
 *    <code>Traversable</code></a>.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableTemplate[+A, +This <: IterableTemplate[A, This] with Iterable[A]] extends TraversableTemplate[A, This] { self =>

  /** Creates a new iterator over all elements contained in this
   *  iterable object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A]

  @deprecated("use `iterator' instead")
  def elements = iterator

  /** Apply a function <code>f</code> to all elements of this
   *  traversable object.
   *
   *  @param  f   A function that is applied for its side-effect to every element.
   *              The result (of arbitrary type U) of function `f` is discarded.
   *
   *  @note This method underlies the implementation of most other bulk operations.
   *        Implementing `foreach` with `iterator` is often suboptimal.
   *        So `foreach` should be overridden in concrete collection classes if a more
   *        efficient implementation is available.
   */
  def foreach[U](f: A => U): Unit = this.iterator.foreach(f)

  /** Does this iterable contain no elements?
   */
  override def isEmpty: Boolean = !this.iterator.hasNext

  /** Combines the elements of this iterable together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the iterable is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  override def foldRight[B](z: B)(op: (A, B) => B): B =
    this.iterator.foldRight(z)(op)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
   *  @note Will not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered, or
   *        the operator is associative and commutative.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  override def reduceRight[B >: A](op: (A, B) => B): B =
    this.iterator.reduceRight(op)

  /** The iterable itself */
  override def toIterable: Iterable[A] = thisCollection

  /** The first element of this iterable.
   *
   *  @note  Might return different results for different runs, unless this iterable is ordered
   *  @throws Predef.NoSuchElementException if the iterable is empty.
   */
  override def head: A =
    if (isEmpty)
      throw new NoSuchElementException
    else
      this.iterator.next

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeRight(n: Int): This = {
    val b = newBuilder
    val lead = this.iterator drop n
    var go = false
    for (x <- this) {
      if (lead.hasNext) lead.next
      else go = true
      if (go) b += x
    }
    b.result
  }

  /** Returns the iterable wihtout its rightmost <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def dropRight(n: Int): This = {
    val b = newBuilder
    val lead = iterator drop n
    breakable {
      for (x <- this) {
        if (!lead.hasNext) break
        lead.next
        b += x
      }
    }
    b.result
  }


  /** Returns an iterable formed from this iterable and another iterable
   *  by combining corresponding elements in pairs.
   *  If one of the two iterables is longer than the other, its remaining elements are ignored.
   *  @param   that  The iterable providing the second half of each result pair
   */
  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: BuilderFactory[(A1, B), That, This]): That = {
    val b = bf(thisCollection)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  /** Returns a iterable formed from this iterable and the specified iterable
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param that     iterable <code>that</code> may have a different length
   *                  as the self iterable.
   *  @param thisElem element <code>thisElem</code> is used to fill up the
   *                  resulting iterable if the self iterable is shorter than
   *                  <code>that</code>
   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting iterable if <code>that</code> is shorter than
   *                  the self iterable
   *  @return         <code>Sequence((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   *
   */
  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: BuilderFactory[(A1, B), That, This]): That = {
    val b = bf(thisCollection)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  /** Zips this iterable with its indices (startiong from 0).
   */
  def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, This]): That = {
    val b = bf(thisCollection)
    var i = 0
    for (x <- this) {
      b += ((x, i))
      i +=1
    }
    b.result
  }

  /** Sort the iterable according to the comparison function
   *  <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> is smaller than
   *  <code>e2</code>.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted sequence as in the original.
   *
   *  @param lt the comparison function
   *  @return   an iterable sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sortWith((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   */
  def sortWith(lt: (A, A) => Boolean)(implicit m: ClassManifest[A @uncheckedVariance]): This = {
    // !!! can we supply a default argument to m: ClassManifest ?
    val arr = toArray
    java.util.Arrays.sort(arr, Ordering fromLessThan lt)

    val b = newBuilder
    for (x <- arr) b += x
    b.result
  }

  /** Checks if the other iterable object contains the same elements as this one.
   *
   *  @note will not terminate for infinite-sized iterables.
   *  @param that  the other iterable
   *  @return true, iff both iterables contain the same elements in the same order.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def sameElements[B >: A](that: Iterable[B]): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      if (these.next != those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  /** Returns a stream with all elements in this traversable object.
   */
  override def toStream: Stream[A] = iterator.toStream

  /** Creates a view of this iterable @see IterableView
   */
  override def view = new IterableView[A, This] {
    protected lazy val underlying = self.thisCollection
    override def iterator = self.iterator
  }

  /** A sub-iterable view  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `view` and `slice` is that `view` produces
   *         a view of the current iterable, whereas `slice` produces a new iterable.
   *
   *  @note  Might return different results for different runs, unless this iterable is ordered
   *  @note view(from, to)  is equivalent to view.slice(from, to)
   */
  override def view(from: Int, until: Int) = view.slice(from, until)

  @deprecated("use `head' instead") def first: A = head

  /** <code>None</code> if traversable is empty. */
  @deprecated("use `headOption' instead") def firstOption: Option[A] = headOption

  @deprecated("use `toSequence' instead") def toSeq: Sequence[A] = toSequence

  /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead") def projection = view
}
