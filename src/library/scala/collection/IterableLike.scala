/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableLike.scala 18602 2009-08-29 00:54:16Z extempore $

package scala.collection
import generic._
import annotation.unchecked.uncheckedVariance

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
 *    This trait implements <code>Iterable</code>'s <code>foreach</code>
 *    method by stepping through all elements. Subclasses of <code>Iterable</code>
 *    should re-implement <code>foreach</code> with something more efficient,
 *    if possible.
 *  </p>
 *  <p>
 *    This trait adds methods <code>iterator</code>, <code>sameElements</code>,
 *    <code>takeRight</code>, <code>dropRight</code> to the methods inherited
 *    from trait <a href="../Iterable.html" target="ContentFrame">
 *    <code>Iterable</code></a>.
 *  </p>
 *
 *  @note  This trait replaces every method that uses breaks in the original by an iterator version.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableLike[+A, +Repr] extends Equals with TraversableLike[A, Repr] {
self =>

  override protected[this] def thisCollection: Iterable[A] = this.asInstanceOf[Iterable[A]]
  override protected[this] def toCollection(repr: Repr): Iterable[A] = repr.asInstanceOf[Iterable[A]]

  /** Creates a new iterator over all elements contained in this
   *  iterable object.
   *
   *  @return the new iterator
   */
  def iterator: Iterator[A]

  @deprecated("use `iterator' instead")
  def elements = iterator

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   A function that is applied for its side-effect to every element.
   *              The result (of arbitrary type U) of function `f` is discarded.
   *
   *  @note This method underlies the implementation of most other bulk operations.
   *        Implementing `foreach` with `iterator` is often suboptimal.
   *        So `foreach` should be overridden in concrete collection classes if a more
   *        efficient implementation is available.
   */
  def foreach[U](f: A => U): Unit = iterator.foreach(f)


  /** Return true iff the given predicate `p` yields true for all elements
   *  of this iterable.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  override def forall(p: A => Boolean): Boolean = iterator.forall(p)

  /** Return true iff there is an element in this iterable for which the
   *  given predicate `p` yields true.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  override def exists(p: A => Boolean): Boolean = iterator.exists(p)

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @note Might return different results for different runs, unless this iterable is ordered.
   *  @param p the predicate
   *  @return an option containing the first element in the iterable object
   *  satisfying <code>p</code>, or <code>None</code> if none exists.
   */
  override def find(p: A => Boolean): Option[A] = iterator.find(p)

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

  /** Return an iterable consisting only of the first <code>n</code>
   *  elements of this iterable, or else the whole iterable, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  override def take(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    val it = iterator
    while (i < n && it.hasNext) {
      b += it.next
      i += 1
    }
    b.result
  }

  /** A sub-iterable starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @note c.slice(from, to)  is equivalent to (but possibly more efficient than)
   *  c.drop(from).take(to - from)
   *
   *  @param from   The index of the first element of the returned subsequence
   *  @param until  The index of the element following the returned subsequence
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  override def slice(from: Int, until: Int): Repr = {
    val b = newBuilder
    var i = from
    val it = iterator drop from
    while (i < until && it.hasNext) {
      b += it.next
      i += 1
    }
    b.result
  }

  /** Returns the longest prefix of this iterable whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  override def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    val it = iterator
    while (it.hasNext) {
      val x = it.next
      if (!p(x)) return b.result
      b += x
    }
    b.result
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *
   *  @param n the number of elements to take
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  def takeRight(n: Int): Repr = {
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
  def dropRight(n: Int): Repr = {
    val b = newBuilder
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next
      lead.next
    }
    b.result
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this iterable starting at position `start`.
   *  Copying will stop once either the end of the current iterable is reached or
   *  `len` elements have been copied or the end of the array is reached.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    val it = iterator
    while (i < end && it.hasNext) {
      xs(i) = it.next
      i += 1
    }
  }

  /** Returns an iterable formed from this iterable and another iterable
   *  by combining corresponding elements in pairs.
   *  If one of the two iterables is longer than the other, its remaining elements are ignored.
   *  @param   that  The iterable providing the second half of each result pair
   */
  def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: BuilderFactory[(A1, B), That, Repr]): That = {
    val b = bf(repr)
    val these = this.iterator
    val those = that.iterator
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    b.result
  }

  /** Returns an iterable formed from this iterable and the specified iterable
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
  def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: BuilderFactory[(A1, B), That, Repr]): That = {
    val b = bf(repr)
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
  def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, Repr]): That = {
    val b = bf(repr)
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
  def sortWith(lt: (A, A) => Boolean)(implicit m: ClassManifest[A @uncheckedVariance]): Repr = {
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

  /** Returns a stream with all elements in this iterable object.
   */
  override def toStream: Stream[A] = iterator.toStream

  /** Method called from equality methods, so that user-defined subclasses can
   *  refuse to be equal to other collections of the same kind.
   */
  override def canEqual(that: Any) = true

  /** Creates a view of this iterable @see IterableView
   */
  override def view = new IterableView[A, Repr] {
    protected lazy val underlying = self.repr
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

  /** <code>None</code> if iterable is empty. */
  @deprecated("use `headOption' instead") def firstOption: Option[A] = headOption

  @deprecated("use `toSequence' instead") def toSeq: Sequence[A] = toSequence

  /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
   */
  @deprecated("use `view' instead")
  def projection = view
}
