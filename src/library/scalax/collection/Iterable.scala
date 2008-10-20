/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection

import scala.collection.mutable.{Buffer, ArrayBuffer, ListBuffer}
import util.control.Break._

/** Various utilities for instances of <a href="Iterable.html">Iterable</a>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object Iterable extends SeqFactory[Iterable] {

  type IterableOf[+C[+B] <: Iterable[B], A] = Iterable[A] { type CC[+B] = C[B] }

  /** The empty iterable */
  val empty = new Iterable[Nothing] {
    type CC[+B] = Iterable[B]
    def elements = Iterator.empty
    def newBuilder[B]: Builder[Iterable, B] = null // !!!
  }

  class OrderedIterableOps[A](seq: Iterable[A], cmp: Ordering[A]) {
    def min: A = {
      require(!seq.isEmpty, "min(<empty>)")
      var acc = seq.first
      for (x <- seq)
        if (cmp.lt(x, acc)) acc = x
      acc
    }
    def max: A = {
      require(!seq.isEmpty, "max(<empty>)")
      var acc = seq.first
      for (x <- seq)
        if (cmp.gt(x, acc)) acc = x
      acc
    }
  }

  class NumericIterableOps[A](seq: Iterable[A], num: Numeric[A]) {
    def sum: A = {
      var acc = num.zero
      for (x <- seq) acc = num.plus(acc, x)
      acc
    }
    def product: A = {
      var acc = num.one
      for (x <- seq) acc = num.times(acc, x)
      acc
    }
  }

  class PairIterableOps[C[+B] <: Iterable[B], A1, A2](self: C[(A1, A2)]) {
    def unzip: (C[A1], C[A2]) = {
      val as = self.newBuilder[A1].asInstanceOf[Builder[C, A1]]
      val bs = self.newBuilder[A2].asInstanceOf[Builder[C, A2]]
      for ((a, b) <- self) {
        as += a
        bs += b
      }
      (as.result, bs.result)
    }
  }

  class IterableIterableOps[C[+B] <: Iterable[B], A](self: C[Iterable[A]]) {
    def flatten: C[A] = {
      val b = self.newBuilder[A].asInstanceOf[Builder[C, A]]
      for (xs <- self)
        b ++= xs
      b.result
    }
  }

  implicit def orderedIterableWrapper[A](seq: Iterable[A])(implicit cmp: Ordering[A]) =
    new OrderedIterableOps(seq, cmp)
  implicit def numericIterableWrapper[A](seq: Iterable[A])(implicit num: Numeric[A]) =
    new NumericIterableOps(seq, num)
  implicit def pairIterableWrapper[C[+B] <: Iterable[B], A1, A2](seq: C[(A1, A2)]) =
    new PairIterableOps[C, A1, A2](seq)
  implicit def iterableIterableWrapper[C[+B] <: Iterable[B], A](seq: C[Iterable[A]]) =
    new IterableIterableOps[C, A](seq)

  /** The minimum element of a non-empty sequence of ordered elements
   *  @deprecated use seq.min instead
   */
  @deprecated def min[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("min(<empty>)")
    var min = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (x < min) min = x
    }
    min
  }

  /** The maximum element of a non-empty sequence of ordered elements
   *  @deprecated use seq.max instead
   */
  @deprecated def max[A <% Ordered[A]](seq: Iterable[A]): A = {
    val xs = seq.elements
    if (!xs.hasNext) throw new IllegalArgumentException("max(<empty>)")
    var max = xs.next
    while (xs.hasNext) {
      val x = xs.next
      if (max < x) max = x
    }
    max
  }

  /** A non-strict projection of an iterable.
   * @author Sean McDirmid
   * @author Martin Odersky
  abstract class View[+C[+B] <: Iterable[B], +A] extends Iterable[A] { self =>

    type CC[+B] <: View[C, A]

    def origin: IterableOf[C, _]
    def elements: Iterator[A]

    override val underlying: IterableOf[C, _] = origin.underlying

    private[this] var forced: Option[C[A]] = None

    def force: C[A] = forced match {
      case Some(c) => c
      case None =>
        val isDelay = elements eq origin.elements
        val result =
          if (isDelay) origin.force.asInstanceOf[C[A]]
          else {
            val b = newBuilder[A]
            for (x <- elements)
              b += x
            b.result
          }
        this.forced = Some(result)
        result
    }

    def newBuilder[B]: Builder[C, B] = underlying.newBuilder[B]

    /** The builds a new view object */
    protected def newView[B](elems: Iterator[B]): View[C, B] = new View[C, B] {
      val previous = self
      val elements = elems
    }

    /** Non-strict variant of @see IterableLike.++ */
    override def ++[B >: A](that: Iterator[B]): View[C, B] = newView(elements ++ that)

    /** Non-strict variant of @see IterableLike.++ */
    override def ++[B >: A](that: Iterable[B]): View[C, B] = newView(elements ++ that.elements)

    /** Non-strict variant of @see IterableLike.map */
    override def map[B](f: A => B): View[I, B] = newView(elements map f)

    /** Non-strict variant of @see IterableLike.flatMap */
    override def flatMap[B](f: A => Iterable[B]): View[C, B] = newView(elements flatMap (f(_).elements))

    /** Non-strict variant of @see IterableLike.filter */
    override def filter(p: A => Boolean): View[C, A] = newView(elements filter p)

    /** Non-strict variant of @see IterableLike.partition */
    override def partition(p: A => Boolean): (View[C, A], View[C, A]) = {
      val (li, ri) = elements partition p
      (newView(li), newView(ri))
    }

    /** Non-strict variant of @see IterableLike.take */
    override def take(n: Int): View[C, A] = newView(elements take n)

    /** Non-strict variant of @see IterableLike.drop */
    override def drop(n: Int): View[C, A] = newView(elements drop n)

    /** Non-strict variant of @see IterableLike.slice */
    override def slice(from: Int, until: Int): View[C, A] = newView(elements slice (from, until))

    /** Non-strict variant of @see IterableLike.takeWhile */
    override def takeWhile(p: A => Boolean): View[C, A] = newView(elements takeWhile p)

    /** Non-strict variant of @see IterableLike.dropWhile */
    override def dropWhile(p: A => Boolean): View[C, A] = newView(elements dropWhile p)

    /** Non-strict variant of @see IterableLike.zip */
    override def zip[B](other: Iterable[B]): C[(A, B)] = newView(elements zip other.elements)

    /** Non-strict variant of @see IterableLike.indices */
    override def indices: C[Int] = newView(elements.zipWithIndex map (_._2))

    /** Non-strict variant of @see IterableLike.zipWithIndex */
    override def zipWithIndex: C[(A, Int)] = newView(elements.zipWithIndex)

    /** Non-strict variant of @see IterableLike.zipAll */
    override def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): C[(A1, B1)] =
      newView(elements zipAll (that.elements, thisElem, thatElem))

    /** The projection resulting from the concatenation of this projection with the <code>rest</code> projection.
     *  @param rest   The projection that gets appended to this projection
     *  @deprecated   Use ++ instead
     */
    @deprecated def append[B >: A](rest : => Iterable[B]): View[C, B] = this ++ rest.elements

    override def stringPrefix = origin.stringPrefix+"D"
  }
   */
/*
  /** A non-strict projection of an iterable.
   * @author Sean McDirmid
   */
  @deprecated trait Projection[+A] extends Iterable[A] {
    override def projection = this
    /** convert to a copied strict collection */
    def force : Iterable[A] = toList.asInstanceOf[Iterable[A]] //!!!

    /** non-strict */
    override def filter(p : A => Boolean) : Projection[A] = new Projection[A] {
      def elements = Projection.this.elements.filter(p)
    }
    /** non-strict */
    override def map[B](f: A => B) : Projection[B] = new Projection[B] {
      def elements = Projection.this.elements.map(f)
    }
    /** non-strict */
    override def flatMap[B](f: A => Iterable[B]) : Projection[B] = new Projection[B] {
      def elements = Projection.this.elements.flatMap(a => f(a).elements)
    }
    /** non-strict */
    override def takeWhile(p: A => Boolean): Projection[A] = new Projection[A] {
      def elements = Projection.this.elements.takeWhile(p)
    }
    /** The projection resulting from the concatenation of this projection with the <code>rest</code> projection.
     *  @param rest   The projection that gets appended to this projection
     */
    def append[B >: A](rest : => Iterable[B]): Projection[B] = new Projection[B] {
      def elements = Projection.this.elements ++ rest.elements
    }
  }
*/
}

import Iterable._

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>Collection</code>.
 *        Only potentially unbounded collections should directly sub-class <code>Iterable</code>.
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
trait Iterable[+A] {

  /** The type of the underlying iterable object
   */
  type CC[+B] <: Iterable[B]

  /** This iterable seen as a CC-typed value */
  def thisCC: CC[A] = this.asInstanceOf[CC[A]]

  /** Creates a new iterator over all elements contained in this
   *  object.
   *
   *  @return the new iterator
   */
  def elements: Iterator[A]

  /** Create a new builder for this IterableType
   */
  def newBuilder[B]: Builder[CC, B]

  /** Creates a view of this iterable @see Iterable.View
  def view: Iterable.View[C, A] = new Iterable.View {
    val previous = self
    val elements = self.elements
  }
  */

  /** Is this collection empty? */
  def isEmpty: Boolean = !elements.hasNext

  /** returns true iff this collection has a bound size.
   *  Many APIs in this trait will not work on collections of
   *  unbound sizes.
   */
  def hasDefiniteSize = true

  /** Create a new sequence of type CC which contains all elements of this sequence
   *  followed by all elements of Iterable `that'
   */
  def ++[B >: A](that: Iterable[B]): CC[B] = {
    val b = newBuilder[B]
    b ++= this
    b ++= that
    b.result
  }

  /** Create a new sequence of type IterableType which contains all elements of this sequence
   *  followed by all elements of Iterator `that'
   */
  def ++[B >: A](that: Iterator[B]): CC[B] = {
    val b = newBuilder[B]
    b ++= this
    b ++= that
    b.result
  }

  /** Returns the sequence resulting from applying the given function
   *  <code>f</code> to each element of this sequence.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def map[B](f: A => B): CC[B] = {
    val b = newBuilder[B]
    for (x <- this) b += f(x)
    b.result
  }

  /** Applies the given function <code>f</code> to each element of
   *  this sequence, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def flatMap[B](f: A => Iterable[B]): CC[B] = {
    val b = newBuilder[B]
    for (x <- this) b ++= f(x)
    b.result
  }

  /** Returns all the elements of this sequence that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *  It is guaranteed that the receiver iterable
   *  itself is returned iff all its elements satisfy the predicate `p'.
   *  Hence the following equality is valid:
   *
   *  (xs filter p) eq xs  ==  xs forall p
   *
   *  @param p the predicate used to filter the list.
   *  @return the elements of this list satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): CC[A] = {
    val b = newBuilder[A]
    var allTrue = true
    for (x <- this)
      if (p(x)) b += x
      else allTrue = false
    if (allTrue) thisCC
    else b.result
  }

  /** Removes all elements of the iterable which satisfy the predicate
   *  <code>p</code>. This is like <code>filter</code> with the
   *  predicate inversed.
   *
   *  @param p the predicate to use to test elements
   *  @return  the list without all elements which satisfy <code>p</code>
   */
  def remove(p: A => Boolean): CC[A] = filter(!p(_))

  /** Partitions this iterable in two iterables according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterables: the iterable that satisfies the predicate
   *           <code>p</code> and the iterable that does not.
   *           The relative order of the elements in the resulting iterables
   *           is the same as in the original iterable.
   */
  def partition(p: A => Boolean): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  f   a function that is applied to every element.
   *  Note this function underlies the implementation of most other bulk operations.
   *  It should be overridden in concrete collectionc classes with efficient implementations.
   */
  def foreach(f: A => Unit): Unit = elements.foreach(f)

  /** The first element of this sequence.
   *
   *  @throws Predef.NoSuchAentException if the sequence is empty.
   */
  def first: A = if (isEmpty) throw new NoSuchElementException else elements.next

  /** Returns as an option the first element of this list or
   *  <code>None</code> if list is empty.
   */
  def firstOption: Option[A] = if (isEmpty) None else Some(first)

  /** Return an iterable consisting only over the first <code>n</code>
   *  elements of this iterable, or else the whole iterable, if it has less
   *  than <code>n</code> elements.
   *
   *  @param n the number of elements to take
   *  @return a possibly projected sequence
   */
  def take(n: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    breakable {
      for (x <- this) {
        b += x
        i += 1
        if (i == n) break
      }
    }
    b.result
  }

  /** Returns this collection without its <code>n</code> first elements
   *  If this collection has less than <code>n</code> elements, the empty
   *  collection is returned.
   *
   *  @param n the number of elements to drop
   *  @return  the new collection
   */
  def drop(n: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    for (x <- this) {
      if (i >= n) b += x
      i += 1
    }
    b.result
  }

  /** A sub-sequence starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the returned subsequence
   *  @param until  The index of the element following the returned subsequence
   *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
   *          or <code>length &lt; from + len<code>
   */
  def slice(from: Int, until: Int): CC[A] = {
    val b = newBuilder[A]
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from) b += x
        i += 1
        if (i == until) break
      }
    }
    b.result
  }

  /** A sub-sequence view  starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until  The index of the element following the slice
   *  @note  The difference between `subseq` and `slice` is that `slice` produces
   *         a view of the current sequence, whereas `subseq` produces a new sequence.
   * !!!
  def view(from: Int, until: Int) = subseq(from, until)
    // : Iterable.View[C, A] = view.slice(from, until)
   */

  /** An iterable consisting of all elements of this iterable except the last one.
   */
  def init: CC[A] = {
    var last: A = first
    val b = newBuilder[A]
    for (x <- this) {
      b += last
      last = x
    }
    b.result
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *
   *  @param n the number of elements to take
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

  /** Split the iterable at a given point and return the two parts thus
   *  created.
   *
   *  @param n the position at which to split
   *  @return  a pair of iterables composed of the first <code>n</code>
   *           elements, and the other elements.
   */
  def splitAt(n: Int): (CC[A], CC[A]) = {
    val l, r = newBuilder[A]
    var i = 0
    for (x <- this)
      (if (i < n) l else r) += x
    (l.result, r.result)
  }

  /** Returns the longest prefix of this sequence whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this sequence whose elements satisfy
   *           the predicate <code>p</code>.
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

  /** Returns the longest suffix of this sequence whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the sequence whose first element
   *           does not satisfy the predicate <code>p</code>.
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

 /** Returns a pair consisting of the longest prefix of the list whose
   *  elements all satisfy the given predicate, and the rest of the list.
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of the list whose
   *           elements all satisfy <code>p</code>, and the rest of the list.
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

  /** Return true iff the given predicate `p` yields true for all elements
   *  of this iterable.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  def forall(p: A => Boolean): Boolean = {
    var result = true
    breakable {
      for (x <- this)
        if (!p(x)) { result = false; break }
    }
    result
  }

  /** Return true iff there is an element in this iterable for which the
   *  given predicate `p` yields true.
   *
   *  @note May not terminate for infinite-sized collections.
   *  @param   p     the predicate
   */
  def exists(p: A => Boolean): Boolean = {
    var result = false
    breakable {
      for (x <- this)
        if (p(x)) { result = true; break }
    }
    result
  }

  /** Count the number of elements in the iterable which satisfy a predicate.
   *
   *  @param p the predicate for which to count
   *  @return  the number of elements satisfying the predicate <code>p</code>.
   */
  def count(p: A => Boolean): Int = {
    var cnt = 0
    for (x <- this) {
      if (p(x)) cnt += 1
    }
    cnt
  }

  /** Find and return the first element of the iterable object satisfying a
   *  predicate, if any.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param p the predicate
   *  @return an option containing the first element in the iterable object
   *  satisfying <code>p</code>, or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var result: Option[A] = None
    breakable {
      for (x <- this)
        if (p(x)) { result = Some(x); break }
    }
    result
  }

  /** Returns index of the first element satisying a predicate, or -1, if none exists.
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
  def indexOf[B >: A](elem: B, from: Int): Int = {
    var result = -1
    var i = from
    breakable {
      for (x <- this) {
        if (x == elem) { result = i; break }
        else i += 1
      }
    }
    result
  }

  /** Combines the elements of this iterable object together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this)
      result = op(result, x)
    result
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = elements.foldRight(z)(op)

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of list and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def /: [B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>
   *  @note Will not terminate for infinite-sized collections.
   */
  def :\ [B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from left to right
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterable object has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterable object is empty.
   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    var result: B = elements.next
    var first = true
    for (x <- this)
      if (first) first = false
      else result = op(result, x)
    result
  }

  /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left
   *  @note Will not terminate for infinite-sized collections.
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterable object has elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   *
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B =
    elements.reduceRight(op)

  /** Returns an iterable formed from this iterable and the specified list
   *  `other` by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two iterables is longer than the other, its remaining elements are ignored.
   */
  def zip[B](that: Iterable[B]): CC[(A, B)] = {
    val these = this.elements
    val those = that.elements
    val b = this.newBuilder[(A, B)]
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
b   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting iterable if <code>that</code> is shorter than
   *                  the self iterable
   *  @return         <code>Iterable((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   */
  def zipAll[B, A1 >: A, B1 >: B](that: Iterable[B], thisElem: A1, thatElem: B1): CC[(A1, B1)] = {
    val these = this.elements
    val those = that.elements
    val b = newBuilder[(A1, B1)]
    while (these.hasNext && those.hasNext)
      b += ((these.next, those.next))
    while (these.hasNext)
      b += ((these.next, thatElem))
    while (those.hasNext)
      b += ((thisElem, those.next))
    b.result
  }

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to
   *  `s zip s.indices`, but is usually more efficient.
   */
  def zipWithIndex: CC[(A, Int)] = {
    val b = newBuilder[(A, Int)]
    var i = 0
    for (x <- this) {
      b += (x, i)
      i +=1
    }
    b.result
  }

  /** Copy all elements to a given buffer
   *  @note Will not terminate for infinite-sized collections.
   *  @param  dest   The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    for (x <- this) dest += x
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this sequence starting at position `start`.
   *  Copying will stop oce either the end of the current iterable is reached or
   *  `len` elements have been copied.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = (start + len) min xs.length
    for (x <- this) {
      if (i < end) {
        xs(i) = x
        i += 1
      }
    }
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>
   *  until either the end of the current iterable or the end of array `xs` is reached.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)

  /** Converts this collection to a fresh Array with <code>size</code> elements.
   *  @note  Will not terminate for infinite-sized collections.
   */
  def toArray[B >: A]: Array[B] = {
    var size = 0
    for (x <- this) size += 1
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }

  /** Checks if the other iterable object contains the same elements.
   *
   *  @note will not terminate for infinite-sized collections.
   *  @param that  the other iterable object
   *  @return true, iff both iterable objects contain the same elements.
   */
  def sameElements[B >: A](that: Iterable[B]): Boolean = {
    val ita = this.elements
    val itb = that.elements
    var res = true
    while (res && ita.hasNext && itb.hasNext) {
      res = (ita.next == itb.next)
    }
    !ita.hasNext && !itb.hasNext && res
  }

  /**
   *  Create a fresh list with all the elements of this iterable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toList: List[A] = {
    val b = new ListBuffer[A]()
    b ++= this.asInstanceOf[scala.Iterable[A]] // !!!
    b.toList
  }

  /**
   *  Returns a sequence containing all of the elements in this iterable object.
   *  @note Will not terminate for infinite-sized collections.
   */
  def toSeq: Seq[A] = toList

  /**
   *  Create a stream which contains all the elements of this iterable object.
   *  @note consider using <code>projection</code> for lazy behavior.
   */
  def toStream: Stream[A] = elements.toStream

  /** Sort the iterable according to the comparison function
   *  <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>,
   *  which should be true iff <code>e1</code> is smaller than
   *  <code>e2</code>.
   *  The sort is stable. That is elements that are equal wrt `lt` appear in the
   *  same order in the sorted iterable as in the original.
   *
   *  @param lt the comparison function
   *  @return   a list sorted according to the comparison function
   *            <code>&lt;(e1: a, e2: a) =&gt; Boolean</code>.
   *  @ex <pre>
   *    List("Steve", "Tom", "John", "Bob")
   *      .sort((e1, e2) => (e1 compareTo e2) &lt; 0) =
   *    List("Bob", "John", "Steve", "Tom")</pre>
   *  !!!
  def sortWith(lt : (A,A) => Boolean): CC[A] = {
    val arr = toArray
    Array.sortWith(arr, lt)
    val b = newBuilder[A]
    for (x <- arr) b += x
    b.result
  }
  */

  /** Returns a string representation of this iterable object. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *
   *  @ex  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *  @note Will not terminate for infinite-sized collections.
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this iterable object.
   */
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @note Will not terminate for infinite-sized collections.
   *  @param sep separator string.
   *  @return a string representation of this iterable object.
   */
  def mkString(sep: String): String =
    addString(new StringBuilder(), sep).toString

  /** Converts a collection into a flat <code>String</code> by each element's toString method.
   *  @note Will not terminate for infinite-sized collections.
   */
  def mkString =
    addString(new StringBuilder()).toString

  /** Write all elements of this iterable into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b append start
    var first = true
    for (x <- this) {
      if (first) first = false
      else b append sep
      b append x
    }
    b append end
  }

  /** Write all elements of this string into given string builder.
   *  The string representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = {
    var first = true
    for (x <- this) {
      if (first) first = false
      else b append sep
      b append x
    }
    b
  }

  /** Write all elements of this string into given string builder without using
   *  any separator between consecutive elements.
   *  @note Will not terminate for infinite-sized collections.
   */
  def addString(b: StringBuilder): StringBuilder = {
    for (x <- this) {
      b append x
    }
    b
  }

  /**
   * returns a projection that can be used to call non-strict <code>filter</code>,
   * <code>map</code>, and <code>flatMap</code> methods that build projections
   * of the collection.
  def projection : Iterable.Projection[A] = new Iterable.Projection[A] {
    def elements = Iterable.this.elements
    override def force = Iterable.this
  }
   */

  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  protected def stringPrefix : String = {
    var string = this.getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }
}



