/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterator.scala 15939 2008-08-26 14:33:17Z stepancheg $


package scala.collection

import mutable.{Buffer, ArrayBuffer, ListBuffer}
import annotation.tailrec
// import immutable.{List, Nil, ::, Stream}

/** The <code>Iterator</code> object provides various functions for
 *  creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 2.8
 */
object Iterator {

  val empty = new Iterator[Nothing] {
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  /** An iterator with a single element.
   *  @param elem the element
   *  @note  Equivalent, but more efficient than Iterator(elem)
   */
  def single[A](elem: A) = new Iterator[A] {
    private var hasnext = true
    def hasNext: Boolean = hasnext
    def next(): A =
      if (hasnext) { hasnext = false; elem }
      else empty.next()
  }

  /** Creates an iterator with given elements
   *  @param elems  The elements returned one-by-one from the iterator
   */
  def apply[A](elems: A*): Iterator[A] = elems.iterator

  /** Concatenates the given argument iterators into a single iterator.
   *
   *  @param its the argument iterators that are to be concatenated
   *  @return the concatenation of all the argument iterators
   */
  @deprecated("use <code>++</code>")
  def concat[A](xss: Iterator[A]*): Iterator[A] = xss.iterator.flatten

  /** An iterator that returns the results of some element computation a number of times.
   *  @param   len  The number of elements returned
   *  @param   elem The element computation determinining each result
   */
  def fill[A](len: Int)(elem: => A) = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = i < len
    def next(): A =
      if (hasNext) { i += 1; elem }
      else empty.next()
  }

  /** An iterator that returns values of a given function over a range of integer values starting from 0.
   *  @param end   The argument up to which values are tabulated.
   *  @param f     The function computing the results
   *  @return  An iterator with values `f(0) ... f(end-1)`
   */
  def tabulate[A](end: Int)(f: Int => A) = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = i < end
    def next(): A =
      if (hasNext) { val result = f(i); i += 1; result }
      else empty.next()
  }

 /** An iterator returning successive values in some integer interval.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @return      the iterator with values in range `start, start + 1, ..., end - 1`
   */
  def range(start: Int, end: Int): Iterator[Int] = range(start, end, 1)

  /** An iterator returning equally spaced values in some integer interval.

   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @param step  the increment value of the iterator (must be positive or negative)
   *  @return      the iterator with values in `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int) = new Iterator[Int] {
    if (step == 0) throw new IllegalArgumentException("zero step")
    private var i = start
    def hasNext: Boolean = (step <= 0 || i < end) && (step >= 0 || i > end)
    def next(): Int =
      if (hasNext) { val result = i; i += step; result }
      else empty.next()
  }

  /** An infinite iterator that repeatedly applies a given function to the previous result.
   *
   *  @param start the start value of the iterator
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[T](start: T)(f: T => T): Iterator[T] = new Iterator[T] {
    private[this] var acc = start
    def hasNext: Boolean = true
    def next(): T = { val res = acc ; acc = f(acc) ; res }
  }

  /** An infinite-length iterator which returns successive values from some start value.

   *  @param start the start value of the iterator
   *  @return      the iterator returning the infinite sequence of values `start, start + 1, start + 2, ...`
   */
  def from(start: Int): Iterator[Int] = from(start, 1)

  /** An infinite-length iterator returning values equally spaced apart.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment between successive values
   *  @return      the iterator returning the infinite sequence of values `start, start + 1 * step, start + 2 * step, ...`
   */
  def from(start: Int, step: Int): Iterator[Int] = new Iterator[Int] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Int = { val result = i; i += step; result }
  }

  /**
   * Create an infinite iterator based on the given expression
   * (which is recomputed for every element)
   *
   * @param elem the element composing the resulting iterator
   * @return the iterator containing an infinite number of elem
   */
  def continually[A](elem: => A): Iterator[A] = new Iterator[A] {
    def hasNext = true
    def next = elem
  }

  /** A wrapper class for the `flatten `method that is added to class Iterator with implicit conversion @see iteratorIteratorWrapper.
   */
  class IteratorIteratorOps[A](its: Iterator[Iterator[A]]) {
    /** If `its` is an iterator of iterators, `its.flatten` gives the iterator that is the concatenation of all iterators in `its`.
     */
    def flatten: Iterator[A] = new Iterator[A] {
      private var it: Iterator[A] = empty
      def hasNext: Boolean = it.hasNext || its.hasNext && { it = its.next(); hasNext }
      def next(): A = if (hasNext) it.next() else empty.next()
    }
  }

  /** An implicit conversion which adds the `flatten` method to class `Iterator` */
  implicit def iteratorIteratorWrapper[A](its: Iterator[Iterator[A]]): IteratorIteratorOps[A] =
    new IteratorIteratorOps[A](its)

  @deprecated("use `xs.iterator' or `Iterator(xs)' instead")
  def fromValues[a](xs: a*) = xs.iterator

  /**
   *  @param xs the array of elements
   *  @see also: Vector.iterator and slice
   */
  @deprecated("use `xs.iterator' instead")
  def fromArray[a](xs: Array[a]): Iterator[a] =
    fromArray(xs, 0, xs.length)

  /**
   *  @param xs     the array of elements
   *  @param start  the start index
   *  @param length  the length
   *  @see also: Vector.iterator and slice
   */
  @deprecated("use `xs.slice(start, start + length).iterator' instead")
  def fromArray[a](xs: Array[a], start: Int, length: Int): Iterator[a] =
    xs.slice(start, start + length).iterator

  /**
   *  @param str the given string
   *  @return    the iterator on <code>str</code>
   */
  @deprecated("replaced by <code>str.iterator</code>")
  def fromString(str: String): Iterator[Char] = str.iterator

  /**
   *  @param n the product arity
   *  @return  the iterator on <code>Product&lt;n&gt;</code>.
   */
  @deprecated("use product.productIterator instead")
  def fromProduct(n: Product): Iterator[Any] = new Iterator[Any] {
    private var c: Int = 0
    private val cmax = n.productArity
    def hasNext = c < cmax
    def next() = { val a = n productElement c; c += 1; a }
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = start</code>
   *  and elements are in the range between <code>start</code> (inclusive)
   *  and <code>end</code> (exclusive)
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator
   *  @param step  the increment function of the iterator, must be monotonically increasing or decreasing
   *  @return      the iterator with values in range <code>[start;end)</code>.
   */
  @deprecated("use Iterator.iterate(start, end - start)(step) instead")
  def range(start: Int, end: Int, step: Int => Int) = new Iterator[Int] {
    private val up = step(start) > start
    private val down = step(start) < start
    private var i = start
    def hasNext: Boolean = (!up || i < end) && (!down || i > end)
    def next(): Int =
      if (hasNext) { val j = i; i = step(i); j }
      else empty.next()
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = step(e<sub>n</sub>)</code>
   *  where <code>e<sub>0</sub> = start</code>.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment function of the iterator
   *  @return      the iterator starting at value <code>start</code>.
   */
  @deprecated("use iterate(start)(step) instead")
  def from(start: Int, step: Int => Int): Iterator[Int] = new Iterator[Int] {
    private var i = start
    override def hasNext: Boolean = true
    def next(): Int = { val j = i; i = step(i); j }
  }

  /** Create an iterator that is the concantenation of all iterators
   *  returned by a given iterator of iterators.
   *   @param its   The iterator which returns on each call to next
   *                a new iterator whose elements are to be concatenated to the result.
   */
  @deprecated("use its.flatten instead")
  def flatten[T](its: Iterator[Iterator[T]]): Iterator[T] = new Iterator[T] {
    private var cur = its.next
    def hasNext: Boolean = {
      while (!cur.hasNext && its.hasNext) cur = its.next
      cur.hasNext
    }
    def next(): T =
      (if (hasNext) cur else empty).next()
  }
}

import Iterator.empty

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a <code>hasNext</code> method for checking
 *  if there is a next element available, and a <code>next</code> method
 *  which returns the next element and discards it from the iterator.
 *
 *  @author  Martin Odersky, Matthias Zenger
 *  @version 2.8
 */
trait Iterator[+A] { self =>

  /** Does this iterator provide another element?
   */
  def hasNext: Boolean

  /** Returns the next element of this iterator.
   */
  def next(): A

  /** Returns a new iterator that iterates only over the first <code>n</code>
   *  elements of this iterator, or the length of the iterator, whichever is smaller.
   *
   *  @param n the number of elements to take
   *  @return  the new iterator
   */
  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var remaining = n
    def hasNext = remaining > 0 && self.hasNext
    def next(): A =
      if (hasNext) { remaining -= 1; self.next }
      else empty.next()
  }

  /** Advances this iterator past the first <code>n</code> elements,
   *  or the length of the iterator, whichever is smaller.
   *  @param n the number of elements to drop
   *  @return  the new iterator
   */
  def drop(n: Int): Iterator[A] = {
    @tailrec
    def loop(left: Int): Iterator[A] =
      if (left > 0 && hasNext) { next; loop(left - 1) }
      else this

    loop(n)
  }

  /** Advances this iterator past the first `from` elements using `drop`,
   *  and then takes `until - from` elements, using `take`.
   *
   *  @param from   The index of the first element of the slice
   *  @param until    The index of the element following the slice
   */
  def slice(from: Int, until: Int): Iterator[A] = drop(from).take(until - from)

  /** Returns a new iterator that maps all elements of this iterator
   *  to new elements using function <code>f</code>.
   */
  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   */
  def ++[B >: A](that: => Iterator[B]) = new Iterator[B] {
    // optimize a little bit to prevent n log n behavior.
    var cur : Iterator[B] = self
    def hasNext = cur.hasNext || (cur eq self) && { cur = that; hasNext }
    def next() = { hasNext; cur.next() }
  }

  /** Applies the given function <code>f</code> to each element of
   *  this iterator, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  an iterator over <code>f(a<sub>0</sub>), ... ,
   *           f(a<sub>n</sub>)</code> if this iterator yields the
   *           elements <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new Iterator[B] {
    private var cur: Iterator[B] = empty
    def hasNext: Boolean =
      cur.hasNext || self.hasNext && { cur = f(self.next); hasNext }
    def next(): B = (if (hasNext) cur else empty).next()
  }

  /** Returns an iterator over all the elements of this iterator that
   *  satisfy the predicate <code>p</code>. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to filter the iterator.
   *  @return  the elements of this iterator satisfying <code>p</code>.
   */
  def filter(p: A => Boolean): Iterator[A] = {
    val self = buffered
    new Iterator[A] {
      private def skip() = while (self.hasNext && !p(self.head)) self.next()
      def hasNext = { skip(); self.hasNext }
      def next() = { skip(); self.next() }
    }
  }

  /** Returns an iterator over the longest prefix of this iterator such that
   *  all elements of the result satisfy the predicate <code>p</code>.
   *  The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the iterator.
   *  @return  the longest prefix of this iterator satisfying <code>p</code>.
   */
  def takeWhile(p: A => Boolean): Iterator[A] = {
    val self = buffered
    new Iterator[A] {
      def hasNext = { self.hasNext && p(self.head) }
      def next() = (if (hasNext) self else empty).next()
    }
  }

  /** Partitions this iterator in two iterators according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterators: the iterator that satisfies the predicate
   *           <code>p</code> and the iterator that does not.
   *           The relative order of the elements in the resulting iterators
   *           is the same as in the original iterator.
   */
  def partition(p: A => Boolean): (Iterator[A], Iterator[A]) = {
    val self = buffered
    class PartitionIterator(p: A => Boolean) extends Iterator[A] {
      var other: PartitionIterator = _
      val lookahead = new scala.collection.mutable.Queue[A]
      def skip() =
        while (self.hasNext && !p(self.head)) {
          other.lookahead += self.next
        }
      def hasNext = !lookahead.isEmpty || { skip(); self.hasNext }
      def next() = if (!lookahead.isEmpty) lookahead.dequeue()
                   else { skip(); self.next() }
    }
    val l = new PartitionIterator(p)
    val r = new PartitionIterator(!p(_))
    l.other = r
    r.other = l
    (l, r)
  }

  /** Skips longest sequence of elements of this iterator which satisfy given
   *  predicate <code>p</code>, and returns an iterator of the remaining elements.
   *
   *  @param p the predicate used to skip elements.
   *  @return  an iterator consisting of the remaining elements
   */
  def dropWhile(p: A => Boolean): Iterator[A] = {
    val self = buffered
    new Iterator[A] {
      var dropped = false
      private def skip() =
        if (!dropped) {
          while (self.hasNext && p(self.head)) self.next()
          dropped = true
        }
      def hasNext = { skip(); self.hasNext }
      def next() = { skip(); self.next() }
    }
  }

  /** Return an iterator formed from this iterator and the specified iterator
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two iterators is longer than the other, its remaining elements are ignored.
   *
   */
  def zip[B](that: Iterator[B]) = new Iterator[(A, B)] {
    def hasNext = self.hasNext && that.hasNext
    def next = (self.next, that.next)
  }

  /** Return a new iterator with a length equal or longer to `len`.
   *  If the current iterator returns fewer than `len` elements
   *  return `elem` until the required length `len` is reached.
   */
  def padTo[A1 >: A](len: Int, elem: A1) = new Iterator[A1] {
    private var count = 0
    def hasNext = self.hasNext || count < len
    def next = {
      count += 1
      if (self.hasNext) self.next
      else if (count <= len) elem
      else empty.next
    }
  }

  /** Return an iterator that pairs each element of this iterator
   *  with its index, counting from 0.
   *
   */
  def zipWithIndex = new Iterator[(A, Int)] {
    var idx = 0
    def hasNext = self.hasNext
    def next = {
      val ret = (self.next, idx)
      idx += 1
      ret
    }
  }

  /** Returns an iterator formed from this iterator and the specified iterator
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *
   *  @param that     iterator <code>that</code> may have a different length
   *                  as the self iterator.
   *  @param thisElem element <code>thisElem</code> is used to fill up the
   *                  resulting iterator if the self iterator is shorter than
   *                  <code>that</code>
   *  @param thatElem element <code>thatElem</code> is used to fill up the
   *                  resulting iterator if <code>that</code> is shorter than
   *                  the self iterator
   *  @return         <code>Iterator((a<sub>0</sub>,b<sub>0</sub>), ...,
   *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
   *                  ..., {elem,b<sub>m</sub>})</code>
   *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
   *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
   *                  invoked where <code>m &gt; n</code>.
   */
  def zipAll[B, A1 >: A, B1 >: B](that: Iterator[B], thisElem: A1, thatElem: B1) = new Iterator[(A1, B1)] {
    def hasNext = self.hasNext || that.hasNext
    def next(): (A1, B1) =
      if (self.hasNext) {
        if (that.hasNext) (self.next(), that.next())
        else (self.next(), thatElem)
      } else {
        if (that.hasNext) (thisElem, that.next())
        else empty.next()
      }
  }

  /** Execute a function <code>f</code> for all elements of this
   *  iterator.
   *
   *  @param  f   a function that is applied to every element.
   */
  def foreach[U](f: A =>  U) { while (hasNext) f(next()) }

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return <code>true</code> iff the predicate yields
   *  <code>true</code> for all elements.
   *
   *  @param p the predicate
   *  @return  <code>true</code> iff the predicate yields <code>true</code>
   *           for all elements.
   */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) res = p(next())
    res
  }

  /** Apply a predicate <code>p</code> to all elements of this
   *  iterable object and return true iff there is at least one
   *  element for which <code>p</code> yields <code>true</code>.
   *
   *  @param p the predicate
   *  @return  <code>true</code> iff the predicate yields <code>true</code>
   *           for at least one element.
   */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) res = p(next())
    res
  }

 /** Tests if the given value <code>elem</code> is a member of this iterator.
   *
   *  @param elem element whose membership has to be tested.
   */
  def contains(elem: Any): Boolean = exists(_ == elem)

  /** Find and return the first value returned by the iterator satisfying a
   *  predicate, if any.
   *
   *  @param p the predicate
   *  @return  the first element in the iterable object satisfying
   *           <code>p</code>, or <code>None</code> if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var res: Option[A] = None
    while (res.isEmpty && hasNext) {
      val e = next()
      if (p(e)) res = Some(e)
    }
    res
  }

  /** Returns index of the first element satisfying a predicate, or -1.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @return   the index of the first element satisfying <code>p</code>,
   *           or -1 if such an element does not exist
   */
  def indexWhere(p: A => Boolean): Int = {
    var i = 0
    var found = false
    while (!found && hasNext) {
      if (p(next())) {
        found = true
      } else {
        i += 1
      }
    }
    if (found) i else -1
  }

  /** Returns the index of the first occurence of the specified
   *  object in this iterable object.
   *
   *  @note may not terminate for infinite-sized collections.
   *  @param  elem  element to search for.
   *  @return the index in this sequence of the first occurence of the
   *          specified element, or -1 if the sequence does not contain
   *          this element.
   */
  def indexOf[B >: A](elem: B): Int = {
    var i = 0
    var found = false
    while (!found && hasNext) {
      if (next() == elem) {
        found = true
      } else {
        i += 1
      }
    }
    if (found) i else -1
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>op(... (op(op(z,a<sub>0</sub>),a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the iterator yields elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var acc = z
    while (hasNext) acc = op(acc, next())
    acc
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n</sub> op z)...)</code>
   *          if the iterator yields elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B =
    if (hasNext) op(next(), foldRight(z)(op)) else z

  /** Similar to <code>foldLeft</code> but can be used as
   *  an operator with the order of iterator and zero arguments reversed.
   *  That is, <code>z /: xs</code> is the same as <code>xs foldLeft z</code>.
   *
   *  @param z the left argument of the first application of <code>op</code>
   *           (evaluation occurs from left to right).
   *  @param op the applied operator.
   *  @return  the result value
   *  @see     <code><a href="#foldLeft">foldLeft</a></code>.
   */
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** An alias for <code>foldRight</code>.
   *  That is, <code>xs :\ z</code> is the same as <code>xs foldRight z</code>.
   *
   *  @param z the right argument of the first application of <code>op</code>
   *           (evaluation occurs from right to left).
   *  @param op the applied operator.
   *  @return  the result value.
   *  @see     <code><a href="#foldRight">foldRight</a></code>.
   */
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the iterator yields elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (hasNext) foldLeft[B](next())(op)
    else throw new UnsupportedOperationException("empty.reduceLeft")
  }

  /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from right to left
   *  @param op  The operator to apply
   *
   *  @return <code>a<sub>0</sub> op (... op (a<sub>n-1</sub> op a<sub>n</sub>)...)</code>
   *          if the iterator yields elements <code>a<sub>0</sub>, a<sub>1</sub>, ...,
   *          a<sub>n</sub></code>.

   *  @throws Predef.UnsupportedOperationException if the iterator is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (hasNext) foldRight[B](next())(op)
    else throw new UnsupportedOperationException("empty.reduceRight")
  }

 /** Combines the elements of this iterator together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return  If the iterable is non-empty, the result of the operations as an Option, otherwise None.
   */
  def reduceLeftOpt[B >: A](op: (B, A) => B): Option[B] = {
    if (!hasNext) None else Some(reduceLeft(op))
  }

 /** Combines the elements of this iterable object together using the binary
   *  operator <code>op</code>, from right to left.
   *  @param op  The operator to apply
   *  @return  If the iterable is non-empty, the result of the operations as an Option, otherwise None.
   */
  def reduceRightOpt[B >: A](op: (A, B) => B): Option[B] = {
    if (!hasNext) None else Some(reduceRight(op))
  }

  /** Returns a buffered iterator from this iterator.
   */
  def buffered = new BufferedIterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false

    def head: A = {
      if (!hdDefined) {
        hd = next()
        hdDefined = true
      }
      hd
    }

    def hasNext =
      hdDefined || self.hasNext

    def next =
      if (hdDefined) {
        hdDefined = false
        hd
      } else self.next
  }

  /** Since I cannot reliably get take(n) to influence the original
   *  iterator (it seems to depend on some ordering issue I don't
   *  understand) this method takes the way one might expect, leaving
   *  the original iterator with 'size' fewer elements.
   */
  private def takeDestructively(size: Int): Sequence[A] = {
    val buf = new ArrayBuffer[A]
    var i = 0
    while (self.hasNext && i < size) {
      buf += self.next
      i += 1
    }
    buf
  }

  /** Returns an iterator which groups this iterator into fixed size
   *  blocks, possibly except for the final block.  For instance:
   *    <code>(1 to 8).iterator grouped 3</code>
   *  will return an iterator equivalent to
   *    Iterator(Seq(1,2,3), Seq(4,5,6), Seq(7,8))
   */
  def grouped(size: Int): Iterator[Sequence[A]] = new Iterator[Sequence[A]] {
    def hasNext = self.hasNext
    def next = self takeDestructively size toList
  }
  /** Returns an iterator which presents a "sliding window" of the
   *  given size across this iterator.  For instance:
   *    <code>(1 to 5).iterator sliding 3</code>
   *  will return an iterator equivalent to
   *    Iterator(Seq(1,2,3), Seq(2,3,4), Seq(3,4,5))
   *
   *  The optional 'step' parameter if given advances the window
   *  by the given number of positions.
   *
   *  Note: if your parameters don't perfectly "fit" the sequence,
   *  the last result may be of a size less than windowSize, and the
   *  last step may be less than the given step, or both.  What is
   *  guaranteed is that each element of the original iterator will
   *  appear in at least one sequence in the sliding iterator, UNLESS
   *  the step size is larger than the windowSize.
   */
  def sliding(windowSize: Int, step: Int = 1): Iterator[Sequence[A]] = {
    require(windowSize >= 1 && step >= 1)

    val buf = takeDestructively(windowSize)

    if (!self.hasNext) Iterator single buf.toList
    else new Iterator[Sequence[A]] {
      private[this] var filled = true
      private[this] var buffer = ArrayBuffer(buf: _*)
      def fill() = {
        val xs = self takeDestructively step
        val len = xs.length min buf.size

        (len > 0) && {
          buffer trimStart len
          buffer ++= (xs takeRight len)
          filled = true
          true
        }
      }

      def hasNext = filled || fill()
      def next = {
        if (!filled)
          fill()

        filled = false
        buffer.toList
      }
    }
  }

  /** Returns the number of elements in this iterator.
   *  @note The iterator is at its end after this method returns.
   */
  def length: Int = {
    var i = 0
    while (hasNext) {
      next(); i += 1
    }
    i
  }

  /** Creates two new iterators that both iterate over the same elements
   *  as this iterator (in the same order).
   *
   *  @return a pair of iterators
   */
  def duplicate: (Iterator[A], Iterator[A]) = {
    val gap = new scala.collection.mutable.Queue[A]
    var ahead: Iterator[A] = null
    class Partner extends Iterator[A] {
      def hasNext: Boolean = self.synchronized {
        (this ne ahead) && !gap.isEmpty || self.hasNext
      }
      def next(): A = self.synchronized {
        if (gap.isEmpty) ahead = this
        if (this eq ahead) {
          val e = self.next()
          gap enqueue e
          e
        } else gap.dequeue
      }
    }
    (new Partner, new Partner)
  }

  /** Returns this iterator with patched values.
   *  @param from     The start index from which to patch
   *  @param ps       The iterator of patch values
   *  @param replaced The number of values in the original iterator that are replaced by the patch.
   */
  def patch[B >: A](from: Int, patchElems: Iterator[B], replaced: Int) = new Iterator[B] {
    private var origElems = self
    private var i = 0
    def hasNext: Boolean =
      if (i < from) origElems.hasNext
      else patchElems.hasNext || origElems.hasNext
    def next(): B = {
      val result: B =
        if (i < from || !patchElems.hasNext) origElems.next()
        else patchElems.next()
      i += 1
      if (i == from) origElems = origElems drop replaced
      result
    }
  }

  /** Fills the given array `xs` with at most `len` elements of
   *  this iterator starting at position `start` until either `len` elements have been copied,
   *  or the end of the iterator is reached, or the end of the array `xs` is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = start + len min xs.length
    while (hasNext && i < end) {
      xs(i) = next()
      i += 1
    }
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this iterator starting at position <code>start</code>
   *  until either the end of the current iterator or the end of array `xs` is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)

  /** Fills the given array <code>xs</code> with the elements of
   *  this iterator starting at position <code>0</code>
   *  until either the end of the current iterator or the end of array `xs` is reached.
   *
   *  @param  xs the array to fill.
   */
  def copyToArray[B >: A](xs: Array[B]): Unit = copyToArray(xs, 0, xs.length)

  /** Copy all elements to a buffer
   *  @param  dest The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    while (hasNext) dest += next
  }

  /** Traverse this iterator and return all elements in a list.
   *
   *  @return  A list which enumerates all elements of this iterator.
   */
  def toList: List[A] = {
    val res = new ListBuffer[A]
    while (hasNext) res += next
    res.toList
  }

   /** Traverse this iterator and return all elements in a stream.
   *
   *  @return  A stream which enumerates all elements of this iterator.
   */
  def toStream: Stream[A] =
    if (hasNext) Stream.cons(next, toStream) else Stream.empty

  /** Traverse this iterator and return all elements in a sequence.
   *
   *  @return  A sequence which enumerates all elements of this iterator.
   */
  def toSequence: Sequence[A] = {
    val buffer = new ArrayBuffer[A]
    this copyToBuffer buffer
    buffer
  }

  /** Returns a string representation of the elements in this iterator. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString</code>) are separated by the string
   *  <code>sep</code>.
   *  <p/>
   *  Ex: <br/>
   *  <code>List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"</code>
   *
   *  @param start starting string.
   *  @param sep separator string.
   *  @param end ending string.
   *  @return a string representation of this iterable object.
   */
  def mkString(start: String, sep: String, end: String): String = {
    val buf = new StringBuilder
    addString(buf, start, sep, end).toString
  }

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   *
   *  @param sep separator string.
   *  @return a string representation of this iterable object.
   */
  def mkString(sep: String): String = mkString("", sep, "")

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are concatenated without any separator string.
   *
   *  @return a string representation of this iterable object.
   */
  def mkString: String = mkString("")

  /** Write all elements of this iterator into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>.
   */
  def addString(buf: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    buf.append(start)
    val elems = this
    if (elems.hasNext) buf.append(elems.next)
    while (elems.hasNext) {
      buf.append(sep); buf.append(elems.next)
    }
    buf.append(end)
  }

  /** Write all elements of this iterator into given string builder.
   *  The string representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by the string <code>sep</code>.
   */
  def addString(buf: StringBuilder, sep: String): StringBuilder = addString(buf, "", sep, "")

  /** Write all elements of this string into given string builder without using
   *  any separator between consecutive elements.
   */
  def addString(buf: StringBuilder): StringBuilder = addString(buf, "", "", "")

  override def toString = (if (hasNext) "non-empty" else "empty")+" iterator"

 /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   */
  @deprecated("use <code>++</code>")
  def append[B >: A](that: Iterator[B]) = new Iterator[B] {
    def hasNext = self.hasNext || that.hasNext
    def next() = (if (self.hasNext) self else that).next()
  }

  /** Returns index of the first element satisfying a predicate, or -1. */
  @deprecated("use `indexWhere` instead")
  def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  /** Collect elements into a seq.
   *
   * @return  a sequence which enumerates all elements of this iterator.
   */
  @deprecated("use toSequence instead")
  def collect: Sequence[A] = toSequence

  /** Returns a counted iterator from this iterator.
   */
  @deprecated("use zipWithIndex in Iterator")
  def counted = new CountedIterator[A] {
    private var cnt = 0
    def count = cnt
    def hasNext: Boolean = self.hasNext
    def next(): A = { cnt += 1; self.next }
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.  Like <code>copyToArray</code>,
   *  but designed to accomodate IO stream operations.
   *
   *  @param  xs    the array to fill.
   *  @param  start the starting index.
   *  @param  sz    the maximum number of elements to be read.
   *  @pre          the array must be large enough to hold <code>sz</code> elements.
   */
  @deprecated("use copyToArray instead")
  def readInto[B >: A](xs: Array[B], start: Int, sz: Int) {
    var i = start
    while (hasNext && i - start < sz) {
      xs(i) = next
      i += 1
    }
  }
  @deprecated("use copyToArray instead")
  def readInto[B >: A](xs: Array[B], start: Int) {
    readInto(xs, start, xs.length - start)
  }
  @deprecated("use copyToArray instead")
  def readInto[B >: A](xs: Array[B]) {
    readInto(xs, 0, xs.length)
  }
}
