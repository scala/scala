/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterator.scala 15939 2008-08-26 14:33:17Z stepancheg $


package scalax.collection

import mutable.{Buffer, ArrayBuffer, ListBuffer}
import immutable.{List, Nil, ::, Stream}

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

  /**
   *  @param x the element
   *  @return  the iterator with one single element
   *  @note  Equivalent, but more efficient than Iterator(x)
   */
  def single[A](x: A) = new Iterator[A] {
    private var hasnext = true
    def hasNext: Boolean = hasnext
    def next(): A =
      if (hasnext) { hasnext = false; x }
      else empty.next()
  }

  def apply[A](args: A*): Iterator[A] = args.asInstanceOf[Iterable[A]].elements // !@!

  /** Concatenate all the argument iterators into a single iterator.
   *
   *  @param xss the lists that are to be concatenated
   *  @return the concatenation of all the lists
   */
  def concat[A](xss: Iterator[A]*): Iterator[A] =
    xss.asInstanceOf[Iterable[Iterator[A]]].elements.flatten // !@!

  /** An iterator that returns the same element a number of times
   *  @param   len  The number of elements returned
   *  @param   elem The element returned each time
   */
  def fill[A](len: Int, elem: => A) = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = i < len
    def next(): A =
      if (hasNext) { i += 1; elem }
      else empty.next()
  }

  /** An iterator that returns values of a function <code>f(0), ..., f(n-1)</code>,
   *  for given `f` and `n`.
   */
  def tabulate[A](n: Int)(f: Int => A) = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = i < n
    def next(): A =
      if (hasNext) { val result = f(i); i += 1; result }
      else empty.next()
  }

 /** Nn iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = start</code>
   *  and <code>e<sub>i</sub> &lt; end</code>. However,
   *  if <code>start &ge; end</code>, then it will return an empty range.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator
   *  @return      the iterator with values in range <code>[start;end)</code>.
   */
  def range(start: Int, end: Int): Iterator[Int] = range(start, end, 1)

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = start</code>
   *  and elements are in the range between <code>start</code> (inclusive)
   *  and <code>end</code> (exclusive)
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator
   *  @param step  the increment value of the iterator (must be positive or negative)
   *  @return      the iterator with values in range <code>[start;end)</code>.
   */
  def range(start: Int, end: Int, step: Int) = new Iterator[Int] {
    private var i = start
    def hasNext: Boolean = (step <= 0 || i < end) && (step >= 0 || i > end)
    def next(): Int =
      if (hasNext) { val result = i; i += step; result }
      else empty.next()
  }

  /** An iterator that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the iteratpr
   *  @param len   the number of elements returned by the iterator
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator returning values <code>(start, f(start), f(f(start)), ..., f<sup>len-1</sup>(start))</code>
   */
  def iterate(start: Int, len: Int)(f: Int => Int) = new Iterator[Int] {
    private var acc = start
    private var i = 0
    def hasNext: Boolean = i < len
    def next(): Int =
      if (hasNext) { val result = f(acc); i += 1; result }
      else empty.next()
  }

  /** An infinite iterator that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the iteratpr
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator returning values <code>(start, f(start), f(f(start)), ..., f<sup>len-1</sup>(start))</code>
   */
  def iterate(start: Int)(f: Int => Int) = new Iterator[Int] {
    private var acc = start
    private var i = 0
    def hasNext: Boolean = true
    def next(): Int = { val result = f(acc); i += 1; result }
  }

  /** Create an iterator with elements
   *  <code>e<sub>n+1</sub> = e<sub>n</sub> + 1</code>
   *  where <code>e<sub>0</sub> = start</code>.
   *
   *  @param start the start value of the iterator
   *  @return      the iterator starting at value <code>start</code>.
   */
  def from(start: Int): Iterator[Int] = from(start, 1)

  /** Create an iterator with elements
   * <code>e<sub>n+1</sub> = e<sub>n</sub> + step</code>
   *  where <code>e<sub>0</sub> = start</code>.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment value of the iterator
   *  @return      the iterator starting at value <code>start</code>.
   */
  def from(start: Int, step: Int): Iterator[Int] = new Iterator[Int] {
    private var i = 0
    def hasNext: Boolean = true
    def next(): Int = { val result = i; i += 1; result }
  }

  class IteratorIteratorOps[A](its: Iterator[Iterator[A]]) {
    /** Create an iterator that is the concantenation of all iterators
     *  returned by a given iterator of iterators.
     *   @param its   The iterator which returns on each call to next
     *                a new iterator whose elements are to be concatenated to the result.
     */
    def flatten: Iterator[A] = new Iterator[A] {
      private var it = its.next
      def hasNext: Boolean = if (it.hasNext) { it = its.next; hasNext } else false
      def next(): A = if (hasNext) it.next else empty.next()
    }
  }

  implicit def iteratorIteratorWrapper[A](its: Iterator[Iterator[A]]): IteratorIteratorOps[A] =
    new IteratorIteratorOps[A](its)

  /** @deprecated  use `xs.elements` instead
   */
  @deprecated def fromValues[a](xs: a*) = xs.elements

  /**
   *  @param xs the array of elements
   *  @see also: Vector.elements and slice
   *  @deprecated  use `xs.elements` instead
   */
  @deprecated def fromArray[a](xs: Array[a]): Iterator[a] =
    fromArray(xs, 0, xs.length)

  /**
   *  @param xs     the array of elements
   *  @param start  the start index
   *  @param length  the length
   *  @see also: Vector.elements and slice
   *  @deprecated  use `xs.slice(start, start + length).elements` instead
   */
  @deprecated def fromArray[a](xs: Array[a], start: Int, length: Int): Iterator[a] =
    xs.slice(start, start + length).elements.asInstanceOf[Iterator[a]] // !@!

  /**
   *  @param str the given string
   *  @return    the iterator on <code>str</code>
   *  @deprecated replaced by <code>str.elements</code>
   */
  @deprecated def fromString(str: String): Iterator[Char] =
    str.elements.asInstanceOf[Iterator[Char]] // !@!

  /**
   *  @param n the product arity
   *  @return  the iterator on <code>Product&lt;n&gt;</code>.
   *  @deprecated use product.productElements instead
   */
  @deprecated def fromProduct(n: Product): Iterator[Any] = new Iterator[Any] {
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
   *  @deprecated  use Iterator.iterate(start, end - start)(step) instead
   */
  @deprecated def range(start: Int, end: Int, step: Int => Int) = new Iterator[Int] {
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
   *  @deprecated  use iterate(start)(step) instead
   */
  @deprecated def from(start: Int, step: Int => Int): Iterator[Int] = new Iterator[Int] {
    private var i = start
    override def hasNext: Boolean = true
    def next(): Int = { val j = i; i = step(i); j }
  }

  /** Create an iterator that is the concantenation of all iterators
   *  returned by a given iterator of iterators.
   *   @param its   The iterator which returns on each call to next
   *                a new iterator whose elements are to be concatenated to the result.
   *   @deprecated  use its.flatten instead
   */
  @deprecated def flatten[T](its: Iterator[Iterator[T]]): Iterator[T] = new Iterator[T] {
    private var it = its.next
    def hasNext: Boolean = {
      while (!it.hasNext && its.hasNext) it = its.next
      it.hasNext
    }
    def next(): T =
      if (hasNext) it.next
      else empty.next()
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
trait Iterator[+A] {
self =>

  /** Does this iterator provide another element?
   */
  def hasNext: Boolean

  /** Returns the next element.
   */
  def next(): A

  /** Returns a new iterator that iterates only over the first <code>n</code>
   *  elements.
   *
   *  @param n the number of elements to take
   *  @return  the new iterator
   */
  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var remaining = n
    def hasNext = remaining > 0 && self.hasNext
    def next(): A =
      if (hasNext) { remaining -= 1; self.next }
      else throw new NoSuchElementException("next on empty iterator")
  }

  /** Removes the first <code>n</code> elements from this iterator.
   *
   *  @param n the number of elements to drop
   *  @return  the new iterator
   */
  def drop(n: Int): Iterator[A] =
    if (n > 0 && hasNext) { next(); drop(n - 1) } else this

  /** A sub-iterator of <code>until - from elements
   *  starting at index <code>from</code>
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
   *  @deprecated  use <code>++</code>
   */
  def append[B >: A](that: Iterator[B]) = new Iterator[B] {
    def hasNext = self.hasNext || that.hasNext
    def next() = (if (self.hasNext) self else that).next()
  }

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator <code>that</code>.
   */
  def ++[B >: A](that: => Iterator[B]) = new Iterator[B] {
    // optimize a little bit to prevent n log n behavior.
    var cur : Iterator[B] = self
    def hasNext = cur.hasNext || (cur eq self) && { cur = that; hasNext }
    def next() = { hasNext; cur.next }
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
   *  The behavior of <code>this</code> iterator is undefined after this method invocation.
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
      def hasNext = !lookahead.isEmpty || self.hasNext
      def next() = if (lookahead.isEmpty) self.next() else lookahead.dequeue()
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
   *  The behavior of <code>this</code> iterator is undefined after this method invocation.
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
   *  @return     an iterator yielding <code>{a<sub>0</sub>,b<sub>0</sub>},
   *              {a<sub>1</sub>,b<sub>1</sub>}, ...</code> where
   *              <code>a<sub>i</sub></code> are the elements from this iterator
   *              and <code>b<sub>i</sub></code> are the elements from iterator
   *              <code>that</code>.
   */
  def zip[B](that: Iterator[B]) = new Iterator[(A, B)] {
    def hasNext = self.hasNext && that.hasNext
    def next = (self.next, that.next)
  }

  /** Return an iterator that pairs each element of this iterator
   *  with its index, counting from 0.
   *
   *  @return      an iterator yielding <code>{a<sub>0</sub>,0},
   *               {a<sub>1</sub>,1}...</code> where <code>a<sub>i</sub></code>
   *               are the elements from this iterator.
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

  /** Apply a function <code>f</code> to all elements of this
   *  iterable object.
   *
   *  @param  f   a function that is applied to every element.
   */
  def foreach(f: A => Unit) { while (hasNext) f(next()) }

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
   *  iterable object and return true, iff there is at least one
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
   *  @return     <code>true</code> iff there is an element of this iterator which
   *              is equal (w.r.t. <code>==</code>) to <code>elem</code>.
   */
  def contains(elem: Any): Boolean = exists { _ == elem }

  /** Find and return the first element of the iterable object satisfying a
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

  /** Returns index of the first element satisying a predicate, or -1.
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

  /** Returns index of the first element satisying a predicate, or -1.
   *
   *  @deprecated use `indexWhere` instead
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
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    def fold(z: B): B = if (hasNext) op(next(), fold(z)) else z
    fold(z)
  }

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
    if (!hasNext) throw new UnsupportedOperationException("empty.reduceRight")
    foldRight[B](next())(op)
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

  def length: Int = {
    var i = 0
    while (hasNext) {
      next(); i += 1
    }
    i
  }

  /** Returns a counted iterator from this iterator.
   *  @deprecated use @see zipWithIndex in Iterator
   */
  def counted = new CountedIterator[A] {
    private var cnt = -1
    def count = cnt
    def hasNext: Boolean = self.hasNext
    def next(): A = { cnt += 1; self.next }
  }

  /** Creates two new iterators that both iterate over the same elements
   *  than this iterator (in the same order).
   *
   *  @return a pair of iterators
   */
  def duplicate: (Iterator[A], Iterator[A]) = {
    var xs: List[A] = Nil
    var ahead: Iterator[A] = null
    class Partner extends Iterator[A] {
      var ys: List[A] = Nil
      def hasNext: Boolean = self.synchronized (
        ((this == ahead) && self.hasNext) ||
        ((this != ahead) && (!xs.isEmpty || !ys.isEmpty || self.hasNext))
      )
      def next(): A = self.synchronized {
        if (this == ahead) {
          val e = self.next()
          xs = e :: xs; e
        } else {
          if (ys.isEmpty) {
            ys = xs.reverse
            xs = Nil
          }
          ys match {
            case Nil =>
              val e = self.next()
              ahead = this
              xs = e :: xs; e
            case z :: zs =>
              ys = zs; z
          }
        }
      }
    }
    ahead = new Partner
    (ahead, new Partner)
  }

  def patch[B >: A](from: Int, ps: Sequence[B], replaced: Int) = new Iterator[B] {
    private val plen = ps.length
    private var origElems = self
    private val patchElems = ps.elements
    private var i = 0
    def hasNext: Boolean =
      if (i < from) origElems.hasNext
      else patchElems.hasNext || origElems.hasNext
    def next(): B = {
      val result: B =
        if (i < from || i >= from + plen) origElems.next()
        else patchElems.next()
      i += 1
      if (i == from) origElems = origElems drop replaced
      result
    }
  }

  /** Fills the given array <code>xs</code> with at most `len` elements of
   *  this iterator starting at position `start`.
   *  Copying will stop once either the end of the current iterable is reached or
   *  `len` elements have been copied.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = start + len
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
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)

  /** Fills the given array <code>xs</code> with the elements of
   *  this iterator starting at position <code>0</code>
   *  until either the end of the current iterator or the end of array `xs` is reached.
   *
   *  @param  xs the array to fill.
   *  @pre    the array must be large enough to hold all elements.
   */
  def copyToArray[B >: A](xs: Array[B]): Unit = copyToArray(xs, 0, xs.length)

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.  Like <code>copyToArray</code>,
   *  but designed to accomodate IO stream operations.
   *
   *  @param  xs    the array to fill.
   *  @param  start the starting index.
   *  @param  sz    the maximum number of elements to be read.
   *  @pre          the array must be large enough to hold <code>sz</code> elements.
   *  @deprecated   use copyToArray instead
   */
  @deprecated def readInto[B >: A](xs: Array[B], start: Int, sz: Int) {
    var i = start
    while (hasNext && i - start < sz) {
      xs(i) = next
      i += 1
    }
  }
  @deprecated def readInto[B >: A](xs: Array[B], start: Int) {
    readInto(xs, start, xs.length - start)
  }
  @deprecated def readInto[B >: A](xs: Array[B]) {
    readInto(xs, 0, xs.length)
  }

  /** Copy all elements to a buffer
   *  @param   The buffer to which elements are copied
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    while (hasNext) dest += next
  }

  /** Transform this iterator into a list of all elements.
   *
   *  @return  a list which enumerates all elements of this iterator.
   */
  def toList: List[A] = {
    val res = new ListBuffer[A]
    while (hasNext) res += next
    res.toList
  }

  /**
   *  Create a stream which contains all the elements of this iterator.
   */
  def toStream: Stream[A] =
    if (hasNext) Stream.cons(next, toStream) else Stream.empty

  /**
   *  Create a sequence which contains all the elements of this iterator.
   */
  def toSequence: Sequence[A] = {
    val buffer = new ArrayBuffer[A]
    this copyToBuffer buffer
    buffer.readOnly
  }

  /** Collect elements into a seq.
   *
   * @return  a sequence which enumerates all elements of this iterator.
   * @deprecated  use toSequence instead
   */
  @deprecated def collect: Sequence[A] = toSequence

  /** Returns a string representation of the elements in this iterator. The resulting string
   *  begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
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
  def mkString(sep: String): String = this.mkString("", sep, "")

  /** Returns a string representation of this iterable object. The string
   *  representations of elements (w.r.t. the method <code>toString()</code>)
   *  are separated by a comma.
   *
   *  @return a string representation of this iterable object.
   */
  def mkString: String =
    mkString("")

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
  def addString(buf: StringBuilder, sep: String): StringBuilder =
    addString(buf, "", sep, "")

  /** Write all elements of this string into given string builder without using
   *  any separator between consecutive elements.
   */
  def addString(buf: StringBuilder): StringBuilder =
    addString(buf, "", "", "")

  override def toString = (if (hasNext) "non-empty" else "empty")+" iterator"

}
