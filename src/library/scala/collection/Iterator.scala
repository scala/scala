/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import mutable.ArrayBuffer
import scala.annotation.{tailrec, migration}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import immutable.Stream

/** The `Iterator` object provides various functions for creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @since   2.8
 */
object Iterator {

  /** With the advent of `TraversableOnce` and `Iterator`, it can be useful to have a builder which
   *  operates on `Iterator`s so they can be treated uniformly along with the collections.
   *  See `scala.util.Random.shuffle` for an example.
   */
  implicit def IteratorCanBuildFrom[A] = new TraversableOnce.BufferedCanBuildFrom[A, Iterator] {
    def bufferToColl[B](coll: ArrayBuffer[B]) = coll.iterator
    def traversableToColl[B](t: GenTraversable[B]) = t.toIterator
  }

  /** The iterator which produces no values. */
  val empty: Iterator[Nothing] = new AbstractIterator[Nothing] {
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  /** Creates an iterator which produces a single element.
   *  '''Note:''' Equivalent, but more efficient than Iterator(elem)
   *
   *  @param elem the element
   *  @return An iterator which produces `elem` on the first call to `next`,
   *          and which has no further elements.
   */
  def single[A](elem: A): Iterator[A] = new AbstractIterator[A] {
    private var hasnext = true
    def hasNext: Boolean = hasnext
    def next(): A =
      if (hasnext) { hasnext = false; elem }
      else empty.next()
  }

  /** Creates an iterator with given elements.
   *
   *  @param elems  The elements returned one-by-one from the iterator
   *  @return An iterator which produces the given elements on the
   *          first calls to `next`, and which has no further elements.
   */
  def apply[A](elems: A*): Iterator[A] = elems.iterator

  /** Creates iterator that produces the results of some element computation a number of times.
   *
   *  @param   len  the number of elements returned by the iterator.
   *  @param   elem the element computation
   *  @return  An iterator that produces the results of `n` evaluations of `elem`.
   */
  def fill[A](len: Int)(elem: => A): Iterator[A] = new AbstractIterator[A] {
    private var i = 0
    def hasNext: Boolean = i < len
    def next(): A =
      if (hasNext) { i += 1; elem }
      else empty.next()
  }

  /** Creates an iterator producing the values of a given function over a range of integer values starting from 0.
   *
   *  @param  end The number of elements returned by the iterator
   *  @param  f   The function computing element values
   *  @return An iterator that produces the values `f(0), ..., f(n -1)`.
   */
  def tabulate[A](end: Int)(f: Int => A): Iterator[A] = new AbstractIterator[A] {
    private var i = 0
    def hasNext: Boolean = i < end
    def next(): A =
      if (hasNext) { val result = f(i); i += 1; result }
      else empty.next()
  }

  /** Creates nn iterator returning successive values in some integer interval.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @return      the iterator producing values `start, start + 1, ..., end - 1`
   */
  def range(start: Int, end: Int): Iterator[Int] = range(start, end, 1)

  /** An iterator producing equally spaced values in some integer interval.
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator (the first value NOT returned)
   *  @param step  the increment value of the iterator (must be positive or negative)
   *  @return      the iterator producing values `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    if (step == 0) throw new IllegalArgumentException("zero step")
    private var i = start
    private var hasOverflowed = false
    def hasNext: Boolean = {
      (step <= 0 || i < end) && (step >= 0 || i > end) && !hasOverflowed
    }
    def next(): Int =
      if (hasNext) {
        val result = i
        val nextValue = i + step
        hasOverflowed = (step > 0) == nextValue < i
        i = nextValue
        result
      }
      else empty.next()
  }

  /** Creates an infinite iterator that repeatedly applies a given function to the previous result.
   *
   *  @param start the start value of the iterator
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator producing the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[T](start: T)(f: T => T): Iterator[T] = new AbstractIterator[T] {
    private[this] var first = true
    private[this] var acc = start
    def hasNext: Boolean = true
    def next(): T = {
      if (first) first = false
      else acc = f(acc)

      acc
    }
  }

  /** Creates an infinite-length iterator which returns successive values from some start value.

   *  @param start the start value of the iterator
   *  @return      the iterator producing the infinite sequence of values `start, start + 1, start + 2, ...`
   */
  def from(start: Int): Iterator[Int] = from(start, 1)

  /** Creates an infinite-length iterator returning values equally spaced apart.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment between successive values
   *  @return      the iterator producing the infinite sequence of values `start, start + 1 * step, start + 2 * step, ...`
   */
  def from(start: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Int = { val result = i; i += step; result }
  }

  /** Creates an infinite-length iterator returning the results of evaluating an expression.
   *  The expression is recomputed for every element.
   *
   *  @param elem the element computation.
   *  @return the iterator containing an infinite number of results of evaluating `elem`.
   */
  def continually[A](elem: => A): Iterator[A] = new AbstractIterator[A] {
    def hasNext = true
    def next = elem
  }

  /** Creates an iterator to which other iterators can be appended efficiently.
   *  Nested ConcatIterators are merged to avoid blowing the stack.
   */
  private final class ConcatIterator[+A](private var current: Iterator[A @uV]) extends Iterator[A] {
    private var tail: ConcatIteratorCell[A @uV] = null
    private var last: ConcatIteratorCell[A @uV] = null
    private var currentHasNextChecked = false

    // Advance current to the next non-empty iterator
    // current is set to null when all iterators are exhausted
    @tailrec
    private[this] def advance(): Boolean = {
      if (tail eq null) {
        current = null
        last = null
        false
      }
      else {
        current = tail.headIterator
        tail = tail.tail
        merge()
        if (currentHasNextChecked) true
        else if ((current ne null) && current.hasNext) {
          currentHasNextChecked = true
          true
        } else advance()
      }
    }

    // If the current iterator is a ConcatIterator, merge it into this one
    @tailrec
    private[this] def merge(): Unit =
      if (current.isInstanceOf[ConcatIterator[_]]) {
        val c = current.asInstanceOf[ConcatIterator[A]]
        current = c.current
        currentHasNextChecked = c.currentHasNextChecked
        if (c.tail ne null) {
          c.last.tail = tail
          tail = c.tail
        }
        merge()
      }

    def hasNext =
      if (currentHasNextChecked) true
      else if (current eq null) false
      else if (current.hasNext) {
        currentHasNextChecked = true
        true
      } else advance()

    def next()  =
      if (hasNext) {
        currentHasNextChecked = false
        current.next()
      } else Iterator.empty.next()

    override def ++[B >: A](that: => GenTraversableOnce[B]): Iterator[B] = {
      val c = new ConcatIteratorCell[B](that, null).asInstanceOf[ConcatIteratorCell[A]]
      if(tail eq null) {
        tail = c
        last = c
      } else {
        last.tail = c
        last = c
      }
      if(current eq null) current = Iterator.empty
      this
    }
  }

  private[this] final class ConcatIteratorCell[A](head: => GenTraversableOnce[A], var tail: ConcatIteratorCell[A]) {
    def headIterator: Iterator[A] = head.toIterator
  }

  /** Creates a delegating iterator capped by a limit count. Negative limit means unbounded.
   *  Lazily skip to start on first evaluation.  Avoids daisy-chained iterators due to slicing.
   */
  private[scala] final class SliceIterator[A](val underlying: Iterator[A], start: Int, limit: Int) extends AbstractIterator[A] {
    private var remaining = limit
    private var dropping  = start
    @inline private def unbounded = remaining < 0
    private def skip(): Unit =
      while (dropping > 0) {
        if (underlying.hasNext) {
          underlying.next()
          dropping -= 1
        } else
          dropping = 0
      }
    def hasNext = { skip(); remaining != 0 && underlying.hasNext }
    def next()  = {
      skip()
      if (remaining > 0) {
        remaining -= 1
        underlying.next()
      }
      else if (unbounded) underlying.next()
      else empty.next()
    }
    override protected def sliceIterator(from: Int, until: Int): Iterator[A] = {
      val lo = from max 0
      def adjustedBound =
        if (unbounded) -1
        else 0 max (remaining - lo)
      val rest =
        if (until < 0) adjustedBound          // respect current bound, if any
        else if (until <= lo) 0               // empty
        else if (unbounded) until - lo        // now finite
        else adjustedBound min (until - lo)   // keep lesser bound
      if (rest == 0) empty
      else {
        dropping += lo
        remaining = rest
        this
      }
    }
  }
}

/** Iterators are data structures that allow to iterate over a sequence
 *  of elements. They have a `hasNext` method for checking
 *  if there is a next element available, and a `next` method
 *  which returns the next element and advances the iterator.
 *
 *  An iterator is mutable: most operations on it change its state. While it is often used
 *  to iterate through the elements of a collection, it can also be used without
 *  being backed by any collection (see constructors on the companion object).
 *
 *  It is of particular importance to note that, unless stated otherwise, ''one should never
 *  use an iterator after calling a method on it''. The two most important exceptions
 *  are also the sole abstract methods: `next` and `hasNext`.
 *
 *  Both these methods can be called any number of times without having to discard the
 *  iterator. Note that even `hasNext` may cause mutation -- such as when iterating
 *  from an input stream, where it will block until the stream is closed or some
 *  input becomes available.
 *
 *  Consider this example for safe and unsafe use:
 *
 *  {{{
 *  def f[A](it: Iterator[A]) = {
 *    if (it.hasNext) {            // Safe to reuse "it" after "hasNext"
 *      it.next                    // Safe to reuse "it" after "next"
 *      val remainder = it.drop(2) // it is *not* safe to use "it" again after this line!
 *      remainder.take(2)          // it is *not* safe to use "remainder" after this line!
 *    } else it
 *  }
 *  }}}
 *
 *  @author  Martin Odersky, Matthias Zenger
 *  @since   1
 *  @define willNotTerminateInf
 *  Note: will not terminate for infinite iterators.
 *  @define mayNotTerminateInf
 *  Note: may not terminate for infinite iterators.
 *  @define preservesIterator
 *  The iterator remains valid for further use whatever result is returned.
 *  @define consumesIterator
 *  After calling this method, one should discard the iterator it was called
 *  on. Using it is undefined and subject to change.
 *  @define consumesAndProducesIterator
 *  After calling this method, one should discard the iterator it was called
 *  on, and use only the iterator that was returned. Using the old iterator
 *  is undefined, subject to change, and may result in changes to the new
 *  iterator as well.
 *  @define consumesTwoAndProducesOneIterator
 *  After calling this method, one should discard the iterator it was called
 *  on, as well as the one passed as a parameter, and use only the iterator
 *  that was returned. Using the old iterators is undefined, subject to change,
 *  and may result in changes to the new iterator as well.
 *  @define consumesOneAndProducesTwoIterators
 *  After calling this method, one should discard the iterator it was called
 *  on, and use only the iterators that were returned. Using the old iterator
 *  is undefined, subject to change, and may result in changes to the new
 *  iterators as well.
 *  @define consumesTwoIterators
 *  After calling this method, one should discard the iterator it was called
 *  on, as well as the one passed as parameter. Using the old iterators is
 *  undefined and subject to change.
 */
trait Iterator[+A] extends TraversableOnce[A] {
  self =>

  import Iterator.empty

  def seq: Iterator[A] = this

  /** Tests whether this iterator can provide another element.
   *
   *  @return  `true` if a subsequent call to `next` will yield an element,
   *           `false` otherwise.
   *  @note    Reuse: $preservesIterator
   */
  def hasNext: Boolean

  /** Produces the next element of this iterator.
   *
   *  @return  the next element of this iterator, if `hasNext` is `true`,
   *           undefined behavior otherwise.
   *  @note    Reuse: $preservesIterator
   */
  def next(): A

  /** Tests whether this iterator is empty.
   *
   *  @return   `true` if hasNext is false, `false` otherwise.
   *  @note     Reuse: $preservesIterator
   */
  def isEmpty: Boolean = !hasNext

  /** Tests whether this Iterator can be repeatedly traversed.
   *
   *  @return   `false`
   *  @note     Reuse: $preservesIterator
   */
  def isTraversableAgain = false

  /** Tests whether this Iterator has a known size.
   *
   *  @return   `true` for empty Iterators, `false` otherwise.
   *  @note     Reuse: $preservesIterator
   */
  def hasDefiniteSize = isEmpty

  /** Selects first ''n'' values of this iterator.
   *
   *  @param  n    the number of values to take
   *  @return an iterator producing only the first `n` values of this iterator, or else the
   *          whole iterator, if it produces fewer than `n` values.
   *  @note   Reuse: $consumesAndProducesIterator
   */
  def take(n: Int): Iterator[A] = sliceIterator(0, n max 0)

  /** Advances this iterator past the first ''n'' elements, or the length of the iterator, whichever is smaller.
   *
   *  @param n the number of elements to drop
   *  @return  an iterator which produces all values of the current iterator, except
   *           it omits the first `n` values.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def drop(n: Int): Iterator[A] = {
    var j = 0
    while (j < n && hasNext) {
      next()
      j += 1
    }
    this
  }

  /** Creates an iterator returning an interval of the values produced by this iterator.
   *
   *  @param from   the index of the first element in this iterator which forms part of the slice.
   *                If negative, the slice starts at zero.
   *  @param until  the index of the first element following the slice. If negative, the slice is empty.
   *  @return an iterator which advances this iterator past the first `from` elements using `drop`,
   *  and then takes `until - from` elements, using `take`.
   *  @note         Reuse: $consumesAndProducesIterator
   */
  def slice(from: Int, until: Int): Iterator[A] = sliceIterator(from, until max 0)

  /** Creates an optionally bounded slice, unbounded if `until` is negative. */
  protected def sliceIterator(from: Int, until: Int): Iterator[A] = {
    val lo = from max 0
    val rest =
      if (until < 0) -1            // unbounded
      else if (until <= lo) 0      // empty
      else until - lo              // finite

    if (rest == 0) empty
    else new Iterator.SliceIterator(this, lo, rest)
  }

  /** Creates a new iterator that maps all produced values of this iterator
   *  to new values using a transformation function.
   *
   *  @param f  the transformation function
   *  @return a new iterator which transforms every value produced by this
   *          iterator by applying the function `f` to it.
   *  @note   Reuse: $consumesAndProducesIterator
   */
  def map[B](f: A => B): Iterator[B] = new AbstractIterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  /** Concatenates this iterator with another.
   *
   *  @param   that   the other iterator
   *  @return  a new iterator that first yields the values produced by this
   *  iterator followed by the values produced by iterator `that`.
   *  @note    Reuse: $consumesTwoAndProducesOneIterator
   *
   *  @usecase def ++(that: => Iterator[A]): Iterator[A]
   *    @inheritdoc
   */
  def ++[B >: A](that: => GenTraversableOnce[B]): Iterator[B] = new Iterator.ConcatIterator(self) ++ that

  /** Creates a new iterator by applying a function to all values produced by this iterator
   *  and concatenating the results.
   *
   *  @param f the function to apply on each element.
   *  @return  the iterator resulting from applying the given iterator-valued function
   *           `f` to each value produced by this iterator and concatenating the results.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def flatMap[B](f: A => GenTraversableOnce[B]): Iterator[B] = new AbstractIterator[B] {
    private var cur: Iterator[B] = empty
    private def nextCur(): Unit = { cur = null ; cur = f(self.next()).toIterator }
    def hasNext: Boolean = {
      // Equivalent to cur.hasNext || self.hasNext && { nextCur(); hasNext }
      // but slightly shorter bytecode (better JVM inlining!)
      while (!cur.hasNext) {
        if (!self.hasNext) return false
        nextCur()
      }
      true
    }
    def next(): B = (if (hasNext) cur else empty).next()
  }

  /** Returns an iterator over all the elements of this iterator that satisfy the predicate `p`.
   *  The order of the elements is preserved.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def filter(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
    // TODO 2.12 - Make a full-fledged FilterImpl that will reverse sense of p
    private var hd: A = _
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!self.hasNext) return false
        hd = self.next()
      } while (!p(hd))
      hdDefined = true
      true
    }

    def next() = if (hasNext) { hdDefined = false; hd } else empty.next()
  }

  /** Tests whether every element of this iterator relates to the
   *  corresponding element of another collection by satisfying a test predicate.
   *
   *  @param   that    the other collection
   *  @param   p       the test predicate, which relates elements from both collections
   *  @tparam  B       the type of the elements of `that`
   *  @return          `true` if both collections have the same length and
   *                   `p(x, y)` is `true` for all corresponding elements `x` of this iterator
   *                   and `y` of `that`, otherwise `false`
   */
  def corresponds[B](that: GenTraversableOnce[B])(p: (A, B) => Boolean): Boolean = {
    val that0 = that.toIterator
    while (hasNext && that0.hasNext)
      if (!p(next(), that0.next())) return false

    hasNext == that0.hasNext
  }

  /** Creates an iterator over all the elements of this iterator that
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  '''Note:''' `withFilter` is the same as `filter` on iterators. It exists so that
   *  for-expressions with filters work over iterators.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)

  /** Creates an iterator over all the elements of this iterator which
   *  do not satisfy a predicate p.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which do not satisfy the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def filterNot(p: A => Boolean): Iterator[A] = filter(!p(_))

 /** Creates an iterator by transforming values
  *  produced by this iterator with a partial function, dropping those
  *  values for which the partial function is not defined.
  *
  *  @param pf the partial function which filters and maps the iterator.
  *  @return   a new iterator which yields each value `x` produced by this iterator for
  *  which `pf` is defined the image `pf(x)`.
  *  @note     Reuse: $consumesAndProducesIterator
  */
  @migration("`collect` has changed. The previous behavior can be reproduced with `toSeq`.", "2.8.0")
  def collect[B](pf: PartialFunction[A, B]): Iterator[B] = new AbstractIterator[B] {
    // Manually buffer to avoid extra layer of wrapping with buffered
    private[this] var hd: A = _

    // Little state machine to keep track of where we are
    // Seek = 0; Found = 1; Empty = -1
    // Not in vals because scalac won't make them static (@inline def only works with -optimize)
    // BE REALLY CAREFUL TO KEEP COMMENTS AND NUMBERS IN SYNC!
    private[this] var status = 0/*Seek*/

    def hasNext = {
      while (status == 0/*Seek*/) {
        if (self.hasNext) {
          hd = self.next()
          if (pf.isDefinedAt(hd)) status = 1/*Found*/
        }
        else status = -1/*Empty*/
      }
      status == 1/*Found*/
    }
    def next() = if (hasNext) { status = 0/*Seek*/; pf(hd) } else Iterator.empty.next()
  }

  /** Produces a collection containing cumulative results of applying the
   *  operator going left to right, including the initial value.
   *
   *  $willNotTerminateInf
   *  $orderDependent
   *
   *  @tparam B      the type of the elements in the resulting collection
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate result and the element
   *  @return        iterator with intermediate results
   *  @note          Reuse: $consumesAndProducesIterator
   */
  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B] = new AbstractIterator[B] {
    private[this] var state = 0    // 1 consumed initial, 2 self.hasNext, 3 done
    private[this] var accum = z
    private[this] def gen() = { val res = op(accum, self.next()) ; accum = res ; res }
    def hasNext = state match {
      case 0 | 2 => true
      case 3     => false
      case _     => if (self.hasNext) { state = 2 ; true } else { state = 3 ; false }
    }
    def next() = state match {
      case 0 => state = 1 ; accum
      case 1 => gen()
      case 2 => state = 1 ; gen()
      case 3 => Iterator.empty.next()
    }
  }

  /** Produces a collection containing cumulative results of applying the operator going right to left.
   *  The head of the collection is the last cumulative result.
   *
   *  $willNotTerminateInf
   *  $orderDependent
   *
   *  @tparam B      the type of the elements in the resulting collection
   *  @param z       the initial value
   *  @param op      the binary operator applied to the intermediate result and the element
   *  @return        iterator with intermediate results
   *  @example       {{{
   *    Iterator(1, 2, 3, 4).scanRight(0)(_ + _).toList == List(10, 9, 7, 4, 0)
   *  }}}
   *  @note          Reuse: $consumesAndProducesIterator
   */
  def scanRight[B](z: B)(op: (A, B) => B): Iterator[B] = toBuffer.scanRight(z)(op).iterator

  /** Takes longest prefix of values produced by this iterator that satisfy a predicate.
   *
   *  @param   p  The predicate used to test elements.
   *  @return  An iterator returning the values produced by this iterator, until
   *           this iterator produces a value that does not satisfy
   *           the predicate `p`.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def takeWhile(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false
    private var tail: Iterator[A] = self

    def hasNext = hdDefined || tail.hasNext && {
      hd = tail.next()
      if (p(hd)) hdDefined = true
      else tail = Iterator.empty
      hdDefined
    }
    def next() = if (hasNext) { hdDefined = false; hd } else empty.next()
  }

  /** Partitions this iterator in two iterators according to a predicate.
   *
   *  @param p the predicate on which to partition
   *  @return  a pair of iterators: the iterator that satisfies the predicate
   *           `p` and the iterator that does not.
   *           The relative order of the elements in the resulting iterators
   *           is the same as in the original iterator.
   *  @note    Reuse: $consumesOneAndProducesTwoIterators
   */
  def partition(p: A => Boolean): (Iterator[A], Iterator[A]) = {
    val self = buffered
    class PartitionIterator(p: A => Boolean) extends AbstractIterator[A] {
      var other: PartitionIterator = _
      val lookahead = new mutable.Queue[A]
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

  /** Splits this Iterator into a prefix/suffix pair according to a predicate.
   *
   *  @param p the test predicate
   *  @return  a pair of Iterators consisting of the longest prefix of this
   *           whose elements all satisfy `p`, and the rest of the Iterator.
   *  @note    Reuse: $consumesOneAndProducesTwoIterators
   */
  def span(p: A => Boolean): (Iterator[A], Iterator[A]) = {
    /*
     * Giving a name to following iterator (as opposed to trailing) because
     * anonymous class is represented as a structural type that trailing
     * iterator is referring (the finish() method) and thus triggering
     * handling of structural calls. It's not what's intended here.
     */
    class Leading extends AbstractIterator[A] {
      private[this] var lookahead: mutable.Queue[A] = null
      private[this] var hd: A = _
      /* Status is kept with magic numbers
       *   1 means next element is in hd and we're still reading into this iterator
       *   0 means we're still reading but haven't found a next element
       *   -1 means we are done reading into the iterator, so we must rely on lookahead
       *   -2 means we are done but have saved hd for the other iterator to use as its first element
       */
      private[this] var status = 0
      private def store(a: A) {
        if (lookahead == null) lookahead = new mutable.Queue[A]
        lookahead += a
      }
      def hasNext = {
        if (status < 0) (lookahead ne null) && lookahead.nonEmpty
        else if (status > 0) true
        else {
          if (self.hasNext) {
            hd = self.next()
            status = if (p(hd)) 1 else -2
          }
          else status = -1
          status > 0
        }
      }
      def next() = {
        if (hasNext) {
          if (status == 1) { status = 0; hd }
          else lookahead.dequeue()
        }
        else empty.next()
      }
      def finish(): Boolean = status match {
        case -2 => status = -1 ; true
        case -1 => false
        case  1 => store(hd) ; status = 0 ; finish()
        case  0 => 
          status = -1
          while (self.hasNext) {
            val a = self.next()
            if (p(a)) store(a)
            else {
              hd = a
              return true
            }
          }
          false
      }
      def trailer: A = hd
    }

    val leading = new Leading

    val trailing = new AbstractIterator[A] {
      private[this] var myLeading = leading
      /* Status flag meanings:
       *   -1 not yet accessed
       *   0 single element waiting in leading
       *   1 defer to self
       *   2 self.hasNext already
       *   3 exhausted
       */
      private[this] var status = -1
      def hasNext = status match {
        case 3 => false
        case 2 => true
        case 1 => if (self.hasNext) { status = 2 ; true } else { status = 3 ; false }
        case 0 => true
        case _ =>
          if (myLeading.finish()) { status = 0 ; true } else { status = 1 ; myLeading = null ; hasNext }
      }
      def next() = {
        if (hasNext) {
          if (status == 0) {
            status = 1
            val res = myLeading.trailer
            myLeading = null
            res
          } else {
            status = 1
            self.next()
          }
        }
        else empty.next()
      }
    }

    (leading, trailing)
  }

  /** Skips longest sequence of elements of this iterator which satisfy given
   *  predicate `p`, and returns an iterator of the remaining elements.
   *
   *  @param p the predicate used to skip elements.
   *  @return  an iterator consisting of the remaining elements
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def dropWhile(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
    // Magic value: -1 = hasn't dropped, 0 = found first, 1 = defer to parent iterator
    private[this] var status = -1
    // Local buffering to avoid double-wrap with .buffered
    private[this] var fst: A = _
    def hasNext: Boolean =
      if (status == 1) self.hasNext
      else if (status == 0) true
      else {
        while (self.hasNext) {
          val a = self.next()
          if (!p(a)) {
            fst = a
            status = 0
            return true
          }
        }
        status = 1
        false
      }
    def next() =
      if (hasNext) {
        if (status == 1) self.next()
        else {
          status = 1
          fst
        }
      }
      else Iterator.empty.next()
  }

  /** Creates an iterator formed from this iterator and another iterator
   *  by combining corresponding values in pairs.
   *  If one of the two iterators is longer than the other, its remaining
   *  elements are ignored.
   *
   *  @param   that  The iterator providing the second half of each result pair
   *  @return        a new iterator containing pairs consisting of
   *                 corresponding elements of this iterator and `that`. The number
   *                 of elements returned by the new iterator is the
   *                 minimum of the number of elements returned by this
   *                 iterator and `that`.
   *  @note          Reuse: $consumesTwoAndProducesOneIterator
   */
  def zip[B](that: Iterator[B]): Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    def hasNext = self.hasNext && that.hasNext
    def next = (self.next(), that.next())
  }

  /** Appends an element value to this iterator until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @return a new iterator consisting of producing all values of this iterator,
   *          followed by the minimal number of occurrences of `elem` so
   *          that the number of produced values is at least `len`.
   *  @note    Reuse: $consumesAndProducesIterator
   *
   *  @usecase def padTo(len: Int, elem: A): Iterator[A]
   *    @inheritdoc
   */
  def padTo[A1 >: A](len: Int, elem: A1): Iterator[A1] = new AbstractIterator[A1] {
    private var count = 0
    def hasNext = self.hasNext || count < len
    def next = {
      count += 1
      if (self.hasNext) self.next()
      else if (count <= len) elem
      else empty.next()
    }
  }

  /** Creates an iterator that pairs each element produced by this iterator
   *  with its index, counting from 0.
   *
   *  @return        a new iterator containing pairs consisting of
   *                 corresponding elements of this iterator and their indices.
   *  @note          Reuse: $consumesAndProducesIterator
   */
  def zipWithIndex: Iterator[(A, Int)] = new AbstractIterator[(A, Int)] {
    var idx = 0
    def hasNext = self.hasNext
    def next = {
      val ret = (self.next(), idx)
      idx += 1
      ret
    }
  }

  /** Creates an iterator formed from this iterator and another iterator
   *  by combining corresponding elements in pairs.
   *  If one of the two iterators is shorter than the other,
   *  placeholder elements are used to extend the shorter iterator to the length of the longer.
   *
   *  @param that     iterator `that` may have a different length
   *                  as the self iterator.
   *  @param thisElem element `thisElem` is used to fill up the
   *                  resulting iterator if the self iterator is shorter than
   *                  `that`
   *  @param thatElem element `thatElem` is used to fill up the
   *                  resulting iterator if `that` is shorter than
   *                  the self iterator
   *  @return         a new iterator containing pairs consisting of
   *                  corresponding values of this iterator and `that`. The length
   *                  of the returned iterator is the maximum of the lengths of this iterator and `that`.
   *                  If this iterator is shorter than `that`, `thisElem` values are used to pad the result.
   *                  If `that` is shorter than this iterator, `thatElem` values are used to pad the result.
   *  @note           Reuse: $consumesTwoAndProducesOneIterator
   *
   *  @usecase def zipAll[B](that: Iterator[B], thisElem: A, thatElem: B): Iterator[(A, B)]
   *    @inheritdoc
   */
  def zipAll[B, A1 >: A, B1 >: B](that: Iterator[B], thisElem: A1, thatElem: B1): Iterator[(A1, B1)] = new AbstractIterator[(A1, B1)] {
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

  /** Applies a function `f` to all values produced by this iterator.
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @note    Reuse: $consumesIterator
   *
   *  @usecase def foreach(f: A => Unit): Unit
   *    @inheritdoc
   */
  def foreach[U](f: A => U) { while (hasNext) f(next()) }

  /** Tests whether a predicate holds for all values produced by this iterator.
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for all values
   *                 produced by this iterator, otherwise `false`.
   *  @note          Reuse: $consumesIterator
   */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    while (res && hasNext) res = p(next())
    res
  }

  /** Tests whether a predicate holds for some of the values produced by this iterator.
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for some of the values
   *                 produced by this iterator, otherwise `false`.
   *  @note          Reuse: $consumesIterator
   */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    while (!res && hasNext) res = p(next())
    res
  }

  /** Tests whether this iterator contains a given value as an element.
   *  $mayNotTerminateInf
   *
   *  @param elem  the element to test.
   *  @return     `true` if this iterator produces some value that is
   *               is equal (as determined by `==`) to `elem`, `false` otherwise.
   *  @note        Reuse: $consumesIterator
   */
  def contains(elem: Any): Boolean = exists(_ == elem)    // Note--this seems faster than manual inlining!

  /** Finds the first value produced by the iterator satisfying a
   *  predicate, if any.
   *  $mayNotTerminateInf
   *
   *  @param p the predicate used to test values.
   *  @return  an option value containing the first value produced by the iterator that satisfies
   *           predicate `p`, or `None` if none exists.
   *  @note    Reuse: $consumesIterator
   */
  def find(p: A => Boolean): Option[A] = {
    while (hasNext) {
      val a = next()
      if (p(a)) return Some(a)
    }
    None
  }

  /** Returns the index of the first produced value satisfying a predicate, or -1.
   *  $mayNotTerminateInf
   *
   *  @param  p the predicate to test values
   *  @return   the index of the first produced value satisfying `p`,
   *           or -1 if such an element does not exist until the end of the iterator is reached.
   *  @note    Reuse: $consumesIterator
   */
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)

  /** Returns the index of the first produced value satisfying a predicate, or -1, after or at
   *  some start index.
   *  $mayNotTerminateInf
   *
   *  @param p the predicate to test values
   *  @param from the start index
   *  @return the index `>= from` of the first produced value satisfying `p`,
   *          or -1 if such an element does not exist until the end of the iterator is reached.
   *  @note   Reuse: $consumesIterator
   */
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = 0
    while (i < from && hasNext) {
      next()
      i += 1
    }

    while (hasNext) {
      if (p(next())) return i
      i += 1
    }
    -1
  }

  /** Returns the index of the first occurrence of the specified
   *  object in this iterable object.
   *  $mayNotTerminateInf
   *
   *  @param  elem  element to search for.
   *  @return the index of the first occurrence of `elem` in the values produced by this iterator,
   *          or -1 if such an element does not exist until the end of the iterator is reached.
   *  @note   Reuse: $consumesIterator
   */
  def indexOf[B >: A](elem: B): Int = indexOf(elem, 0)

  /** Returns the index of the first occurrence of the specified object in this iterable object
   *  after or at some start index.
   *  $mayNotTerminateInf
   *
   *  @param elem element to search for.
   *  @param from the start index
   *  @return the index `>= from` of the first occurrence of `elem` in the values produced by this
   *          iterator, or -1 if such an element does not exist until the end of the iterator is
   *          reached.
   *  @note   Reuse: $consumesIterator
   */
  def indexOf[B >: A](elem: B, from: Int): Int = {
    var i = 0
    while (i < from && hasNext) {
      next()
      i += 1
    }

    while (hasNext) {
      if (next() == elem) return i
      i += 1
    }
    -1
  }

  /** Creates a buffered iterator from this iterator.
   *
   *  @see [[scala.collection.BufferedIterator]]
   *  @return  a buffered iterator producing the same values as this iterator.
   *  @note    Reuse: $consumesAndProducesIterator
   */
  def buffered: BufferedIterator[A] = new AbstractIterator[A] with BufferedIterator[A] {
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

    def next() =
      if (hdDefined) {
        hdDefined = false
        hd
      } else self.next()
  }

  /** A flexible iterator for transforming an `Iterator[A]` into an
   *  Iterator[Seq[A]], with configurable sequence size, step, and
   *  strategy for dealing with elements which don't fit evenly.
   *
   *  Typical uses can be achieved via methods `grouped` and `sliding`.
   */
  class GroupedIterator[B >: A](self: Iterator[A], size: Int, step: Int)
  extends AbstractIterator[Seq[B]]
     with Iterator[Seq[B]] {

    require(size >= 1 && step >= 1, f"size=$size%d and step=$step%d, but both must be positive")

    private[this] var buffer: ArrayBuffer[B] = ArrayBuffer()  // the buffer
    private[this] var filled = false                          // whether the buffer is "hot"
    private[this] var _partial = true                         // whether we deliver short sequences
    private[this] var pad: Option[() => B] = None             // what to pad short sequences with

    /** Public functions which can be used to configure the iterator before use.
     *
     *  Pads the last segment if necessary so that all segments will
     *  have the same size.
     *
     *  @param x The element that will be appended to the last segment, if necessary.
     *  @return  The same iterator, and ''not'' a new iterator.
     *  @note    This method mutates the iterator it is called on, which can be safely used afterwards.
     *  @note    This method is mutually exclusive with `withPartial(true)`.
     */
    def withPadding(x: => B): this.type = {
      pad = Some(() => x)
      this
    }
    /** Public functions which can be used to configure the iterator before use.
     *
     *  Select whether the last segment may be returned with less than `size`
     *  elements. If not, some elements of the original iterator may not be
     *  returned at all.
     *
     *  @param x `true` if partial segments may be returned, `false` otherwise.
     *  @return  The same iterator, and ''not'' a new iterator.
     *  @note    This method mutates the iterator it is called on, which can be safely used afterwards.
     *  @note    This method is mutually exclusive with `withPadding`.
     */
    def withPartial(x: Boolean): this.type = {
      _partial = x
      if (_partial == true) // reset pad since otherwise it will take precedence
        pad = None

      this
    }

    /** For reasons which remain to be determined, calling
     *  self.take(n).toSeq cause an infinite loop, so we have
     *  a slight variation on take for local usage.
     *  NB: self.take.toSeq is slice.toStream, lazily built on self,
     *  so a subsequent self.hasNext would not test self after the
     *  group was consumed.
     */
    private def takeDestructively(size: Int): Seq[A] = {
      val buf = new ArrayBuffer[A]
      var i = 0
      // The order of terms in the following condition is important
      // here as self.hasNext could be blocking
      while (i < size && self.hasNext) {
        buf += self.next
        i += 1
      }
      buf
    }

    private def padding(x: Int) = List.fill(x)(pad.get())
    private def gap = (step - size) max 0

    private def go(count: Int) = {
      val prevSize = buffer.size
      def isFirst = prevSize == 0
      // If there is padding defined we insert it immediately
      // so the rest of the code can be oblivious
      val xs: Seq[B] = {
        val res = takeDestructively(count)
        // was: extra checks so we don't calculate length unless there's reason
        // but since we took the group eagerly, just use the fast length
        val shortBy = count - res.length
        if (shortBy > 0 && pad.isDefined) res ++ padding(shortBy) else res
      }
      lazy val len = xs.length
      lazy val incomplete = len < count

      // if 0 elements are requested, or if the number of newly obtained
      // elements is less than the gap between sequences, we are done.
      def deliver(howMany: Int) = {
        (howMany > 0 && (isFirst || len > gap)) && {
          if (!isFirst)
            buffer trimStart (step min prevSize)

          val available =
            if (isFirst) len
            else howMany min (len - gap)

          buffer ++= (xs takeRight available)
          filled = true
          true
        }
      }

      if (xs.isEmpty) false                         // self ran out of elements
      else if (_partial) deliver(len min size)      // if _partial is true, we deliver regardless
      else if (incomplete) false                    // !_partial && incomplete means no more seqs
      else if (isFirst) deliver(len)                // first element
      else deliver(step min size)                   // the typical case
    }

    // fill() returns false if no more sequences can be produced
    private def fill(): Boolean = {
      if (!self.hasNext) false
      // the first time we grab size, but after that we grab step
      else if (buffer.isEmpty) go(size)
      else go(step)
    }

    def hasNext = filled || fill()
    def next = {
      if (!filled)
        fill()

      if (!filled)
        throw new NoSuchElementException("next on empty iterator")
      filled = false
      buffer.toList
    }
  }

  /** Returns an iterator which groups this iterator into fixed size
   *  blocks.  Example usages:
   *  {{{
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7)))
   *    (1 to 7).iterator grouped 3 toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6))
   *    (1 to 7).iterator grouped 3 withPartial false toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7, 20, 25)
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 7).iterator grouped 3 withPadding it2.next toList
   *  }}}
   *
   *  @note Reuse: $consumesAndProducesIterator
   */
  def grouped[B >: A](size: Int): GroupedIterator[B] =
    new GroupedIterator[B](self, size, size)

  /** Returns an iterator which presents a "sliding window" view of
   *  this iterator.  The first argument is the window size, and
   *  the second argument `step` is how far to advance the window
   *  on each iteration. The `step` defaults to `1`.
   *
   *  The default `GroupedIterator` can be configured to either
   *  pad a partial result to size `size` or suppress the partial
   *  result entirely.
   *
   *  Example usages:
   *  {{{
   *    // Returns List(List(1, 2, 3), List(2, 3, 4), List(3, 4, 5))
   *    (1 to 5).iterator.sliding(3).toList
   *    // Returns List(List(1, 2, 3, 4), List(4, 5))
   *    (1 to 5).iterator.sliding(4, 3).toList
   *    // Returns List(List(1, 2, 3, 4))
   *    (1 to 5).iterator.sliding(4, 3).withPartial(false).toList
   *    // Returns List(List(1, 2, 3, 4), List(4, 5, 20, 25))
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 5).iterator.sliding(4, 3).withPadding(it2.next).toList
   *  }}}
   *
   *  @return An iterator producing `Seq[B]`s of size `size`, except the
   *          last element (which may be the only element) will be truncated
   *          if there are fewer than `size` elements remaining to be grouped.
   *          This behavior can be configured.
   *
   *  @note Reuse: $consumesAndProducesIterator
   */
  def sliding[B >: A](size: Int, step: Int = 1): GroupedIterator[B] =
    new GroupedIterator[B](self, size, step)

  /** Returns the number of elements in this iterator.
   *  $willNotTerminateInf
   *
   *  @note Reuse: $consumesIterator
   */
  def length: Int = this.size

  /** Creates two new iterators that both iterate over the same elements
   *  as this iterator (in the same order).  The duplicate iterators are
   *  considered equal if they are positioned at the same element.
   *
   *  Given that most methods on iterators will make the original iterator
   *  unfit for further use, this methods provides a reliable way of calling
   *  multiple such methods on an iterator.
   *
   *  @return a pair of iterators
   *  @note   The implementation may allocate temporary storage for elements
   *          iterated by one iterator but not yet by the other.
   *  @note   Reuse: $consumesOneAndProducesTwoIterators
   */
  def duplicate: (Iterator[A], Iterator[A]) = {
    val gap = new scala.collection.mutable.Queue[A]
    var ahead: Iterator[A] = null
    class Partner extends AbstractIterator[A] {
      def hasNext: Boolean = self.synchronized {
        (this ne ahead) && !gap.isEmpty || self.hasNext
      }
      def next(): A = self.synchronized {
        if (gap.isEmpty) ahead = this
        if (this eq ahead) {
          val e = self.next()
          gap enqueue e
          e
        } else gap.dequeue()
      }
      // to verify partnerhood we use reference equality on gap because
      // type testing does not discriminate based on origin.
      private def compareGap(queue: scala.collection.mutable.Queue[A]) = gap eq queue
      override def hashCode = gap.hashCode()
      override def equals(other: Any) = other match {
        case x: Partner   => x.compareGap(gap) && gap.isEmpty
        case _            => super.equals(other)
      }
    }
    (new Partner, new Partner)
  }

  /** Returns this iterator with patched values.
   *  Patching at negative indices is the same as patching starting at 0.
   *  Patching at indices at or larger than the length of the original iterator appends the patch to the end.
   *  If more values are replaced than actually exist, the excess is ignored.
   *
   *  @param from       The start index from which to patch
   *  @param patchElems The iterator of patch values
   *  @param replaced   The number of values in the original iterator that are replaced by the patch.
   *  @note           Reuse: $consumesTwoAndProducesOneIterator
   */
  def patch[B >: A](from: Int, patchElems: Iterator[B], replaced: Int): Iterator[B] = new AbstractIterator[B] {
    private var origElems = self
    private var i = (if (from > 0) from else 0)  // Counts down, switch to patch on 0, -1 means use patch first
    def hasNext: Boolean = {
      if (i == 0) {
        origElems = origElems drop replaced
        i = -1
      }
      origElems.hasNext || patchElems.hasNext
    }
    def next(): B = {
      if (i == 0) {
        origElems = origElems drop replaced
        i = -1
      }
      if (i < 0) {
        if (patchElems.hasNext) patchElems.next()
        else origElems.next()
      }
      else {
        if (origElems.hasNext) {
          i -= 1
          origElems.next()
        }
        else {
          i = -1
          patchElems.next()
        }
      }
    }
  }

  /** Copies selected values produced by this iterator to an array.
   *  Fills the given array `xs` starting at index `start` with at most
   *  `len` values produced by this iterator.
   *  Copying will stop once either the end of the current iterator is reached,
   *  or the end of the array is reached, or `len` elements have been copied.
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   *  @tparam B      the type of the elements of the array.
   *
   *  @note    Reuse: $consumesIterator
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
   *    @inheritdoc
   *
   *    $willNotTerminateInf
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = start + math.min(len, xs.length - start)
    while (i < end && hasNext) {
      xs(i) = next()
      i += 1
    }
    // TODO: return i - start so the caller knows how many values read?
  }

  /** Tests if another iterator produces the same values as this one.
   *
   *  $willNotTerminateInf
   *
   *  @param that  the other iterator
   *  @return      `true`, if both iterators produce the same elements in the same order, `false` otherwise.
   *
   *  @note        Reuse: $consumesTwoIterators
   */
  def sameElements(that: Iterator[_]): Boolean = {
    while (hasNext && that.hasNext)
      if (next != that.next)
        return false

    !hasNext && !that.hasNext
  }

  def toTraversable: Traversable[A] = toStream
  def toIterator: Iterator[A] = self
  def toStream: Stream[A] =
    if (self.hasNext) Stream.cons(self.next(), self.toStream)
    else Stream.empty[A]


  /** Converts this iterator to a string.
   *
   *  @return `"<iterator>"`
   *           whether or not the iterator is empty.
   *  @note    Reuse: $preservesIterator
   */
  override def toString = "<iterator>"
}

/** Explicit instantiation of the `Iterator` trait to reduce class file size in subclasses. */
abstract class AbstractIterator[+A] extends Iterator[A]
