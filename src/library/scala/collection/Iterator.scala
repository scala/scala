/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import mutable.ArrayBuffer
import annotation.{ tailrec, migration }

/** The `Iterator` object provides various functions for
 *  creating specialized iterators.
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 2.8
 *  @since   2.8
 */
object Iterator {

  /** The iterator which produces no values */
  val empty = new Iterator[Nothing] {
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  /** Creates an iterator which produces a single element.
   *  '''Note:''' Equivalent, but more efficient than Iterator(elem)
   *  @param elem the element
   *  @return An iterator which produces `elem` on the first call to `next`,
   *          and which has no further elements.
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
   *  @return An iterator which produces the given elements on the
   *          first calls to `next`, and which has no further elements.
   */
  def apply[A](elems: A*): Iterator[A] = elems.iterator

  /** Creates iterator that produces the results of some element computation
   *  a number of times.
   *  @param   n  the number of elements returned by the iterator.
   *  @param   elem the element computation
   *  @return  An iterator that produces the results of `n` evaluations of `elem`.
   */
  def fill[A](len: Int)(elem: => A) = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = i < len
    def next(): A =
      if (hasNext) { i += 1; elem }
      else empty.next()
  }

  /** Creates an iterator producing the values of a given function over a range of integer values starting from 0.
   *  @param  n   The number of elements returned by the iterator
   *  @param  f   The function computing element values
   *  @return An iterator that produces the values `f(0), ..., f(n -1)`.
   */
  def tabulate[A](end: Int)(f: Int => A) = new Iterator[A] {
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
  def range(start: Int, end: Int, step: Int) = new Iterator[Int] {
    if (step == 0) throw new IllegalArgumentException("zero step")
    private var i = start
    def hasNext: Boolean = (step <= 0 || i < end) && (step >= 0 || i > end)
    def next(): Int =
      if (hasNext) { val result = i; i += step; result }
      else empty.next()
  }

  /** Creates an infinite iterator that repeatedly applies a given function to the previous result.
   *
   *  @param start the start value of the iterator
   *  @param f     the function that's repeatedly applied
   *  @return      the iterator producing the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[T](start: T)(f: T => T): Iterator[T] = new Iterator[T] {
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
  def from(start: Int, step: Int): Iterator[Int] = new Iterator[Int] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Int = { val result = i; i += step; result }
  }

  /** Creates an infinite-length iterator returning the results of evaluating
   *  an expression. The expression is recomputed for every element.
   *
   *  @param elem the element computation.
   *  @return the iterator containing an infinite number of results of evaluating `elem`.
   */
  def continually[A](elem: => A): Iterator[A] = new Iterator[A] {
    def hasNext = true
    def next = elem
  }

  /** With the advent of TraversableOnce, it can be useful to have a builder
   *  for Iterators so they can be treated uniformly along with the collections.
   *  See scala.util.Random.shuffle for an example.
   */
  class IteratorCanBuildFrom[A] extends generic.CanBuildFrom[Iterator[A], A, Iterator[A]] {
    def newIterator = new ArrayBuffer[A] mapResult (_.iterator)

    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: Iterator[A]) = newIterator

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newIterator
  }

  implicit def iteratorCanBuildFrom[T]: IteratorCanBuildFrom[T] = new IteratorCanBuildFrom[T]

  /** A wrapper class for the `flatten` method that is added to
   *  class `Iterator` with implicit conversion
   *  @see iteratorIteratorWrapper.
   */
  class IteratorIteratorOps[A](its: Iterator[Iterator[A]]) {
    /** If `its` is an iterator of iterators, `its.flatten` gives the iterator
     *  that is the concatenation of all iterators in `its`.
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

  /** @param xs the array of elements
   *  @see also: IndexedSeq.iterator and slice
   */
  @deprecated("use `xs.iterator' instead")
  def fromArray[a](xs: Array[a]): Iterator[a] =
    fromArray(xs, 0, xs.length)

  /**
   *  @param xs     the array of elements
   *  @param start  the start index
   *  @param length  the length
   *  @see also: IndexedSeq.iterator and slice
   */
  @deprecated("use `xs.slice(start, start + length).iterator' instead")
  def fromArray[a](xs: Array[a], start: Int, length: Int): Iterator[a] =
    xs.slice(start, start + length).iterator

  /**
   *  @param n the product arity
   *  @return  the iterator on `Product&lt;n&gt;`.
   */
  @deprecated("use product.productIterator instead")
  def fromProduct(n: Product): Iterator[Any] = new Iterator[Any] {
    private var c: Int = 0
    private val cmax = n.productArity
    def hasNext = c < cmax
    def next() = { val a = n productElement c; c += 1; a }
  }

  /** Create an iterator with elements
   *  `e<sub>n+1</sub> = step(e<sub>n</sub>)`
   *  where `e<sub>0</sub> = start`
   *  and elements are in the range between `start` (inclusive)
   *  and `end` (exclusive)
   *
   *  @param start the start value of the iterator
   *  @param end   the end value of the iterator
   *  @param step  the increment function of the iterator, must be monotonically increasing or decreasing
   *  @return      the iterator with values in range `[start;end)`.
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
   *  `e<sub>n+1</sub> = step(e<sub>n</sub>)`
   *  where `e<sub>0</sub> = start`.
   *
   *  @param start the start value of the iterator
   *  @param step  the increment function of the iterator
   *  @return      the iterator starting at value `start`.
   */
  @deprecated("use iterate(start)(step) instead")
  def from(start: Int, step: Int => Int): Iterator[Int] = new Iterator[Int] {
    private var i = start
    override def hasNext: Boolean = true
    def next(): Int = { val j = i; i = step(i); j }
  }

  /** Create an iterator that is the concatenation of all iterators
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
 *  of elements. They have a `hasNext` method for checking
 *  if there is a next element available, and a `next` method
 *  which returns the next element and discards it from the iterator.
 *
 *  @author  Martin Odersky, Matthias Zenger
 *  @version 2.8
 *  @since   1
 *  @define willNotTerminateInf
 *  Note: will not terminate for infinite iterators.
 *  @define mayNotTerminateInf
 *  Note: may not terminate for infinite iterators.
 */
trait Iterator[+A] extends TraversableOnce[A] {
  self =>

  /** Tests whether this iterator can provide another element.
   *  @return  `true` if a subsequent call to `next` will yield an element,
   *           `false` otherwise.
   */
  def hasNext: Boolean

  /** Produces the next element of this iterator.
   *  @return  the next element of this iterator, if `hasNext` is `true`,
   *           undefined behavior otherwise.
   */
  def next(): A

  /** Tests whether this iterator is empty.
   *  @return   `true` if hasNext is false, `false` otherwise.
   */
  def isEmpty: Boolean = !hasNext

  /** Tests whether this Iterator can be repeatedly traversed.
   *  @return   `false`
   */
  def isTraversableAgain = false

  /** Tests whether this Iterator has a known size.
   *
   *  @return   `true` for empty Iterators, `false` otherwise.
   */
  def hasDefiniteSize = isEmpty

  /** Selects first ''n'' values of this iterator.
   *  @param  n    the number of values to take
   *  @return an iterator producing only of the first `n` values of this iterator, or else the
   *          whole iterator, if it produces less than `n` values.
   */
  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var remaining = n
    def hasNext = remaining > 0 && self.hasNext
    def next(): A =
      if (hasNext) { remaining -= 1; self.next() }
      else empty.next()
  }

  /** Advances this iterator past the first ''n'' elements,
   *  or the length of the iterator, whichever is smaller.
   *
   *  @param n the number of elements to drop
   *  @return  an iterator which produces all values of the current iterator, except
   *           it omits the first `n` values.
   */
  def drop(n: Int): Iterator[A] = {
    @tailrec
    def loop(left: Int): Iterator[A] =
      if (left > 0 && hasNext) { next; loop(left - 1) }
      else this

    loop(n)
  }

  /** Creates an iterator returning an interval of the values produced by this iterator.
   *  @param from   the index of the first element in this iterator which forms part of the slice.
   *  @param until  the index of the first element following the slice.
   *  @return an iterator which advances this iterator past the first `from` elements using `drop`,
   *  and then takes `until - from` elements, using `take`.
   */
  def slice(from: Int, until: Int): Iterator[A] = drop(from).take(until - from)

  /** Creates a new iterator that maps all produced values of this iterator
   *  to new values using a transformation function.
   *  @param f  the transformation function
   *  @return a new iterator which transforms every value produced by this
   *          iterator by applying the function `f` to it.
   */
  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  /** Concatenates this iterator with another.
   *  @param   that   the other iterator
   *  @return  a new iterator that first yields the values produced by this
   *  iterator followed by the values produced by iterator `that`.
   *  @usecase def ++(that: => Iterator[A]): Iterator[A]
   */
  def ++[B >: A](that: => Iterator[B]): Iterator[B] = new Iterator[B] {
    // optimize a little bit to prevent n log n behavior.
    private var cur : Iterator[B] = self
    // since that is by-name, make sure it's only referenced once -
    // if "val it = that" is inside the block, then hasNext on an empty
    // iterator will continually reevaluate it.  (ticket #3269)
    lazy val it = that
    // the eq check is to avoid an infinite loop on "x ++ x"
    def hasNext = cur.hasNext || ((cur eq self) && {
      it.hasNext && {
        cur = it
        true
      }
    })
    def next() = { hasNext; cur.next() }
  }

  /** Creates a new iterator by applying a function to all values produced by this iterator
   *  and concatenating the results.
   *
   *  @param f the function to apply on each element.
   *  @return   the iterator resulting from applying the given iterator-valued function
   *                `f` to each value produced by this iterator and concatenating the results.
   */
  def flatMap[B](f: A => Iterator[B]): Iterator[B] = new Iterator[B] {
    private var cur: Iterator[B] = empty
    def hasNext: Boolean =
      cur.hasNext || self.hasNext && { cur = f(self.next); hasNext }
    def next(): B = (if (hasNext) cur else empty).next()
  }

  /** Returns an iterator over all the elements of this iterator that
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   */
  def filter(p: A => Boolean): Iterator[A] = new Iterator[A] {
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

  /** Creates an iterator over all the elements of this iterator that
   *  satisfy the predicate `p`. The order of the elements
   *  is preserved.
   *
   *  '''Note:''' `withFilter` is the same as `filter` on iterators. It exists so that
   *  for-expressions with filters work over iterators.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which satisfy the predicate `p`.
   */
  def withFilter(p: A => Boolean): Iterator[A] = filter(p)

  /** Creates an iterator over all the elements of this iterator which
   *  do not satisfy a predicate p.
   *
   *  @param p the predicate used to test values.
   *  @return  an iterator which produces those values of this iterator which do not satisfy the predicate `p`.
   */
  def filterNot(p: A => Boolean): Iterator[A] = filter(!p(_))

 /** Creates an iterator by transforming values
  *  produced by this iterator with a partial function, dropping those
  *  values for which the partial function is not defined.
  *  @param pf the partial function which filters and maps the iterator.
  *  @return a new iterator which yields each value `x` produced by this iterator for
  *          which `pf` is defined the image `pf(x)`.
  */
  @migration(2, 8,
    "This collect implementation bears no relationship to the one before 2.8.\n"+
    "The previous behavior can be reproduced with toSeq."
  )
  def collect[B](pf: PartialFunction[A, B]): Iterator[B] = {
    val self = buffered
    new Iterator[B] {
      private def skip() = while (self.hasNext && !pf.isDefinedAt(self.head)) self.next()
      def hasNext = { skip(); self.hasNext }
      def next() = { skip(); pf(self.next()) }
    }
  }

  /** Takes longest prefix of values produced by this iterator that satisfy a predicate.
   *  @param   p  The predicate used to test elements.
   *  @return  An iterator returning the values produced by this iterator, until
   *           this iterator produces a value that does not satisfy
   *           the predicate `p`.
   */
  def takeWhile(p: A => Boolean): Iterator[A] = new Iterator[A] {
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
   *  predicate `p`, and returns an iterator of the remaining elements.
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

  /** Creates an iterator formed from this iterator and another iterator
   *  by combining corresponding values in pairs.
   *  If one of the two iterators is longer than the other, its remaining
   *  elements are ignored.
   *  @param   that  The iterator providing the second half of each result pair
   *  @return        a new iterator containing pairs consisting of
   *                 corresponding elements of this iterator and `that`. The number
   *                 of elements returned by the new iterator is the
   *                 minimum of the number of elements returned by this
   *                 iterator and `that`.
   */
  def zip[B](that: Iterator[B]) = new Iterator[(A, B)] {
    def hasNext = self.hasNext && that.hasNext
    def next = (self.next, that.next)
  }

  /** Appends an element value to this iterator until a given target length is reached.
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @return a new iterator consisting of producing all values of this iterator,
   *          followed by the minimal number of occurrences of `elem` so
   *          that the number of produced values is at least `len`.
   *  @usecase def padTo(len: Int, elem: A): Iterator[A]
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

  /** Creates an iterator that pairs each element produced by this iterator
   *  with its index, counting from 0.
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
   *  @usecase def zipAll[B](that: Iterator[B], thisElem: A, thatElem: B): Iterator[(A, B)]
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

  /** Applies a function `f` to all values produced by this iterator.
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   */
  def foreach[U](f: A =>  U) { while (hasNext) f(next()) }

  /** Tests whether a predicate holds for all values produced by this iterator.
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for all values
   *                 produced by this iterator, otherwise `false`.
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
   *               is equal (wrt `==`) to `elem`, `false` otherwise.
   */
  def contains(elem: Any): Boolean = exists(_ == elem)

  /** Finds the first value produced by the iterator satisfying a
   *  predicate, if any.
   *  $mayNotTerminateInf
   *
   *  @param p the predicate used to test values.
   *  @return  an option value containing the first value produced by the iterator that satisfies
   *           predicate `p`, or `None` if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var res: Option[A] = None
    while (res.isEmpty && hasNext) {
      val e = next()
      if (p(e)) res = Some(e)
    }
    res
  }

  /** Applies option-valued function to successive elements of this iterator
   *  until a defined value is found.
   *
   *  @param f    the function to be applied to successive elements.
   *  @return     an option value containing the first defined result of
   *              `f`, or `None` if `f` returns `None` for all all elements.
  def mapFind[B](f: A => Option[B]): Option[B] = {
    var res: Option[B] = None
    while (res.isEmpty && hasNext) {
      res = f(next())
    }
    res
  }
   */

  /** Returns the index of the first produced value satisfying a predicate, or -1.
   *  $mayNotTerminateInf
   *  @param  p the predicate to test values
   *  @return   the index of the first produced value satisfying `p`,
   *           or -1 if such an element does not exist until the end of the iterator is reached.
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

  /** Returns the index of the first occurrence of the specified
   *  object in this iterable object.
   *  $mayNotTerminateInf
   *
   *  @param  elem  element to search for.
   *  @return the index of the first occurrence of `elem` in the values produced by this iterator,
   *          or -1 if such an element does not exist until the end of the iterator is reached.
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

  /** Creates a buffered iterator from this iterator.
   *  @see BufferedIterator
   *  @return  a buffered iterator producing the same values as this iterator.
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

  /** A flexible iterator for transforming an `Iterator[A]` into an
   *  Iterator[Seq[A]], with configurable sequence size, step, and
   *  strategy for dealing with elements which don't fit evenly.
   *
   *  Typical uses can be achieved via methods `grouped' and `sliding'.
   */
  class GroupedIterator[B >: A](self: Iterator[A], size: Int, step: Int) extends Iterator[Seq[B]] {
    require(size >= 1 && step >= 1)

    private[this] var buffer: ArrayBuffer[B] = ArrayBuffer()  // the buffer
    private[this] var filled = false                          // whether the buffer is "hot"
    private[this] var _partial = true                         // whether we deliver short sequences
    private[this] var pad: Option[() => B] = None             // what to pad short sequences with

    /** Public functions which can be used to configure the iterator before use. */
    def withPadding(x: => B): this.type = {
      pad = Some(() => x)
      this
    }
    def withPartial(x: Boolean): this.type = {
      _partial = x
      if (_partial == true) // reset pad since otherwise it will take precedence
        pad = None

      this
    }

    /** For reasons which remain to be determined, calling
     *  self.take(n).toSeq cause an infinite loop, so we have
     *  a slight variation on take for local usage.
     */
    private def takeDestructively(size: Int): Seq[A] = {
      val buf = new ArrayBuffer[A]
      var i = 0
      while (self.hasNext && i < size) {
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
        // extra checks so we don't calculate length unless there's reason
        if (pad.isDefined && !self.hasNext) {
          val shortBy = count - res.length
          if (shortBy > 0) res ++ padding(shortBy) else res
        }
        else res
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
   *
   *  <pre>
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7)))
   *    (1 to 7).iterator grouped 3 toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6))
   *    (1 to 7).iterator grouped 3 withPartial false toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7, 20, 25)
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 7).iterator grouped 3 withPadding it2.next toList
   *  </pre>
   */
  def grouped[B >: A](size: Int): GroupedIterator[B] =
    new GroupedIterator[B](self, size, size)

  /** Returns an iterator which presents a "sliding window" view of
   *  another iterator.  The first argument is the window size, and
   *  the second is how far to advance the window on each iteration;
   *  defaults to 1.  Example usages:
   *
   *  <pre>
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
   *  </pre>
   */
  def sliding[B >: A](size: Int, step: Int = 1): GroupedIterator[B] =
    new GroupedIterator[B](self, size, step)

  /** Returns the number of elements in this iterator.
   *  $willNotTerminateInf
   *
   *  Note: The iterator is at its end after this method returns.
   */
  def length: Int = this.size

  /** Creates two new iterators that both iterate over the same elements
   *  as this iterator (in the same order).  The duplicate iterators are
   *  considered equal if they are positioned at the same element.
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
      // to verify partnerhood we use reference equality on gap because
      // type testing does not discriminate based on origin.
      private def compareGap(queue: scala.collection.mutable.Queue[A]) = gap eq queue
      override def hashCode = gap.hashCode
      override def equals(other: Any) = other match {
        case x: Partner   => x.compareGap(gap) && gap.isEmpty
        case _            => super.equals(other)
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

  /** Copies selected values produced by this iterator to an array.
   *  Fills the given array `xs` with at most `len` values produced by this
   *  iterator, after skipping `start` values.
   *  Copying will stop once either the end of the current iterator is reached,
   *  or the end of the array is reached, or `len` elements have been copied.
   *
   *  $willNotTerminateInf
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   *  @tparam B      the type of the elements of the array.
   *
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
    var i = start
    val end = start + len min xs.length
    while (hasNext && i < end) {
      xs(i) = next()
      i += 1
    }
  }

  /** Tests if another iterator produces the same values as this one.
   *  $willNotTerminateInf
   *  @param that  the other iterator
   *  @return `true`, if both iterators produce the same elements in the same order, `false` otherwise.
   */
  def sameElements(that: Iterator[_]): Boolean = {
    while (hasNext && that.hasNext)
      if (next != that.next)
        return false

    !hasNext && !that.hasNext
  }

  def toTraversable: Traversable[A] = toStream
  def toIterator: Iterator[A] = self

  /** Converts this iterator to a string.
   *  @return `"empty iterator"` or `"non-empty iterator"`, depending on whether or not the iterator is empty.
   */
  override def toString = (if (hasNext) "non-empty" else "empty")+" iterator"

  /** Returns a new iterator that first yields the elements of this
   *  iterator followed by the elements provided by iterator `that`.
   */
  @deprecated("use `++`")
  def append[B >: A](that: Iterator[B]) = self ++ that

  /** Returns index of the first element satisfying a predicate, or -1. */
  @deprecated("use `indexWhere` instead")
  def findIndexOf(p: A => Boolean): Int = indexWhere(p)

  /** Returns a counted iterator from this iterator.
   */
  @deprecated("use zipWithIndex in Iterator")
  def counted = new CountedIterator[A] {
    private var cnt = 0
    def count = cnt
    def hasNext: Boolean = self.hasNext
    def next(): A = { cnt += 1; self.next }
  }

  /** Fills the given array `xs` with the elements of
   *  this sequence starting at position `start`.  Like `copyToArray`,
   *  but designed to accomodate IO stream operations.
   *
   *  '''Note:'''   the array must be large enough to hold `sz` elements.
   *  @param  xs    the array to fill.
   *  @param  start the starting index.
   *  @param  sz    the maximum number of elements to be read.
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
