package strawman.collection

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.{Any, AnyRef, Array, Boolean, IllegalArgumentException, Int, NoSuchElementException, None, Nothing, Numeric, Option, Ordering, PartialFunction, Some, StringContext, Unit, UnsupportedOperationException, `inline`, math, throws}
import scala.Predef.{identity, intWrapper, require, String}
import strawman.collection.mutable.{ArrayBuffer, StringBuilder}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

/** A core Iterator class
  *
  * @define consumesIterator
  * After calling this method, one should discard the iterator it was called
  * on. Using it is undefined and subject to change.
  *
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
  *  @define undefinedorder
  *  The order in which operations are performed on elements is unspecified
  *  and may be nondeterministic.
  *  @define coll iterator
  */
trait Iterator[+A] extends IterableOnce[A] { self =>
  def hasNext: Boolean
  @throws[NoSuchElementException]
  def next(): A
  def iterator() = this

  /** @return the ''remaining'' number of elements, if it can be computed in O(1) time, otherwise -1 */
  def knownSize: Int = -1

  /** Tests whether this iterator is empty.
    *
    *  @return   `true` if hasNext is false, `false` otherwise.
    *  @note     Reuse: $preservesIterator
    */
  def isEmpty: Boolean = !hasNext

  /** Tests whether this iterator is not empty, same as `hasNext`.
    *
    *  @return   `true` if hasNext is true, `false` otherwise.
    *  @note     Reuse: $preservesIterator
    */
  @`inline` final def nonEmpty: Boolean = hasNext

  /** Wraps the value of `next()` in an option.
    *
    * @return `Some(next)` if a next element exists, `None` otherwise.
    */
  def nextOption(): Option[A] = if (hasNext) Some(next()) else None

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

  /** Counts the number of elements in the $coll which satisfy a predicate.
    *
    *  @param p     the predicate  used to test elements.
    *  @return      the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Int = {
    var res = 0
    while (hasNext) if (p(next())) res += 1
    res
  }

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

  /** Finds the first element of the iterator for which the given partial
    *  function is defined, and applies the partial function to it.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param pf   the partial function
    *  @return     an option value containing pf applied to the first
    *              value for which it is defined, or `None` if none exists.
    *  @example    `Seq("a", 1, 5L).iterator().collectFirst({ case x: Int => x*10 }) = Some(10)`
    */
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself
    // (Tested to be lower-overhead than runWith.  Would be better yet to not need to (formally) allocate it)
    val sentinel: scala.Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any]{ def apply(a: A) = this }
    while (hasNext) {
      val x = pf.applyOrElse(next(), sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) return Some(x.asInstanceOf[B])
    }
    None
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
  class GroupedIterator[B >: A](self: Iterator[B], size: Int, step: Int) extends Iterator[Seq[B]] {

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
    private def takeDestructively(size: Int): Seq[B] = {
      val buf = new ArrayBuffer[B]
      var i = 0
      // The order of terms in the following condition is important
      // here as self.hasNext could be blocking
      while (i < size && self.hasNext) {
        buf += self.next()
        i += 1
      }
      buf
    }

    private def padding(x: Int) = immutable.ImmutableArray.fill(x)(pad.get())
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
            buffer dropInPlace (step min prevSize)

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
    @throws[NoSuchElementException]
    def next() = {
      if (!filled)
        fill()

      if (!filled)
        throw new NoSuchElementException("next on empty iterator")
      filled = false
      immutable.ImmutableArray.fromArrayBuffer(buffer)
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

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    while (hasNext) {
      result = op(result, next())
    }
    result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B = {
    var reversed: immutable.List[A] = immutable.Nil
    while (hasNext) reversed = next() :: reversed
    reversed.foldLeft(z)((b, a) => op(a, b))
  }
  
  /** Produces a collection containing cumulative results of applying the
   *  operator going left to right.
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
  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B] = new Iterator[B] {
    // We use an intermediate iterator that iterates through the first element `z`
    // and then that will be modified to iterate through the collection
    private var current: Iterator[B] =
      new Iterator[B] {
        def hasNext: Boolean = true
        def next(): B = {
          // Here we change our self-reference to a new iterator that iterates through `self`
          current = new Iterator[B] {
            private var acc = z
            def next(): B = {
              acc = op(acc, self.next())
              acc
            }
            def hasNext: Boolean = self.hasNext
          }
          z
        }
      }
    def next(): B = current.next()
    def hasNext: Boolean = current.hasNext
  }

  def foreach[U](f: A => U): Unit =
    while (hasNext) f(next())

  def indexWhere(p: A => Boolean, from: Int = 0): Int = {
    var i = math.max(from, 0)
    drop(from)
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

  /** Reduces the elements of this $coll using the specified associative binary operator.
    *
    *  $undefinedorder
    *
    *  @tparam B      A type parameter for the binary operator, a supertype of `A`.
    *  @param op       A binary operator that must be associative.
    *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
    *  @throws UnsupportedOperationException
    *  if this $coll is empty.
    */
  def reduce[B >: A](op: (B, B) => B): B = reduceLeft(op)

  /** Reduces the elements of this $coll, if any, using the specified
    *  associative binary operator.
    *
    *  $undefinedorder
    *
    *  @tparam B     A type parameter for the binary operator, a supertype of `A`.
    *  @param op      A binary operator that must be associative.
    *  @return        An option value containing result of applying reduce operator `op` between all
    *                 the elements if the collection is nonempty, and `None` otherwise.
    */
  def reduceOption[B >: A](op: (B, B) => B): Option[B] = reduceLeftOption(op)

  /** Applies a binary operator to all elements of this $coll,
    *  going left to right.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going left to right:
    *           {{{
    *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

    while (hasNext) {
      val x = next()
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

  // For internal use
  protected[this] def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = iterator()
    while (it.hasNext) xs = it.next() :: xs
    xs
  }

  /** Applies a binary operator to all elements of this $coll, going right to left.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left:
    *           {{{
    *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.
    */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceRight")

    reversed.reduceLeft[B]((x, y) => op(y, x))
  }

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  an option value containing the result of `reduceLeft(op)` if this $coll is nonempty,
    *           `None` otherwise.
    */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = if (isEmpty) None else Some(reduceLeft(op))

  /** Optionally applies a binary operator to all elements of this $coll, going
    *  right to left.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  an option value containing the result of `reduceRight(op)` if this $coll is nonempty,
    *           `None` otherwise.
    */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] = if (isEmpty) None else Some(reduceRight(op))

  /** Sums up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `+` operator to be used in forming the sum.
    *   @tparam  B    the result type of the `+` operator.
    *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
    *
    *   @usecase def sum: A
    *     @inheritdoc
    *
    *     @return       the sum of all elements in this $coll of numbers of type `Int`.
    *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
    *     can be used as element type of the $coll and as result type of `sum`.
    *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
    *
    */
  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  /** Multiplies up the elements of this collection.
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `*` operator to be used in forming the product.
    *   @tparam  B   the result type of the `*` operator.
    *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
    *
    *   @usecase def product: A
    *     @inheritdoc
    *
    *     @return       the product of all elements in this $coll of numbers of type `Int`.
    *     Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
    *     can be used as element type of the $coll and as result type of `product`.
    *     Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
    */
  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  /** Finds the smallest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   the smallest element of this $coll with respect to the ordering `ord`.
    *
    *  @usecase def min: A
    *    @inheritdoc
    *
    *    @return   the smallest element of this $coll
    */
  def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")

    reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)
  }

  /** Finds the largest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   the largest element of this $coll with respect to the ordering `ord`.
    *
    *  @usecase def max: A
    *    @inheritdoc
    *
    *    @return   the largest element of this $coll.
    */
  def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")

    reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)
  }

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the largest value measured by function f
    *  with respect to the ordering `cmp`.
    *
    *  @usecase def maxBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the largest value measured by function f.
    */
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    var maxF: B = null.asInstanceOf[B]
    var maxElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.gt(fx, maxF)) {
        maxElem = elem
        maxF = fx
        first = false
      }
    }
    maxElem
  }

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the smallest value measured by function f
    *  with respect to the ordering `cmp`.
    *
    *  @usecase def minBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the smallest value measured by function f.
    */
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    var minF: B = null.asInstanceOf[B]
    var minElem: A = null.asInstanceOf[A]
    var first = true

    for (elem <- this) {
      val fx = f(elem)
      if (first || cmp.lt(fx, minF)) {
        minElem = elem
        minF = fx
        first = false
      }
    }
    minElem
  }

  def length: Int = {
    var len = 0
    while (hasNext) { len += 1; next() }
    len
  }

  final def size: Int = length
  
  /** Selects all elements of this iterator which satisfy a predicate.
    *
    *  @param p     the predicate used to test elements.
    *  @return      a new iterator consisting of all elements of this $coll that satisfy the given
    *               predicate `p`. The order of the elements is preserved.
    */
  def filter(p: A => Boolean): Iterator[A] = filterImpl(p, isFlipped = false)

  /** Creates an iterator over all the elements of this iterator which
    *  do not satisfy a predicate p.
    *
    *  @param p the predicate used to test values.
    *  @return  an iterator which produces those values of this iterator which do not satisfy the predicate `p`.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def filterNot(p: A => Boolean): Iterator[A] = filterImpl(p, isFlipped = true)

  private[collection] def filterImpl(p: A => Boolean, isFlipped: Boolean): Iterator[A] = new Iterator[A] {
    private var hd: A = _
    private var hdDefined: Boolean = false

    def hasNext: Boolean = hdDefined || {
      do {
        if (!self.hasNext) return false
        hd = self.next()
      } while (p(hd) == isFlipped)
      hdDefined = true
      true
    }

    def next() =
      if (hasNext) {
        hdDefined = false
        hd
      }
      else Iterator.empty.next()
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

  /** Creates an iterator by transforming values
    *  produced by this iterator with a partial function, dropping those
    *  values for which the partial function is not defined.
    *
    *  @param pf the partial function which filters and maps the iterator.
    *  @return   a new iterator which yields each value `x` produced by this iterator for
    *  which `pf` is defined the image `pf(x)`.
    *  @note     Reuse: $consumesAndProducesIterator
    */
  def collect[B](pf: PartialFunction[A, B]): Iterator[B] = new Iterator[B] {
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

  /**
    *  Builds a new iterator from this one without any duplicated elements on it.
    *  @return iterator with distinct elements
    *
    *  @note   Reuse: $consumesIterator
    */
  def distinct: Iterator[A] = distinctBy(identity)

  /**
    *  Builds a new iterator from this one without any duplicated elements as determined by `==` after applying
    *  the transforming function `f`.
    *
    *  @param f The transforming function whose result is used to determine the uniqueness of each element
    *  @tparam B the type of the elements after being transformed by `f`
    *  @return iterator with distinct elements
    *
    *  @note   Reuse: $consumesIterator
    */
  def distinctBy[B](f: A => B): Iterator[A] = new Iterator[A] {

    private val traversedValues = mutable.HashSet.empty[B]
    private var nextElementDefined: Boolean = false
    private var nextElement: A = _

    def hasNext: Boolean = {
      @tailrec
      def loop(): Boolean = {
        if (!self.hasNext) false
        else {
          nextElement = self.next()
          val y = f(nextElement)
          if (traversedValues.contains(y)) {
            loop()
          } else {
            traversedValues += y
            nextElementDefined = true
            true
          }
        }
      }

      nextElementDefined || loop()
    }

    def next(): A =
      if (hasNext) {
        nextElementDefined = false
        nextElement
      } else {
        Iterator.empty.next()
      }
  }

  def map[B](f: A => B): Iterator[B] = new Iterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new Iterator[B] {
    private var myCurrent: Iterator[B] = Iterator.empty
    private def current = {
      while (!myCurrent.hasNext && self.hasNext)
        myCurrent = f(self.next()).iterator()
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def flatten[B](implicit ev: A => IterableOnce[B]): Iterator[B] =
    flatMap[B](ev)

  def concat[B >: A](xs: => IterableOnce[B]): Iterator[B] = new Iterator.ConcatIterator[B](self).concat(xs)

  @`inline` def ++ [B >: A](xs: => IterableOnce[B]): Iterator[B] = concat(xs)

  def take(n: Int): Iterator[A] = new Iterator[A] {
    private var i = 0
    def hasNext = self.hasNext && i < n
    def next() =
      if (hasNext) {
        i += 1
        self.next()
      }
      else Iterator.empty.next()
  }

  /** Takes longest prefix of values produced by this iterator that satisfy a predicate.
    *
    *  @param   p  The predicate used to test elements.
    *  @return  An iterator returning the values produced by this iterator, until
    *           this iterator produces a value that does not satisfy
    *           the predicate `p`.
    *  @note    Reuse: $consumesAndProducesIterator
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
    def next() = if (hasNext) { hdDefined = false; hd } else Iterator.empty.next()
  }

  def drop(n: Int): Iterator[A] = {
    var i = 0
    while (i < n && hasNext) {
      next()
      i += 1
    }
    this
  }

  /** Skips longest sequence of elements of this iterator which satisfy given
    *  predicate `p`, and returns an iterator of the remaining elements.
    *
    *  @param p the predicate used to skip elements.
    *  @return  an iterator consisting of the remaining elements
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def dropWhile(p: A => Boolean): Iterator[A] = new Iterator[A] {
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

    if (rest == 0) Iterator.empty
    else new Iterator.SliceIterator(this, lo, rest)
  }

  def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
    val thatIterator = that.iterator()
    def hasNext = self.hasNext && thatIterator.hasNext
    def next() = (self.next(), thatIterator.next())
  }

  def zipAll[A1 >: A, B](that: IterableOnce[B], thisElem: A1, thatElem: B): Iterator[(A1, B)] = new Iterator[(A1, B)] {
    val thatIterator = that.iterator()
    def hasNext = self.hasNext || thatIterator.hasNext
    def next(): (A1, B) = {
      val next1 = self.hasNext
      val next2 = thatIterator.hasNext
      if(!(next1 || next2)) throw new NoSuchElementException
      (if(next1) self.next() else thisElem, if(next2) thatIterator.next() else thatElem)
    }
  }

  /** Creates an iterator that pairs each element produced by this iterator
    *  with its index, counting from 0.
    *
    *  @return        a new iterator containing pairs consisting of
    *                 corresponding elements of this iterator and their indices.
    *  @note          Reuse: $consumesAndProducesIterator
    */
  def zipWithIndex: Iterator[(A, Int)] = new Iterator[(A, Int)] {
    var idx = 0
    def hasNext = self.hasNext
    def next() = {
      val ret = (self.next(), idx)
      idx += 1
      ret
    }
  }

  def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    val those = that.iterator()
    while (hasNext && those.hasNext)
      if (next() != those.next())
        return false
    // At that point we know that *at least one* iterator has no next element
    // If *both* of them have no elements then the collections are the same
    hasNext == those.hasNext
  }

  /** Returns this iterator with patched values.
    * Patching at negative indices is the same as patching starting at 0.
    * Patching at indices at or larger than the length of the original iterator appends the patch to the end.
    * If more values are replaced than actually exist, the excess is ignored.
    *
    *  @param from       The start index from which to patch
    *  @param patchElems The iterator of patch values
    *  @param replaced   The number of values in the original iterator that are replaced by the patch.
    *  @note           Reuse: $consumesTwoAndProducesOneIterator
    */
  def patch[B >: A](from: Int, patchElems: Iterator[B], replaced: Int): Iterator[B] =
    new Iterator[B] {
      private var origElems = self
      private var i = if (from > 0) from else 0 // Counts down, switch to patch on 0, -1 means use patch first
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

  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  def mkString(sep: String): String = mkString("", sep, "")

  def mkString: String = mkString("")

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
    *  The written text begins with the string `start` and ends with the string `end`.
    *  Inside, the string representations (w.r.t. the method `toString`)
    *  of all elements of this $coll are separated by the string `sep`.
    *
    * Example:
    *
    * {{{
    *      scala> val a = List(1,2,3,4)
    *      a: List[Int] = List(1, 2, 3, 4)
    *
    *      scala> val b = new StringBuilder()
    *      b: StringBuilder =
    *
    *      scala> a.addString(b , "List(" , ", " , ")")
    *      res5: StringBuilder = List(1, 2, 3, 4)
    * }}}
    *
    *  @param  b    the string builder to which elements are appended.
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      the string builder `b` to which elements were appended.
    */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true

    b addAll start
    for (x <- self) {
      if (first) {
        b addAll x.toString
        first = false
      }
      else {
        b addAll sep
        b addAll x.toString
      }
    }
    b addAll end

    b
  }

  /** Converts this Iterator into another collection.
    *  @return a new collection containing all elements of this Iterator.
    *  @tparam C The collection type to build.
    *  @param factory Collection factory to use. The factory may or may
    *                  not eagerly consume this iterator.
    */
  def to[C](factory: Factory[A, C]): C = factory.fromSpecific(self)

}

object Iterator {

  val empty: Iterator[Nothing] = new Iterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
  }

  def single[A](a: A): Iterator[A] = new Iterator[A] {
    private var consumed: Boolean = false
    def hasNext = !consumed
    def next() = if (consumed) empty.next() else { consumed = true; a }
  }

  def apply[A](xs: A*): Iterator[A] = new IndexedView[A] {
    val length = xs.length
    def apply(n: Int) = xs(n)
  }.iterator()

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
  def range(start: Int, end: Int, step: Int): Iterator[Int] = new Iterator[Int] {
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

  /** Creates an infinite-length iterator returning the results of evaluating an expression.
    *  The expression is recomputed for every element.
    *
    *  @param elem the element computation.
    *  @return the iterator containing an infinite number of results of evaluating `elem`.
    */
  def continually[A](elem: => A): Iterator[A] = new Iterator[A] {
    def hasNext = true
    def next() = elem
  }

  /** Creates an iterator to which other iterators can be appended efficiently.
    *  Nested ConcatIterators are merged to avoid blowing the stack.
    */
  private final class ConcatIterator[+A](private var current: Iterator[A @uncheckedVariance]) extends Iterator[A] {
    private var tail: ConcatIteratorCell[A @uncheckedVariance] = null
    private var last: ConcatIteratorCell[A @uncheckedVariance] = null
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
        else if (current.hasNext) {
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

    override def concat[B >: A](that: => IterableOnce[B]): Iterator[B] = {
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

  private[this] final class ConcatIteratorCell[A](head: => IterableOnce[A], var tail: ConcatIteratorCell[A]) {
    def headIterator: Iterator[A] = head.iterator()
  }

  /** Creates a delegating iterator capped by a limit count. Negative limit means unbounded.
    *  Lazily skip to start on first evaluation.  Avoids daisy-chained iterators due to slicing.
    */
  private[strawman] final class SliceIterator[A](val underlying: Iterator[A], start: Int, limit: Int) extends AbstractIterator[A] {
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

/** Explicit instantiation of the `Iterator` trait to reduce class file size in subclasses. */
abstract class AbstractIterator[+A] extends Iterator[A]
