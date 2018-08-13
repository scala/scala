package scala.collection

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.{ArrayBuffer, Builder, ImmutableBuilder}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.runtime.Statics

/** Iterators are data structures that allow to iterate over a sequence
  * of elements. They have a `hasNext` method for checking
  * if there is a next element available, and a `next` method
  * which returns the next element and advances the iterator.
  *
  * An iterator is mutable: most operations on it change its state. While it is often used
  * to iterate through the elements of a collection, it can also be used without
  * being backed by any collection (see constructors on the companion object).
  *
  * It is of particular importance to note that, unless stated otherwise, ''one should never
  * use an iterator after calling a method on it''. The two most important exceptions
  * are also the sole abstract methods: `next` and `hasNext`.
  *
  * Both these methods can be called any number of times without having to discard the
  * iterator. Note that even `hasNext` may cause mutation -- such as when iterating
  * from an input stream, where it will block until the stream is closed or some
  * input becomes available.
  *
  * Consider this example for safe and unsafe use:
  *
  * {{{
  * def f[A](it: Iterator[A]) = {
  *   if (it.hasNext) {            // Safe to reuse "it" after "hasNext"
  *     it.next                    // Safe to reuse "it" after "next"
  *     val remainder = it.drop(2) // it is *not* safe to use "it" again after this line!
  *     remainder.take(2)          // it is *not* safe to use "remainder" after this line!
  *   } else it
  * }
  * }}}
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
trait Iterator[+A] extends IterableOnce[A] with IterableOnceOps[A, Iterator, Iterator[A]] { self =>

  /** Check if there is a next element available.
    *
    * @return `true` if there is a next element, `false` otherwise
    * @note   Reuse: $preservesIterator
    */
  def hasNext: Boolean

  /** Return the next element and advance the iterator.
    *
    * @throws NoSuchElementException if there is no next element.
    * @return the next element.
    * @note   Reuse: Advances the iterator, which may exhaust the elements. It is valid to
    *         make additional calls on the iterator.
    */
  @throws[NoSuchElementException]
  def next(): A

  @inline final def iterator = this

  /** Wraps the value of `next()` in an option.
    *
    * @return `Some(next)` if a next element exists, `None` otherwise.
    */
  def nextOption(): Option[A] = if (hasNext) Some(next()) else None

  /** Tests whether this iterator contains a given value as an element.
    *  $mayNotTerminateInf
    *
    *  @param elem  the element to test.
    *  @return     `true` if this iterator produces some value that is
    *               is equal (as determined by `==`) to `elem`, `false` otherwise.
    *  @note        Reuse: $consumesIterator
    */
  def contains(elem: Any): Boolean = exists(_ == elem)    // Note--this seems faster than manual inlining!

  /** Creates a buffered iterator from this iterator.
    *
    *  @see [[scala.collection.BufferedIterator]]
    *  @return  a buffered iterator producing the same values as this iterator.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def buffered: BufferedIterator[A] = new AbstractIterator[A] with BufferedIterator[A] {
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false

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
  class GroupedIterator[B >: A](self: Iterator[B], size: Int, step: Int) extends AbstractIterator[immutable.Seq[B]] {

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

    private def padding(x: Int) = immutable.ArraySeq.untagged.fill(x)(pad.get())
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
    def next(): immutable.Seq[B] = {
      if (!filled)
        fill()

      if (!filled)
        throw new NoSuchElementException("next on empty iterator")
      filled = false
      immutable.ArraySeq.unsafeWrapArray(buffer.toArray[Any]).asInstanceOf[immutable.ArraySeq[B]]
    }
  }

  /** A copy of this $coll with an element value appended until a given target length is reached.
   *
   *  @param   len   the target length
   *  @param   elem  the padding value
   *  @tparam B      the element type of the returned $coll.
   *  @return a new $coll consisting of
   *          all elements of this $coll followed by the minimal number of occurrences of `elem` so
   *          that the resulting collection has a length of at least `len`.
   */
  def padTo[B >: A](len: Int, elem: B): Iterator[B] = {
    val it = this
    new AbstractIterator[B] {
      private[this] var i = 0

      def next(): B = {
        val b =
          if (it.hasNext) it.next()
          else if (i < len) elem
          else Iterator.empty.next()
        i += 1
        b
      }

      def hasNext: Boolean = it.hasNext || i < len
    }
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
    val (a, b) = duplicate
    (a filter p, b filterNot p)
  }

  /** Returns an iterator which groups this iterator into fixed size
   *  blocks.  Example usages:
   *  {{{
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7)))
   *    (1 to 7).iterator.grouped(3).toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6))
   *    (1 to 7).iterator.grouped(3).withPartial(false).toList
   *    // Returns List(List(1, 2, 3), List(4, 5, 6), List(7, 20, 25)
   *    // Illustrating that withPadding's argument is by-name.
   *    val it2 = Iterator.iterate(20)(_ + 5)
   *    (1 to 7).iterator.grouped(3).withPadding(it2.next).toList
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

  def scanLeft[B](z: B)(op: (B, A) => B): Iterator[B] = new AbstractIterator[B] {
    // We use an intermediate iterator that iterates through the first element `z`
    // and then that will be modified to iterate through the collection
    private[this] var current: Iterator[B] =
      new AbstractIterator[B] {
        def hasNext: Boolean = true
        def next(): B = {
          // Here we change our self-reference to a new iterator that iterates through `self`
          current = new AbstractIterator[B] {
            private[this] var acc = z
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

  @deprecated("Call scanRight on an Iterable instead.", "2.13.0")
  def scanRight[B](z: B)(op: (A, B) => B): Iterator[B] = ArrayBuffer.from(this).scanRight(z)(op).iterator

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

  @inline final def length: Int = size

  @deprecatedOverriding("isEmpty is defined as !hasNext; override hasNext instead", "2.13.0")
  override def isEmpty: Boolean = !hasNext

  def filter(p: A => Boolean): Iterator[A] = filterImpl(p, isFlipped = false)

  def filterNot(p: A => Boolean): Iterator[A] = filterImpl(p, isFlipped = true)

  private[collection] def filterImpl(p: A => Boolean, isFlipped: Boolean): Iterator[A] = new AbstractIterator[A] {
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false

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

  def collect[B](pf: PartialFunction[A, B]): Iterator[B] = new AbstractIterator[B] {
    // Manually buffer to avoid extra layer of wrapping with buffered
    private[this] var hd: B = _

    // Little state machine to keep track of where we are
    // Seek = 0; Found = 1; Empty = -1
    // Not in vals because scalac won't make them static (@inline def only works with -optimize)
    // BE REALLY CAREFUL TO KEEP COMMENTS AND NUMBERS IN SYNC!
    private[this] var status = 0/*Seek*/

    def hasNext = {
      val marker = Statics.pfMarker
      while (status == 0/*Seek*/) {
        if (self.hasNext) {
          val x = self.next()
          val v = pf.applyOrElse(x, ((x: A) => marker).asInstanceOf[A => B])
          if (marker ne v.asInstanceOf[AnyRef]) {
            hd = v
            status = 1/*Found*/
          }
        }
        else status = -1/*Empty*/
      }
      status == 1/*Found*/
    }
    def next() = if (hasNext) { status = 0/*Seek*/; hd } else Iterator.empty.next()
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
  def distinctBy[B](f: A => B): Iterator[A] = new AbstractIterator[A] {

    private[this] val traversedValues = mutable.HashSet.empty[B]
    private[this] var nextElementDefined: Boolean = false
    private[this] var nextElement: A = _

    def hasNext: Boolean = nextElementDefined || (self.hasNext && {
      val a = self.next()
      if (traversedValues.add(f(a))) {
        nextElement = a
        nextElementDefined = true
        true
      }
      else hasNext
    })

    def next(): A =
      if (hasNext) {
        nextElementDefined = false
        nextElement
      } else {
        Iterator.empty.next()
      }
  }

  def map[B](f: A => B): Iterator[B] = new AbstractIterator[B] {
    def hasNext = self.hasNext
    def next() = f(self.next())
  }

  def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new AbstractIterator[B] {
    private[this] var myCurrent: Iterator[B] = Iterator.empty
    private def current = {
      while (!myCurrent.hasNext && self.hasNext)
        myCurrent = f(self.next()).iterator
      myCurrent
    }
    def hasNext = current.hasNext
    def next() = current.next()
  }

  def flatten[B](implicit ev: A => IterableOnce[B]): Iterator[B] =
    flatMap[B](ev)

  def concat[B >: A](xs: => IterableOnce[B]): Iterator[B] = new Iterator.ConcatIterator[B](self).concat(xs)

  @`inline` final def ++ [B >: A](xs: => IterableOnce[B]): Iterator[B] = concat(xs)

  def take(n: Int): Iterator[A] = sliceIterator(0, n max 0)

  def takeWhile(p: A => Boolean): Iterator[A] = new AbstractIterator[A] {
    private[this] var hd: A = _
    private[this] var hdDefined: Boolean = false
    private[this] var tail: Iterator[A] = self

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
      private def store(a: A): Unit = {
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
        else Iterator.empty.next()
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
        else Iterator.empty.next()
      }
    }

    (leading, trailing)
  }

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

  def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new AbstractIterator[(A, B)] {
    val thatIterator = that.iterator
    def hasNext = self.hasNext && thatIterator.hasNext
    def next() = (self.next(), thatIterator.next())
  }

  def zipAll[A1 >: A, B](that: IterableOnce[B], thisElem: A1, thatElem: B): Iterator[(A1, B)] = new AbstractIterator[(A1, B)] {
    val thatIterator = that.iterator
    def hasNext = self.hasNext || thatIterator.hasNext
    def next(): (A1, B) = {
      val next1 = self.hasNext
      val next2 = thatIterator.hasNext
      if(!(next1 || next2)) throw new NoSuchElementException
      (if(next1) self.next() else thisElem, if(next2) thatIterator.next() else thatElem)
    }
  }

  def zipWithIndex: Iterator[(A, Int)] = new AbstractIterator[(A, Int)] {
    var idx = 0
    def hasNext = self.hasNext
    def next() = {
      val ret = (self.next(), idx)
      idx += 1
      ret
    }
  }

  def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    val those = that.iterator
    while (hasNext && those.hasNext)
      if (next() != those.next())
        return false
    // At that point we know that *at least one* iterator has no next element
    // If *both* of them have no elements then the collections are the same
    hasNext == those.hasNext
  }

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
    new AbstractIterator[B] {
      private[this] var origElems = self
      private[this] var i = if (from > 0) from else 0 // Counts down, switch to patch on 0, -1 means use patch first
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

  /** Converts this iterator to a string.
   *
   *  @return `"<iterator>"`
   *  @note    Reuse: $preservesIterator
   */
  override def toString = "<iterator>"

  @deprecated("Iterator.seq always returns the iterator itself", "2.13.0")
  def seq: this.type = this
}

@SerialVersionUID(3L)
object Iterator extends IterableFactory[Iterator] {

  private[this] val _empty: Iterator[Nothing] = new AbstractIterator[Nothing] {
    def hasNext = false
    def next() = throw new NoSuchElementException("next on empty iterator")
    override def knownSize: Int = 0
  }

  /** Creates a target $coll from an existing source collection
    *
    * @param source Source collection
    * @tparam A the type of the collection’s elements
    * @return a new $coll with the elements of `source`
    */
  override def from[A](source: IterableOnce[A]): Iterator[A] = source.iterator

  /** The iterator which produces no values. */
  @`inline` final def empty[T]: Iterator[T] = _empty

  def single[A](a: A): Iterator[A] = new AbstractIterator[A] {
    private[this] var consumed: Boolean = false
    def hasNext = !consumed
    def next() = if (consumed) empty.next() else { consumed = true; a }
  }

  override def apply[A](xs: A*): Iterator[A] = xs.iterator

  /**
    * @return A builder for $Coll objects.
    * @tparam A the type of the ${coll}’s elements
    */
  def newBuilder[A]: Builder[A, Iterator[A]] =
    new ImmutableBuilder[A, Iterator[A]](empty[A]) {
      override def addOne(elem: A): this.type = { elems = elems ++ single(elem); this }
    }

  /** Creates iterator that produces the results of some element computation a number of times.
    *
    *  @param   len  the number of elements returned by the iterator.
    *  @param   elem the element computation
    *  @return  An iterator that produces the results of `n` evaluations of `elem`.
    */
  override def fill[A](len: Int)(elem: => A): Iterator[A] = new AbstractIterator[A] {
    private[this] var i = 0
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
  override def tabulate[A](end: Int)(f: Int => A): Iterator[A] = new AbstractIterator[A] {
    private[this] var i = 0
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
  def from(start: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    private[this] var i = start
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
  def range(start: Int, end: Int, step: Int): Iterator[Int] = new AbstractIterator[Int] {
    if (step == 0) throw new IllegalArgumentException("zero step")
    private[this] var i = start
    private[this] var hasOverflowed = false
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

  /** Creates an Iterator that uses a function `f` to produce elements of type `A`
    * and update an internal state of type `S`.
    *
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    * @return an Iterator that produces elements using `f` until `f` returns `None`
    */
  override def unfold[A, S](init: S)(f: S => Option[(A, S)]): Iterator[A] = new UnfoldIterator(init)(f)

  /** Creates an infinite-length iterator returning the results of evaluating an expression.
    *  The expression is recomputed for every element.
    *
    *  @param elem the element computation.
    *  @return the iterator containing an infinite number of results of evaluating `elem`.
    */
  def continually[A](elem: => A): Iterator[A] = new AbstractIterator[A] {
    def hasNext = true
    def next() = elem
  }

  /** Creates an iterator to which other iterators can be appended efficiently.
    *  Nested ConcatIterators are merged to avoid blowing the stack.
    */
  private final class ConcatIterator[+A](private var current: Iterator[A @uncheckedVariance]) extends AbstractIterator[A] {
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
    def headIterator: Iterator[A] = head.iterator
  }

  /** Creates a delegating iterator capped by a limit count. Negative limit means unbounded.
    *  Lazily skip to start on first evaluation.  Avoids daisy-chained iterators due to slicing.
    */
  private[scala] final class SliceIterator[A](val underlying: Iterator[A], start: Int, limit: Int) extends AbstractIterator[A] {
    private[this] var remaining = limit
    private[this] var dropping  = start
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

  /** Creates an iterator that uses a function `f` to produce elements of
    * type `A` and update an internal state of type `S`.
    */
  private final class UnfoldIterator[A, S](init: S)(f: S => Option[(A, S)]) extends AbstractIterator[A] {
    private[this] var state: S = init
    private[this] var nextResult: Option[(A, S)] = null

    override def hasNext: Boolean = {
      if (nextResult eq null) {
        nextResult = {
          val res = f(state)
          if (res eq null) throw new NullPointerException("null during unfold")
          res
        }
        state = null.asInstanceOf[S] // allow GC
      }
      nextResult.isDefined
    }

    override def next(): A = {
      if (hasNext) {
        val (value, newState) = nextResult.get
        state = newState
        nextResult = null
        value
      } else Iterator.empty.next()
    }
  }

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}

/** Explicit instantiation of the `Iterator` trait to reduce class file size in subclasses. */
abstract class AbstractIterator[+A] extends Iterator[A]
