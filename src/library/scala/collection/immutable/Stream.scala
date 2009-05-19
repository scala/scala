/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Stream.scala 16287 2008-10-18 13:41:36Z nielsen $


package scala.collection.immutable

import mutable.ListBuffer
import generic._

/**
 * <p>The class <code>Stream</code> implements lazy lists where elements
 * are only evaluated when they are needed. Here is an example:</p>
 * <pre>
 * <b>object</b> Main <b>extends</b> Application {
 *
 *   <b>def</b> from(n: Int): Stream[Int] =
 *     Stream.cons(n, from(n + 1))
 *
 *   <b>def</b> sieve(s: Stream[Int]): Stream[Int] =
 *     Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))
 *
 *   <b>def</b> primes = sieve(from(2))
 *
 *   primes take 10 print
 * }
 * </pre>
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 */
abstract class Stream[+A] extends LinearSequence[A]
                             with TraversableClass[A, Stream]
                             with LinearSequenceTemplate[A, Stream[A]] {
self =>
  override def companion: Companion[Stream] = Stream

  import collection.{Traversable, Iterable, Sequence, Vector}

  /** is this stream empty? */
  def isEmpty: Boolean

  /** The first element of this stream
   *  @throws Predef.NoSuchElementException if the stream is empty.
   */
  def head: A

  /** A stream consisting of the remaining elements of this stream after the first one.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   */
  def tail: Stream[A]

  /** Is the tail of this stream defined? */
  protected def tailDefined: Boolean

  // Implementation of abstract method in Traversable

  // New methods in Stream

  /** The stream resulting from the concatenation of this stream with the argument stream.
   *  @param rest   The stream that gets appended to this stream
   */
  def append[B >: A](rest: => Traversable[B]): Stream[B] =
    if (isEmpty) rest.toStream else new Stream.Cons(head, tail append rest)

  /** Force evaluation of the whole stream and return it */
  def force: Stream[A] = {
    var these = this
    while (!these.isEmpty) these = these.tail
    this
  }

  /** Does this stream have more than one elements defined?
   */
  private def hasMoreThanOneElements = false

  /** Prints elements of this stream one by one, separated by commas */
  def print() { print(", ") }

  /** Prints elements of this stream one by one, separated by <code>sep</code>
   *  @param sep   The separator string printed between consecutive elements.
   */
  def print(sep: String) {
    def loop(these: Stream[A], start: String) {
      Console.print(start)
      if (these.isEmpty) Console.print("empty")
      else {
        Console.print(these.head)
        loop(these.tail, sep)
      }
    }
    loop(this, "")
  }

  // Overridden methods from Traversable

  override def toStream: Stream[A] = this

  override def hasDefiniteSize = {
    def loop(s: Stream[A]): Boolean = s.isEmpty || s.tailDefined && loop(s.tail)
    loop(this)
  }

  /** Create a new stream which contains all elements of this stream
   *  followed by all elements of Traversable `that'
   *  @note It's subtle why this works. We know that if the target type
   *  of the Builder That is either a Stream, or one of its supertypes, or undefined,
   *  then StreamBuilder will be chosen for the implicit.
   *  we recognize that fact and optimize to get more laziness.
   */
  override def ++[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, Stream[A]]): That = {
    def loop(these: Stream[A]): Stream[B] =
      if (these.isEmpty) that.toStream else new Stream.Cons(these.head, loop(these.tail))
    if (bf.isInstanceOf[Stream.StreamBuilderFactory[_]]) loop(this).asInstanceOf[That]
    else super.++(that)
  }

  /** Create a new stream which contains all elements of this stream
   *  followed by all elements of Iterator `that'
   */
  override def++[B >: A, That](that: Iterator[B])(implicit bf: BuilderFactory[B, That, Stream[A]]): That =
    this ++ that.toStream

  /** Returns the stream resulting from applying the given function
   *  <code>f</code> to each element of this stream.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  override def map[B, That](f: A => B)(implicit bf: BuilderFactory[B, That, Stream[A]]): That = {
    def loop(these: Stream[A]): Stream[B] =
      if (these.isEmpty) Stream.Empty else new Stream.Cons(f(these.head), loop(these.tail))
    if (bf.isInstanceOf[Stream.StreamBuilderFactory[_]]) loop(this).asInstanceOf[That]
    else super.map(f)
  }

  /** Applies the given function <code>f</code> to each element of
   *  this stream, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this stream is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def flatMap[B, That](f: A => Traversable[B])(implicit bf: BuilderFactory[B, That, Stream[A]]): That = {
    def loop(these: Stream[A]): Stream[B] =
      if (these.isEmpty) Stream.Empty
      else {
        val seg = f(these.head)
        if (seg.isEmpty) loop(these.tail)
        else seg.toStream ++ loop(these.tail)
      }
    if (bf.isInstanceOf[Stream.StreamBuilderFactory[_]]) loop(this).asInstanceOf[That]
    else super.flatMap(f)
  }

  /** Returns all the elements of this stream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the stream.
   *  @return the elements of this stream satisfying <code>p</code>.
   */
  override def filter(p: A => Boolean): Stream[A] = {
    // drops A's for which p yields false
    def loop(these: Stream[A]): Stream[A] =
      if (these.isEmpty) Stream.Empty
      else {
        val b = p(these.head)
        if (!b) loop(these.tail)
        else new Stream.Cons(these.head, these.tail filter p)
      }
    loop(this)
  }

  /** Returns all the elements of this stream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the stream.
   *  @return the elements of this stream satisfying <code>p</code>.
   */
  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), remove(p(_)))

  /** Returns a stream formed from this stream and the specified stream
   *  <code>that</code> by associating each element of the former with
   *  the element at the same position in the latter.
   *  If one of the two streams is longer than the other, its remaining elements are ignored.
   *
   *  @return     <code>Stream({a<sub>0</sub>,b<sub>0</sub>}, ...,
   *              {a<sub>min(m,n)</sub>,b<sub>min(m,n)</sub>)}</code> when
   *              <code>Stream(a<sub>0</sub>, ..., a<sub>m</sub>)
   *              zip Stream(b<sub>0</sub>, ..., b<sub>n</sub>)</code> is invoked.
   */
  override def zip[A1 >: A, B, That](that: Sequence[B])(implicit bf: BuilderFactory[(A1, B), That, Stream[A]]): That = {
    def loop(these: Stream[A], elems2: Iterator[B]): Stream[(A1, B)] =
      if (these.isEmpty || !elems2.hasNext) Stream.Empty
      else new Stream.Cons((these.head, elems2.next), loop(these.tail, elems2))
    if (bf.isInstanceOf[Stream.StreamBuilderFactory[_]]) loop(this, that.elements).asInstanceOf[That]
    else super.zip[A1, B, That](that)
  }

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to
   *  `s zip s.indices`
   */
  override def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, Stream[A]]): That =
    this.zip[A1, Int, That](Stream.from(0))

  /** Write all defined elements of this iterable into given string builder.
   *  The written text begins with the string <code>start</code> and is finished by the string
   *  <code>end</code>. Inside, the string representations of defined elements (w.r.t.
   *  the method <code>toString()</code>) are separated by the string
   *  <code>sep</code>. The method will not force evaluation of undefined elements. A
   *  tail of such elements will be represented by a "?" instead.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    def loop(pre: String, these: Stream[A]) {
      if (these.isEmpty) b append end
      else {
        b append pre append these.head
        if (these.tailDefined) loop(sep, these.tail)
        else b append sep append "?" append end
      }
    }
    b append start
    loop("", this)
    b
  }

  /** Returns the <code>n</code> first elements of this stream, or else the whole
   *  stream, if it has less than <code>n</code> elements.
   *
   *  @param n the number of elements to take.
   *  @return the <code>n</code> first elements of this stream.
   */
  override def take(n: Int): Stream[A] =
    if (n <= 0 || isEmpty) Stream.Empty else new Stream.Cons(head, tail take (n-1))

  /** Returns the stream without its <code>n</code> first elements.
   *  If the stream has less than <code>n</code> elements, the empty stream is returned.
   *
   *  @param n the number of elements to drop.
   *  @return the stream without its <code>n</code> first elements.
   */
  override def drop(n: Int): Stream[A] = {
    var these: Stream[A] = this
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
  }

  /** A substream starting at index `from`
   *  and extending up to (but not including) index `until`.
   *
   *  @note This is equivalent to (but possibly more efficient than)
   *  c.drop(from).take(to - from)
   *
   *  @param start   The index of the first element of the returned subsequence
   *  @param end     The index of the element following the returned subsequence
   *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
   *          or <code>length &lt; from + len<code>
   *  @note  Might return different results for different runs, unless this iterable is ordered
   */
  override def slice(start: Int, end: Int): Stream[A] = {
    var len = end
    if (start > 0) len -= start
    drop(start) take len
  }

  /** The stream without its last element.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   */
  override def init: Stream[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) Stream.Empty
    else new Stream.Cons(head, tail.init)

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *  @param n the number of elements to take
   */
  override def takeRight(n: Int): Stream[A] = {
    var these: Stream[A] = this
    var lead = this drop n
    while (!lead.isEmpty) {
      these = these.tail
      lead = lead.tail
    }
    these
  }

  // there's nothing we can do about dropRight, so we just keep the definition in LinearSequence

  /** Returns the longest prefix of this stream whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   */
  override def takeWhile(p: A => Boolean): Stream[A] =
    if (!isEmpty && p(head)) new Stream.Cons(head, tail takeWhile p)
    else Stream.Empty

  /** Returns the longest suffix of this iterable whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   */
  override def dropWhile(p: A => Boolean): Stream[A] = {
    var these: Stream[A] = this
    while (!these.isEmpty && p(these.head)) these = these.tail
    these
  }

  /** Builds a new stream from this stream in which any duplicates (wrt to ==) removed.
   *  Among duplicate elements, only the first one is retained in the result stream
   */
  override def removeDuplicates: Stream[A] =
    if (isEmpty) this
    else new Stream.Cons(head, tail.filter(head !=).removeDuplicates)

  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements.
   */
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: BuilderFactory[B, That, Stream[A]]): That = {
    def loop(len: Int, these: Stream[A]): Stream[B] =
      if (these.isEmpty) Stream.fill(len)(elem)
      else new Stream.Cons(these.head, loop(len - 1, these.tail))
    if (bf.isInstanceOf[Stream.StreamBuilderFactory[_]]) loop(len, this).asInstanceOf[That]
    else super.padTo(len, elem)
  }

  /** A list consisting of all elements of this list in reverse order.
   */
  override def reverse: Stream[A] = {
    var result: Stream[A] = Stream.Empty
    var these = this
    while (!these.isEmpty) {
      val r = Stream.consWrapper(result).#::(these.head)
      r.tail // force it!
      result = r
      these = these.tail
    }
    result
  }

  /** Defines the prefix of this object's <code>toString</code> representation as ``Stream''.
   */
  override def stringPrefix = "Stream"
}

/**
 * The object <code>Stream</code> provides helper functions
 * to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 */
object Stream extends SequenceFactory[Stream] {

  class StreamBuilderFactory[A] extends VirtualBuilderFactory[A]

  implicit def builderFactory[A]: BuilderFactory[A, Stream[A], Coll] = new StreamBuilderFactory[A]

  /** Creates a new builder for a stream */
  def newBuilder[A]: Builder[A, Stream[A]] = new StreamBuilder[A]

  import collection.{Iterable, Sequence, Vector}

  /** A builder for streams
   *  @note: This builder is lazy only in the sense that it does not go downs the spine
   *         of traversables taht are added as a whole. If more layzness can be achieved,
   *         this builder should be bypassed.
   */
  class StreamBuilder[A] extends LazyBuilder[A, Stream[A]] {
    def result: Stream[A] = (for (xs <- parts.elements; x <- xs.toIterable.elements) yield x).toStream
  }

  object Empty extends Stream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("head of empty stream")
    override def tail = throw new UnsupportedOperationException("tail of empty stream")
    def tailDefined = false
  }

  /** The empty stream */
  override def empty[A]: Stream[A] = Empty

  /** A stream consisting of given elements */
  override def apply[A](xs: A*): Stream[A] = xs.toStream

  /** A wrapper class that adds `#::` for cons and `#:::` for concat as operations
   *  to streams.
   */
  class ConsWrapper[A](tl: => Stream[A]) {
    def #::(hd: A): Stream[A] = new Stream.Cons(hd, tl)
    def #:::(prefix: Stream[A]): Stream[A] = prefix append tl
  }

  /** A wrapper method that adds `#::` for cons and `#::: for concat as operations
   *  to streams.
   */
  implicit def consWrapper[A](stream: => Stream[A]): ConsWrapper[A] =
    new ConsWrapper[A](stream)

  /** An extractor that allows to pattern match streams with `#::`.
   */
  object #:: {
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

  /** @deprecated   use #:: instead */
  @deprecated lazy val lazy_:: = #::

  /** An alternative way of building and matching Streams using Stream.cons(hd, tl).
   */
  object cons {

    /** A stream consisting of a given first element and remaining elements
     *  @param hd   The first element of the result stream
     *  @param tl   The remaining elements of the result stream
     */
    def apply[A](hd: A, tl: => Stream[A]) = new Cons(hd, tl)

    /** Maps a stream to its head and tail */
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] = #::.unapply(xs)
  }

  /** A lazy cons cell, from which streams are built. */
  final class Cons[+A](hd: A, tl: => Stream[A]) extends Stream[A] {
    override def isEmpty = false
    override def head = hd
    private[this] var tlVal: Stream[A] = _
    def tailDefined = tlVal ne null
    override def tail: Stream[A] = {
      if (!tailDefined) { tlVal = tl }
      tlVal
    }
  }

  /** An infinite stream that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the stream
   *  @param f     the function that's repeatedly applied
   *  @return      the stream returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate(start: Int)(f: Int => Int): Stream[Int] = new Cons(start, iterate(f(start))(f))

  override def iterate(start: Int, len: Int)(f: Int => Int): Stream[Int] = iterate(start)(f) take len

  /**
   * Create an infinite stream starting at <code>start</code>
   * and incrementing by step <code>step</code>
   *
   * @param start the start value of the stream
   * @param step the increment value of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def from(start: Int, step: Int): Stream[Int] =
    new Cons(start, from(start+step, step))

  /**
   * Create an infinite stream starting at <code>start</code>
   * and incrementing by 1.
   *
   * @param start the start value of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def from(start: Int): Stream[Int] = from(start, 1)

  /**
   * Create an infinite stream containing the given element expression (which is computed for each
   * occurrence)
   * @param elem the element composing the resulting stream
   * @return the stream containing an inifinite number of elem
   * @deprecated use fill instead
   */
  def continually[A](elem: => A): Stream[A] = new Cons(elem, continually(elem))

  override def fill[A](n: Int)(elem: => A): Stream[A] =
    if (n <= 0) Empty else new Cons(elem, fill(n-1)(elem))

  override def tabulate[A](n: Int)(f: Int => A): Stream[A] = {
    def loop(i: Int) =
      if (i >= n) Empty else new Cons(f(i), tabulate(i+1)(f))
    loop(0)
  }

  override def range(start: Int, end: Int, step: Int): Stream[Int] =
    if (if (step < 0) start <= end else end <= start) Empty
    else new Cons(start, range(start + step, end, step))

  /** A stream containing all elements of a given iterator, in the order they are produced.
   *  @param it   The iterator producing the stream's elements
   *  @deprecated use it.toStream instead
   */
  @deprecated def fromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) cons(it.next, fromIterator(it)) else empty

  /** The concatenation of a sequence of streams
   * @deprecated use xs.flatten instead
   */
  def concat[A](xs: Iterable[Stream[A]]): Stream[A] = concat(xs.elements)

  /** The concatenation of all streams returned by an iterator
   * @deprecated use xs.toStream.flatten instead
   */
  def concat[A](xs: Iterator[Stream[A]]): Stream[A] =
    if (xs.hasNext) xs.next append concat(xs)
    else empty

  /**
   * Create a stream with element values
   * <code>v<sub>n+1</sub> = step(v<sub>n</sub>)</code>
   * where <code>v<sub>0</sub> = start</code>
   * and elements are in the range between <code>start</code> (inclusive)
   * and <code>end</code> (exclusive)
   * @deprecated  use @see iterate instead.
   * @param start the start value of the stream
   * @param end the end value of the stream
   * @param step the increment function of the stream, must be monotonically increasing or decreasing
   * @return the stream starting at value <code>start</code>.
   */
  @deprecated def range(start: Int, end: Int, step: Int => Int): Stream[Int] = {
    val up = step(start) > start
    val down = step(start) < start
    def loop(lo: Int): Stream[Int] =
      if ((!up || lo < end) && (!down || lo > end)) cons(lo, loop(step(lo)))
      else empty
    loop(start)
  }

  /**
   * Create an infinite stream containing the given element.
   *
   * @param elem the element composing the resulting stream
   * @return the stream containing an inifinite number of elem
   * @deprecated use fill(elem) instead
   */
  @deprecated def const[A](elem: A): Stream[A] = cons(elem, const(elem))

  /** Create a stream containing several copies of an element.
   *
   *  @param n    the length of the resulting stream
   *  @param elem the element composing the resulting stream
   *  @return     the stream composed of n elements all equal to elem
   *  @deprecated use fill(n, elem) instead
   */
  @deprecated def make[A](n: Int, elem: A): Stream[A] =
    const(elem) take n
}


