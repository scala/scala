/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Stream.scala 16287 2008-10-18 13:41:36Z nielsen $


package scalax.collection.immutable

import mutable.ListBuffer
import generic.{SequenceTemplate, SequenceFactory, EmptyIterableFactory, Builder, LazyBuilder}
import annotation.unchecked.uncheckedVariance
import annotation.tailrec

/**
 * The object <code>Stream</code> provides helper functions
 * to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 */
object Stream extends SequenceFactory[Stream] with EmptyIterableFactory[Stream] {

  import collection.{Iterable, OrderedIterable, Sequence, Vector}

  override val empty: Stream[Nothing] = Nil
  override def apply[A](xs: A*) = xs.asInstanceOf[Iterable[A]].toList // !@!
  override def newBuilder[B]: Builder[Stream, B] = new ListBuffer[B]

  class ConsWrapper[A](tl: => Stream[A]) {
    def #::(hd: A): Stream[A] = new Cons(hd, tl)
    def #:::(prefix: Stream[A]): Stream[A] = prefix append tl
  }

  implicit def consWrapper[A](stream: => Stream[A]): ConsWrapper[A] =
    new ConsWrapper[A](stream)

  object #:: {
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] =
      if (xs.isEmpty) None
      else Some(xs.head, xs.tail)
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

  final class Cons[+A](hd: A, tl: => Stream[A]) extends Stream[A] {
    override def isEmpty = false
    override def head = hd
    private[this] var tlVal: Stream[A] = _
    private def tlDefined = tlVal ne null
    override def tail: Stream[A] = {
      if (!tlDefined) { tlVal = tl }
      tlVal
    }
    override def hasDefiniteSize = tlDefined && tlVal.hasDefiniteSize

    // Overridden methods from IterableTemplate or overloaded variants of such methods

    /** Create a new stream which contains all elements of this stream
     *  followed by all elements of Iterable `that'
     */
    override def ++[B >: A](that: Iterable[B]): Stream[B] = this append that

    /** Create a new stream which contains all elements of this stream
     *  followed by all elements of Iterator `that'
     */
    override def ++[B >: A](that: Iterator[B]): Stream[B] = this append that.toStream

    /** Returns the stream resulting from applying the given function
     *  <code>f</code> to each element of this stream.
     *
     *  @param f function to apply to each element.
     *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
     *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
     */
    override def map[B](f: A => B): Stream[B] =
      new Cons(f(head), tail map f)

    /** Applies the given function <code>f</code> to each element of
     *  this stream, then concatenates the results.
     *
     *  @param f the function to apply on each element.
     *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
     *           this stream is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
     */
    override def flatMap[B](f: A => Iterable[B]): Stream[B] = {
      // drops A's for which f yields an empty Iterable[B]
      def loop(s: Stream[A]): Stream[B] =
        if (s.isEmpty) Nil
        else {
          val i = f(s.head)
          if (i.isEmpty) loop(s.tail)
          else i.toStream append loop(s.tail)
        }
      loop(this)
    }

    /** Returns all the elements of this stream that satisfy the
     *  predicate <code>p</code>. The order of the elements is preserved.
     *
     *  @param p the predicate used to filter the stream.
     *  @return the elements of this stream satisfying <code>p</code>.
     */
    override def filter(p: A => Boolean): Stream[A] = {
      // drops A's for which p yields false
      def loop(s: Stream[A]): Stream[A] =
        if (s.isEmpty) Nil
        else {
          val b = p(s.head)
          if (!b) loop(s.tail)
          else new Cons(s.head, tail filter p)
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
    def zip[B](that: Stream[B]): Stream[(A, B)] =
      if (that.isEmpty) empty
      else new Cons((this.head, that.head), this.tail zip that.tail)

    /** Returns a list formed from this list and the specified list
     *  <code>that</code> by associating each element of the former with
     *  the element at the same position in the latter.
     *
     *  @param that     list <code>that</code> may have a different length
     *                  as the self list.
     *  @param thisElem element <code>thisElem</code> is used to fill up the
     *                  resulting list if the self list is shorter than
     *                  <code>that</code>
     *  @param thatElem element <code>thatElem</code> is used to fill up the
     *                  resulting list if <code>that</code> is shorter than
     *                  the self list
     *  @return         <code>List((a<sub>0</sub>,b<sub>0</sub>), ...,
     *                  (a<sub>n</sub>,b<sub>n</sub>), (elem,b<sub>n+1</sub>),
     *                  ..., {elem,b<sub>m</sub>})</code>
     *                  when <code>[a<sub>0</sub>, ..., a<sub>n</sub>] zip
     *                  [b<sub>0</sub>, ..., b<sub>m</sub>]</code> is
     *                  invoked where <code>m &gt; n</code>.
     */
    def zipAll[B, A1 >: A, B1 >: B](that: Stream[B], thisElem: A1, thatElem: B1): Stream[(A1, B1)] = {
      if (that.isEmpty) new Cons((this.head, thatElem), this.tail.zipAll(that, thisElem, thatElem))
      else new Cons((this.head, that.head), this.tail.zipAll(that.tail, thisElem, thatElem))
    }

    /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to
     *  `s zip s.indices`
     */
    override def zipWithIndex = this zip (Iterator.from(0).toStream)

    /** Write all defined elements of this iterable into given string builder.
     *  The written text begins with the string <code>start</code> and is finished by the string
     *  <code>end</code>. Inside, the string representations of defined elements (w.r.t.
     *  the method <code>toString()</code>) are separated by the string
     *  <code>sep</code>. The method will not force evaluation of undefined elements. A
     *  tail of such elements will be represented by a "?" instead.
     */
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
      b append start append hd
      if (tlDefined) tlVal.addString(b, sep, sep, end)
      else b append ", ?" append end
    }

    /** Returns the <code>n</code> first elements of this stream, or else the whole
     *  stream, if it has less than <code>n</code> elements.
     *
     *  @param n the number of elements to take.
     *  @return the <code>n</code> first elements of this stream.
     */
    override def take(n: Int): Stream[A] =
      if (n <= 0) Nil else new Cons(head, tail take (n-1))

    /** Returns the stream without its <code>n</code> first elements.
     *  If the stream has less than <code>n</code> elements, the empty stream is returned.
     *
     *  @param n the number of elements to drop.
     *  @return the stream without its <code>n</code> first elements.
     */
    override def drop(n: Int): Stream[A] = {
      var these: Stream[A] = this
      var i = n
      while (!these.isEmpty && i > 0) {
        these = these.tail
        i -= 1
      }
      these
    }

    /** A substream starting at index `from`
     *  and extending up to (but not including) index `until`.
     *
     *  @This is equivalent to (but possibly more efficient than)
     *  c.drop(from).take(to - from)
     *
     *  @param from   The index of the first element of the returned subsequence
     *  @param until  The index of the element following the returned subsequence
     *  @throws IndexOutOfBoundsException if <code>from &lt; 0</code>
     *          or <code>length &lt; from + len<code>
     *  @note  Might return different results for different runs, unless this iterable is ordered
     */
    override def slice(from: Int, to: Int): Stream[A] =
      this.drop(from).take(to - from)

    /** The stream without its last element.
     *  @throws Predef.UnsupportedOperationException if the stream is empty.
     */
    override def init: Stream[A] =
      if (tail.isEmpty) Nil
      else new Cons(head, tail.init)

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

    /** Returns the longest prefix of this stream whose elements satisfy
     *  the predicate <code>p</code>.
     *
     *  @param p the test predicate.
     */
    override def takeWhile(p: A => Boolean): Stream[A] =
      if (p(head)) new Cons(head, tail takeWhile p) else Nil


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

    /** Returns a pair consisting of the longest prefix of the stream whose
     *  elements all satisfy the given predicate, and the rest of the stream.
     *
     *  @param p the test predicate
     */
    override def span(p: A => Boolean): (List[A], Stream[A]) = {
      var these: Stream[A] = this
      val l = new ListBuffer[A]
      while (!these.isEmpty && p(these.head)) {
        l += these.head
        these = these.tail
      }
      (l.toList, these)
    }

    // Overridden methods from Sequence

    /** Builds a new stream from this stream in which any duplicates (wrt to ==) removed.
     *  Among duplicate elements, only the first one is retained in the result stream
     */
    override def removeDuplicates: Stream[A] =
      new Cons(head, tail.filter(head !=).removeDuplicates)

    /** Returns a new sequence of given length containing the elements of this sequence followed by zero
     *  or more occurrences of given elements.
     */
    override def padTo[B >: A](len: Int, elem: B): Stream[B] =
      new Cons(head, tail.padTo(len - 1, elem))
  }

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
  @deprecated def fill[A](elem: => A): Stream[A] = new Cons(elem, fill(elem))

  /** A stream containing all elements of a given iterator, in the order they are produced.
   *  @param it   The iterator producing the stream's elements
   *  @deprecated use it.toStream instead
   */
  @deprecated def fromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) cons(it.next, fromIterator(it)) else empty

  /** The concatenation of a sequence of streams
   * @deprecated use xs.flatten instead
   */
  @deprecated def concat[A](xs: Iterable[Stream[A]]): Stream[A] = concat(xs.elements)

  /** The concatenation of all streams returned by an iterator
   * @deprecated use xs.toStream.flatten instead
   */
  @deprecated def concat[A](xs: Iterator[Stream[A]]): Stream[A] =
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

import Stream._

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
abstract class Stream[+A] extends Sequence[A]
                             with SequenceTemplate[Stream, A @uncheckedVariance] {
self =>

  import collection.{Iterable, OrderedIterable, Sequence, Vector}

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

  // Implementation of abstract methods

  /** Create a new @see LazyBuilder to build a stream */
  def newBuilder[B]: Builder[Stream, B] = new LazyBuilder[Stream, B] {
    def result: Stream[B] = elements.toStream
  }

  /** Returns the number of elements in the list.
   */
  def length: Int = {
    var these = self
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Returns the <code>n</code>-th element of this stream. The first element
   *  (head of the stream) is at position 0.
   *
   *  @param n index of the element to return
   *  @return  the element at position <code>n</code> in this stream.
   *  @throws Predef.NoSuchElementException if the stream is too short.
   */
  override def apply(n: Int): A = drop(n).head

  // New methods in Stream

  /** The stream resulting from the concatenation of this stream with the argument stream.
   *  @param rest   The stream that gets appended to this stream
   */
  def append[B >: A](rest: => Iterable[B]): Stream[B] =
    if (isEmpty) rest.toStream else new Cons(head, tail append rest)

  /** Force evaluation of the whole stream and return it */
  def force: Stream[A] = {
    var these = this
    while (!isEmpty) these = these.tail
    this
  }

  /** Prints elements of this stream one by one, separated by commas */
  def print() { print(", ") }

  /** Prints elements of this stream one by one, separated by <code>sep</code>
   *  @param sep   The separator string printed between consecutive elements.
   */
  def print(sep: String) {
    @tailrec
    def loop(these: Stream[A], start: String) {
      Console.print(start)
      if (isEmpty) Console.print("empty")
      else {
        Console.print(these.head)
        loop(these.tail, sep)
      }
    }
    loop(this, "")
  }

  // Overridden methods from IterableTemplate or overloaded variants of such methods

  /** Returns the elements in the sequence as an iterator
   */
  override def elements: Iterator[A] = new Iterator[A] {
    var these = self
    def hasNext: Boolean = !these.isEmpty
    def next: A =
      if (hasNext) {
        val result = these.head; these = these.tail; result
      } else Iterator.empty.next
    override def toList: List[A] = these.toList
  }

  /** Apply the given function <code>f</code> to each element of this stream
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   */
  override def foreach(f: A => Unit) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  /** Tests if the predicate <code>p</code> is satisfied by all elements
   *  in this list.
   *
   *  !!! todo: perform speed with inherited version from Iterable, and drop
   *      if not significantly better
   *  @param p the test predicate.
   *  @return  <code>true</code> iff all elements of this list satisfy the
   *           predicate <code>p</code>.
   */
  override def forall(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  /** Tests the existence in this list of an element that satisfies the
   *  predicate <code>p</code>.
   *
   *  !!! todo: perform speed with inherited version from Iterable, and drop
   *      if not significantly better
   *  @param p the test predicate.
   *  @return  <code>true</code> iff there exists an element in this list that
   *           satisfies the predicate <code>p</code>.
   */
  override def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  /** Count the number of elements in the iterable which satisfy a predicate.
   *
   *  !!! todo: perform speed with inherited version from Iterable, and drop
   *      if not significantly better
   *  @param p the predicate for which to count
   *  @return  the number of elements satisfying the predicate <code>p</code>.
   */
  override def count(p: A => Boolean): Int = {
    var these = this
    var cnt = 0
    while (!these.isEmpty) {
      if (p(these.head)) cnt += 1
      these = these.tail
    }
    cnt
  }

  /** Find and return the first element of the list satisfying a
   *  predicate, if any.
   *
   *  !!! todo: perform speed with inherited version from Iterable, and drop
   *      if not significantly better
   *  @param p the predicate
   *  @return the first element in the list satisfying <code>p</code>,
   *  or <code>None</code> if none exists.
   */
  override def find(p: A => Boolean): Option[A] = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  !!! todo: perform speed with inherited version from Iterable, and drop
   *      if not significantly better
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the list is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  /** Combines the elements of this list together using the binary
   *  function <code>f</code>, from right to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the list is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  override def foldRight[B](z: B)(f: (A, B) => B): B =
    if (this.isEmpty) z
    else f(head, tail.foldRight(z)(f))

  /** Combines the elements of this list together using the binary
   *  operator <code>op</code>, from left to right
   *  @param op  The operator to apply
   *  @return <code>op(... op(a<sub>0</sub>,a<sub>1</sub>), ..., a<sub>n</sub>)</code>
      if the list has elements
   *          <code>a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub></code>.
   *  @throws Predef.UnsupportedOperationException if the list is empty.
   */
  override def reduceLeft[B >: A](f: (B, A) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else tail.foldLeft[B](head)(f)

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
  override def reduceRight[B >: A](op: (A, B) => B): B =
    if (isEmpty) throw new UnsupportedOperationException("Nil.reduceRight")
    else if (tail.isEmpty) head
    else op(head, tail.reduceRight(op))

  /**
   *  Create a stream which contains all the elements of this iterable object.
   *  @note consider using <code>projection</code> for lazy behavior.
   */
  override def toStream: Stream[A] = this

  /** Defines the prefix of this object's <code>toString</code> representation as ``Stream''.
   */
  override def stringPrefix = "Stream"

  /** The last element of this stream.
   *
   *  @throws Predef.NoSuchElementException if the stream is empty.
   */
  override def last: A = {
    if (isEmpty) throw new NoSuchElementException
    var these = this
    var nx = these.tail
    while (!nx.isEmpty) {
      these = nx
      nx = nx.tail
    }
    these.head
  }

  /** Returns the rightmost <code>n</code> elements from this iterable.
   *  @param n the number of elements to take
   */
  override def dropRight(n: Int): List[A] = {
    val b = new ListBuffer[A]
    var these = this
    var lead = this drop n
    while (!lead.isEmpty) {
      b += these.head
      these = these.tail
      lead = lead.tail
    }
    b.toList
  }

  /** Returns true iff the other stream contains the same elements as this one.
   *
   *  @note will not terminate for two infinite-sized streams.
   *  @param that  the other stream
   */
  def sameElements[B >: A](that: Stream[B]): Boolean = {
    val these = this
    val those = that
    while (!these.isEmpty && !those.isEmpty && these.head == those.head) {}
    these.isEmpty && those.isEmpty
  }

  // Overridden methods from Sequence

  /** Result of comparing <code>length</code> with operand <code>len</code>.
   *  returns <code>x</code> where
   *  <code>x &lt; 0</code>    iff    <code>this.length &lt; len</code>
   *  <code>x == 0</code>   iff    <code>this.length == len</code>
   *  <code>x &gt; 0</code>    iff    <code>this.length &gt; len</code>.
   */
  override def lengthCompare(len: Int): Int =  {
    var i = 0
    var these = self
    while (!these.isEmpty && i <= len) {
      i += 1
      these = these.tail
    }
    i - len
  }

  /** Is this partial function defined for the index <code>x</code>?
   */
  override def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) >= 0

  /** Returns length of longest segment starting from a start index `from`
   *  such that every element of the segment satisfies predicate `p`.
   *  @note may not terminate for infinite-sized collections.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  override def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (!these.isEmpty && p(these.head)) {
      i += 1
      these = these.tail
    }
    i
  }

  /** Returns index of the first element starting from a start index
   *  satisying a predicate, or -1, if none exists.
   *
   *  @note may not terminate for infinite-sized streams.
   *  @param  p the predicate
   *  @param  from  the start index
   */
  override def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (!these.isEmpty && !p(these.head)) {
      i += 1
      these = these.tail
    }
    if (these.isEmpty) -1 else i
  }

  /** Returns index of the last element satisying a predicate, or -1, if none exists.
   *
   *  @param  p the predicate
   *  @return   the index of the last element satisfying <code>p</code>,
   *            or -1 if such an element does not exist
   */
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = 0
    var these = this
    var last = -1
    while (!these.isEmpty && i <= end) {
      if (p(these.head)) last = i
    }
    i
  }

  /** A list consisting of all elements of this list in reverse order.
   */
  override def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }
}

