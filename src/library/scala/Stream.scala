/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import Predef._

/**
 * The object <code>Stream</code> provides helper functions
 * to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 */
object Stream {

   /** a stream with a definite size */
   trait Definite[+A] extends Stream[A] with Function0[Stream[A]] {
     override def hasDefiniteSize = true
     override def apply = this
     override def toString = super[Stream].toString
   }

  /** The empty stream */
  val empty: Stream[Nothing] = new Definite[Nothing] {
    override def isEmpty = true
    def head: Nothing = throw new NoSuchElementException("head of empty stream")
    def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    protected def addDefinedElems(buf: StringBuilder, prefix: String): StringBuilder = buf
  }

  object cons {
    /** A stream consisting of a given first element and remaining elements
     *  @param hd   The first element of the result stream
     *  @param tl   The remaining elements of the result stream
     */
    def apply[A](hd: A, tl: => Stream[A]) = new Stream[A] {
      override def hasDefiniteSize = if (tlDefined) tlVal.hasDefiniteSize else super.hasDefiniteSize
      override def isEmpty = false
      def head = hd
      private var tlVal: Stream[A] = _
      private def tlDefined = tlVal ne null
      def tail: Stream[A] = {
        if (!tlDefined) { tlVal = tl }
        tlVal
      }
      protected def addDefinedElems(buf: StringBuilder, prefix: String): StringBuilder = {
        val buf1 = buf.append(prefix).append(hd)
        if (tlDefined) tlVal.addDefinedElems(buf1, ", ") else buf1 append ", ?"
      }
    }

    def unapply[A](str: Stream[A]): Option[(A, Stream[A])] =
      if(str.isEmpty)
        None
      else
        Some((str.head, str.tail))
  }

  /** A stream containing all elements of a given iterator, in the order they are produced.
   *  @param it   The iterator producing the stream's elements
   */
  def fromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) cons(it.next, fromIterator(it)) else empty

  /** The concatenation of a sequence of streams
   */
  def concat[A](xs: Iterable[Stream[A]]): Stream[A] = concat(xs.elements)

  /** The concatenation of all given streams
   */
  def concat[A](s1: Stream[A], s2: Stream[A], ss: Stream[A]*): Stream[A] =
    s1 append s2 append concat(ss.elements)

  /** The concatenation of all streams returned by an iterator
   */
  def concat[A](xs: Iterator[Stream[A]]): Stream[A] =
    if (xs.hasNext) xs.next append concat(xs)
    else empty

  /**
   * Create a stream with element values
   * <code>v<sub>n+1</sub> = v<sub>n</sub> + 1</code>
   * where <code>v<sub>0</sub> = start</code>
   * and <code>v<sub>i</sub> &lt; end</code>.
   *
   * @param start the start value of the stream
   * @param end the end value of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def range(start: Int, end: Int): Stream[Int] =
    range(start, end, 1)

  /**
   * Create a stream with element values
   * <code>v<sub>n+1</sub> = v<sub>n</sub> + step</code>
   * where <code>v<sub>0</sub> = start</code>
   * and <code>v<sub>i</sub> &lt; end</code>.
   *
   * @param start the start value of the stream
   * @param end the end value of the stream
   * @param step the increment value of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def range(start: Int, end: Int, step: Int): Stream[Int] = {
    def loop(lo: Int): Stream[Int] =
      if (lo >= end) empty
      else cons(lo, loop(lo + step))
    loop(start)
  }

  /**
   * Create a stream with element values
   * <code>v<sub>n+1</sub> = step(v<sub>n</sub>)</code>
   * where <code>v<sub>0</sub> = start</code>
   * and <code>v<sub>i</sub> &lt; end</code>.
   *
   * @param start the start value of the stream
   * @param end the end value of the stream
   * @param step the increment function of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def range(start: Int, end: Int, step: Int => Int): Stream[Int] = {
    def loop(lo: Int): Stream[Int] =
      if (lo >= end) empty
      else cons(lo, loop(step(lo)))
    loop(start)
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
    cons(start, from(start+step, step))

  /**
   * Create an infinite stream starting at <code>start</code>
   * and incrementing by 1.
   *
   * @param start the start value of the stream
   * @return the stream starting at value <code>start</code>.
   */
  def from(start: Int): Stream[Int] = from(start, 1)

  /**
   * Create an infinite stream containing the given element.
   *
   * @param elem the element composing the resulting stream
   * @return the stream containing an inifinite number of elem
   */
  def const[A](elem: A): Stream[A] = cons(elem, const(elem))

  /** Create a stream containing several copies of an element.
   *
   *  @param n    the length of the resulting stream
   *  @param elem the element composing the resulting stream
   *  @return     the stream composed of n elements all equal to elem
   */
  def make[A](n: Int, elem: A): Stream[A] =
    Stream.const(elem) take n
}

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
trait Stream[+A] extends Seq.Projection[A] {

  /** is this stream empty? */
  override def isEmpty: Boolean

  override def force : List[A] = toList

  /** The first element of this stream
   *  @throws Predef.NoSuchElementException if the stream is empty.
   */
  def head: A

  /** A stream consisting of the remaining elements of this stream after the first one.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   */
  def tail: Stream[A]

  /** The length of this stream */
  override def length: Int = {
    var len = 0
    var here = this
    while (!here.isEmpty) {
      len += 1
      here = here.tail
    }
    len
  }

  /** returns length - l without calling length
   */
  override def lengthCompare(l: Int) = {
    if (isEmpty) 0 - l
    else if (l <= 0) 1
    else tail.lengthCompare(l - 1)
  }

  override def hasDefiniteSize = false


  /** The stream resulting from the concatenation of this stream with the argument stream.
   *  @param rest   The stream that gets appended to this stream
   */
  override def append[B >: A](rest: => Iterable[B]): Stream[B] =
    if (isEmpty) rest.toStream else Stream.cons(head, tail append rest)


  /** An iterator returning the elements of this stream one by one.
   */
  override def elements: Iterator[A] = new Iterator[A] {
    var current = Stream.this
    def hasNext: Boolean = !current.isEmpty
    def next: A = { val result = current.head; current = current.tail; result }
  }

  /** The stream without its last element.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   */
  def init: Stream[A] =
    if (isEmpty) throw new UnsupportedOperationException("Stream.empty.init")
    else if (tail.isEmpty) Stream.empty
    else Stream.cons(head, tail.init)

  /** Returns the last element of this stream.
   *
   *  @return the last element of the stream.
   *  @throws Predef.NoSuchElementException if the stream is empty.
   */
  override def last: A =
    if (isEmpty) throw new NoSuchElementException("Stream.empty.last")
    else {
      def loop(s: Stream[A]): A = {
        if (s.tail.isEmpty) s.head
        else loop(s.tail)
      }
      loop(this)
    }

  /** Returns the <code>n</code>-th element of this stream. The first element
   *  (head of the stream) is at position 0.
   *
   *  @param n index of the element to return
   *  @return  the element at position <code>n</code> in this stream.
   *  @throws Predef.NoSuchElementException if the stream is too short.
   */
  override def apply(n: Int): A = drop(n).head

  /** Returns the <code>n</code> first elements of this stream, or else the whole
   *  stream, if it has less than <code>n</code> elements.
   *
   *  @param n the number of elements to take.
   *  @return the <code>n</code> first elements of this stream.
   */
  override def take(n: Int): Stream[A] =
    if (isEmpty || n <= 0) Stream.empty
    else Stream.cons(head, tail take (n-1))

  /** Returns the stream without its <code>n</code> first elements.
   *  If the stream has less than <code>n</code> elements, the empty stream is returned.
   *
   *  @param n the number of elements to drop.
   *  @return the stream without its <code>n</code> first elements.
   */
  override def drop(n: Int): Stream[A] = {
    def loop(s: Stream[A], n: Int): Stream[A] =
      if (s.isEmpty || n <= 0) s
      else loop(s.tail, n-1)
    loop(this, n)
  }


  /** Returns the longest prefix of this stream whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest prefix of this stream whose elements satisfy
   *           the predicate <code>p</code>.
   */
  override def takeWhile(p: A => Boolean): Stream[A] =
    if (isEmpty || !p(head)) Stream.empty
    else Stream.cons(head, tail takeWhile p)

  /** Returns the longest suffix of this stream whose first element
   *  does not satisfy the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  the longest suffix of the stream whose first element
   *           does not satisfy the predicate <code>p</code>.
   */
  override def dropWhile(p: A => Boolean): Stream[A] = {
    def loop(s: Stream[A]): Stream[A] =
      if (s.isEmpty || !p(s.head)) s
      else loop(s.tail)
    loop(this)
  }

  /** Returns the stream resulting from applying the given function <code>f</code> to each
   *  element of this stream.
   *
   *  @param f function to apply to each element.
   *  @return <code>[f(a0), ..., f(an)]</code> if this stream is <code>[a0, ..., an]</code>.
   */
  override def map[B](f: A => B): Stream[B] =
    if (isEmpty) Stream.empty
    else Stream.cons(f(head), tail map f)

  /** Apply the given function <code>f</code> to each element of this stream
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   */
  override def foreach(f: A => Unit) {
    def loop(s: Stream[A]) {
      if (s.isEmpty) {}
      else { f(s.head); loop(s.tail) }
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
    def loop(s: Stream[A]): Stream[A] =
      if (s.isEmpty) s
      else if (p(s.head)) Stream.cons(s.head, loop(s.tail))
      else loop(s.tail)
    loop(this)
  }

  /** Tests if the predicate <code>p</code> is satisfied by all elements
   *  in this stream.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff all elements of this stream satisfy the
   *           predicate <code>p</code>.
   */
  override def forall(p: A => Boolean): Boolean = {
    def loop(s: Stream[A]): Boolean =
      if (s.isEmpty) true
      else if (p(s.head)) loop(s.tail)
      else false
    loop(this)
  }

  /** Tests the existence in this stream of an element that satisfies the
   *  predicate <code>p</code>.
   *
   *  @param p the test predicate.
   *  @return  <code>true</code> iff there exists an element in this stream that
   *           satisfies the predicate <code>p</code>.
   */
  override def exists(p: A => Boolean): Boolean = {
    def loop(s: Stream[A]): Boolean =
      if (s.isEmpty) false
      else if (p(s.head)) true
      else loop(s.tail)
    loop(this)
  }

  /** Combines the elements of this stream together using the binary
   *  function <code>f</code>, from left to right, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(... (f(f(z, a<sub>0</sub>), a<sub>1</sub>) ...),
   *          a<sub>n</sub>)</code> if the stream is
   *          <code>[a<sub>0</sub>, a<sub>1</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def foldLeft[B](z: B)(f: (B, A) => B): B = {
    def loop(s: Stream[A], z: B): B =
      if (s.isEmpty) z
      else loop(s.tail, f(z, s.head))
    loop(this, z)
  }

  /** Combines the elements of this stream together using the binary
   *  function <code>f</code>, from rigth to left, and starting with
   *  the value <code>z</code>.
   *
   *  @return <code>f(a<sub>0</sub>, f(a<sub>1</sub>, f(..., f(a<sub>n</sub>, z)...)))</code>
   *          if the stream is <code>[a<sub>0</sub>, a1, ..., a<sub>n</sub>]</code>.
   */
  override def foldRight[B](z: B)(f: (A, B) => B): B =
    if (isEmpty) z
    else f(head, tail.foldRight(z)(f))

  /** Applies the given function <code>f</code> to each element of
   *  this stream, then concatenates the results.
   *
   *  @param f the function to apply on each element.
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this stream is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  override def flatMap[B](f: A => Iterable[B]): Stream[B] =
    if (isEmpty) Stream.empty
    else {
      val s: Stream[B] = f(head) match {
        case x: Stream[_] => x
        case y: List[_]   => y.toStream
        case z            => z.toList.toStream
      }
      s append (tail flatMap f)
    }

  override def toStream = this

  /** A stream consisting of all elements of this stream in reverse order.
   */
  override def reverse: Stream[A] =
    foldLeft(Stream.empty: Stream[A])((xs, x) => Stream.cons(x, xs))

  /** Fills the given array <code>xs</code> with the elements of
   *  this stream starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @pre    the array must be large enough to hold all elements.
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int) {
    def loop(s: Stream[A], start: Int) {
      if (!s.isEmpty) { xs(start) = s.head; loop(s.tail, start + 1) }
    }
    loop(this, start)
  }

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
    if (this.isEmpty || that.isEmpty) Stream.empty
    else Stream.cons((this.head, that.head), this.tail zip that.tail)


  /** Returns a stream that pairs each element of this stream
   *  with its index, counting from 0.
   *
   *  @return      the stream <code>Stream({a<sub>0</sub>,0}, {a<sub>0</sub>,1},...)</code>
   *               where <code>a<sub>i</sub></code> are the elements of this stream.
   */
  def zipWithIndex: Stream[(A, Int)] =
    zip(Stream.from(0))

  /** Prints elements of this stream one by one, separated by commas */
  def print { print(", ") }

  /** Prints elements of this stream one by one, separated by <code>sep</code>
   *  @param sep   The separator string printed between consecutive elements.
   */
  def print(sep: String) {
    def loop(s: Stream[A]) {
      if (s.isEmpty) Console.println("Stream.empty")
      else { Console.print(s.head); Console.print(sep); loop(s.tail) }
    }
    loop(this)
  }

  /** Converts stream to string */
  override def toString =
    "Stream(" + addDefinedElems(new StringBuilder(), "") + ")"

  /** Write all elements of this string into given string builder */
  protected def addDefinedElems(buf: StringBuilder, prefix: String): StringBuilder
}
