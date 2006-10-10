/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import compat.StringBuilder

/**
 * The object <code>Stream</code> provides helper functions
 * to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 */
object Stream {

  val empty: Stream[Nothing] = new Stream[Nothing] {
    override def isEmpty = true
    def head: Nothing = throw new java.util.NoSuchElementException("head of empty stream")
    def tail: Stream[Nothing] = throw new java.util.NoSuchElementException("tail of empty stream")
    def printElems(buf: StringBuilder, prefix: String): StringBuilder = buf
  }

  def cons[a](hd: a, tl: => Stream[a]) = new Stream[a] {
    override def isEmpty = false
    def head = hd
    private var tlVal: Stream[a] = _
    private var tlDefined = false
    def tail: Stream[a] = {
      if (!tlDefined) { tlVal = tl; tlDefined = true }
      tlVal
    }
    def printElems(buf: StringBuilder, prefix: String): StringBuilder = {
      val buf1 = buf.append(prefix).append(hd)
      if (tlDefined) tlVal.printElems(buf1, ", ") else buf1 append ", ?"
    }
  }

  def fromIterator[a](it: Iterator[a]): Stream[a] =
    if (it.hasNext) cons(it.next, fromIterator(it)) else empty

  def concat[a](xs: Seq[Stream[a]]): Stream[a] = concat(xs.elements)

  def concat[a](xs: Iterator[Stream[a]]): Stream[a] =
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
      else cons(lo, loop(lo + step));
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
      else cons(lo, loop(step(lo)));
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
 *     Stream.cons(s.head, sieve(s.tail filter { x => x % s.head != 0 }))
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
trait Stream[+a] extends Seq[a] {

  override def isEmpty: Boolean
  def head: a
  def tail: Stream[a]

  def length: Int = if (isEmpty) 0 else tail.length + 1

  def append[b >: a](rest: => Stream[b]): Stream[b] =
    if (isEmpty) rest
    else Stream.cons(head, tail.append(rest))

  def elements: Iterator[a] = new Iterator[a] {
    var current = Stream.this
    def hasNext: boolean = !current.isEmpty
    def next: a = { val result = current.head; current = current.tail; result }
  }

  def init: Stream[a] =
    if (isEmpty) throw new java.util.NoSuchElementException("Stream.empty.init")
    else if (tail.isEmpty) Stream.empty
    else Stream.cons(head, tail.init)

  def last: a =
    if (isEmpty) throw new java.util.NoSuchElementException("Stream.empty.last")
    else {
      def loop(s: Stream[a]): a = {
        if (s.tail.isEmpty) s.head
        else loop(s.tail)
      }
      loop(this)
    }

  override def take(n: Int): Stream[a] =
    if (n == 0) Stream.empty
    else Stream.cons(head, tail.take(n-1))

  override def drop(n: Int): Stream[a] = {
    def loop(s: Stream[a], n: Int): Stream[a] =
      if (n == 0) s
      else loop(s.tail, n-1)
    loop(this, n)
  }

  def apply(n: Int) = drop(n).head
  def at(n: Int) = drop(n).head

  def takeWhile(p: a => Boolean): Stream[a] =
    if (isEmpty || !p(head)) Stream.empty
    else Stream.cons(head, tail.takeWhile(p))

  def dropWhile(p: a => Boolean): Stream[a] = {
    def loop(s: Stream[a]): Stream[a] =
      if (s.isEmpty || !p(s.head)) this
      else loop(s.tail)
    loop(this)
  }

  def map[b](f: a => b): Stream[b] =
    if (isEmpty) Stream.empty
    else Stream.cons(f(head), tail.map(f))

  override def foreach(f: a => unit): unit = {
    def loop(s: Stream[a]): unit =
      if (s.isEmpty) {}
      else { f(s.head); loop(s.tail) }
    loop(this)
  }

  def filter(p: a => Boolean): Stream[a] = {
    def loop(s: Stream[a]): Stream[a] =
      if (s.isEmpty) s
      else if (p(s.head)) Stream.cons(s.head, loop(s.tail))
      else loop(s.tail)
    loop(this)
  }

  override def forall(p: a => Boolean): Boolean = {
    def loop(s: Stream[a]): Boolean = {
      if (s.isEmpty) true
      else if (p(s.head)) loop(s.tail)
      else false
    }
    loop(this)
  }

  override def exists(p: a => Boolean): Boolean = {
    def loop(s: Stream[a]): Boolean = {
      if (s.isEmpty) false
      else if (p(s.head)) true
      else loop(s.tail)
    }
    loop(this)
  }

  override def foldLeft[b](z: b)(f: (b, a) => b): b = {
    def loop(s: Stream[a], z: b): b =
      if (s.isEmpty) z
      else loop(s.tail, f(z, s.head))
    loop(this, z)
  }

  override def foldRight[b](z: b)(f: (a, b) => b): b =
    if (isEmpty) z
    else f(head, tail.foldRight(z)(f))

  def reduceLeft[b >: a](f: (b, b) => b): b =
    if (isEmpty) throw new java.util.NoSuchElementException("Stream.empty.reduceLeft")
    else ((tail: Stream[b]) foldLeft (head: b))(f)

  def reduceRight[b >: a](f: (b, b) => b): b =
    if (isEmpty) throw new java.util.NoSuchElementException("Stream.empty.reduceRight")
    else if (tail.isEmpty) head: b
    else f(head, tail.reduceRight(f))

  def flatMap[b](f: a => Stream[b]): Stream[b] =
    if (isEmpty) Stream.empty
    else f(head).append(tail.flatMap(f))

  def reverse: Stream[a] =
    foldLeft(Stream.empty: Stream[a])((xs, x) => Stream.cons(x, xs))

  // The following method is not compilable without run-time type
  // information. It should therefore be left commented-out for
  // now.
  //       def toArray: Array[a] = {
  //         val xs = new Array[a](length)
  //         copyToArray(xs, 0)
  //         xs
  //       }

  override def copyToArray[b >: a](xs: Array[b], start: Int): Array[b] = {
    def loop(s: Stream[a], start: Int): Array[b] =
      if (s.isEmpty) xs
      else { xs(start) = s.head; loop(s.tail, start + 1) }
    loop(this, start)
  }

  def zip[b](that: Stream[b]): Stream[Tuple2[a, b]] =
    if (this.isEmpty || that.isEmpty) Stream.empty
    else Stream.cons(Tuple2(this.head, that.head), this.tail.zip(that.tail))

  def zipWithIndex: Stream[Tuple2[a, Int]] =
    zip(Stream.from(0))

  def print: unit = {
    def loop(s: Stream[a]): unit =
      if (s.isEmpty) Console.println("Stream.empty")
      else { Console.print(s.head); Console.print(", "); loop(s.tail) }
    loop(this)
  }

  override def toString() =
    "Stream(" + printElems(new StringBuilder(), "") + ")"

  def printElems(buf: StringBuilder, prefix: String): StringBuilder
}
