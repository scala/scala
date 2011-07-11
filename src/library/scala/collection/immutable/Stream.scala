/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package immutable

import generic._
import mutable.{Builder, StringBuilder, LazyBuilder, ListBuffer}
import scala.annotation.tailrec
import Stream.cons

/** The class `Stream` implements lazy lists where elements
 *  are only evaluated when they are needed. Here is an example:
 *
 *  {{{
 *  object Main extends Application {
 *
 *    def from(n: Int): Stream[Int] =
 *      Stream.cons(n, from(n + 1))
 *
 *    def sieve(s: Stream[Int]): Stream[Int] =
 *      Stream.cons(s.head, sieve(s.tail filter { _ % s.head != 0 }))
 *
 *    def primes = sieve(from(2))
 *
 *    primes take 10 print
 *  }
 *  }}}
 *
 *  @tparam A    the type of the elements contained in this stream.
 *
 *  @author Martin Odersky, Matthias Zenger
 *  @version 1.1 08/08/03
 *  @since   2.8
 *  @define Coll Stream
 *  @define coll stream
 *  @define orderDependent
 *  @define orderDependentFold
 */
abstract class Stream[+A] extends LinearSeq[A]
                             with GenericTraversableTemplate[A, Stream]
                             with LinearSeqOptimized[A, Stream[A]] {
self =>
  override def companion: GenericCompanion[Stream] = Stream

  import scala.collection.{Traversable, Iterable, Seq, IndexedSeq}

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
   *  @return       The stream containing elements of this stream and the traversable object.
   */
  def append[B >: A](rest: => TraversableOnce[B]): Stream[B] =
    if (isEmpty) rest.toStream else cons(head, tail append rest)

  /** Forces evaluation of the whole stream and returns it. */
  def force: Stream[A] = {
    var these = this
    while (!these.isEmpty) these = these.tail
    this
  }

  /** Prints elements of this stream one by one, separated by commas. */
  def print() { print(", ") }

  /** Prints elements of this stream one by one, separated by `sep`.
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

  override def length: Int = {
    var len = 0
    var left = this
    while (!left.isEmpty) {
      len += 1
      left = left.tail
    }
    len
  }

  /** It's an imperfect world, but at least we can bottle up the
   *  imperfection in a capsule.
   */
  @inline private def asThat[That](x: AnyRef): That     = x.asInstanceOf[That]
  @inline private def asStream[B](x: AnyRef): Stream[B] = x.asInstanceOf[Stream[B]]
  @inline private def isStreamBuilder[B, That](bf: CanBuildFrom[Stream[A], B, That]) =
    bf(repr).isInstanceOf[Stream.StreamBuilder[_]]

  // Overridden methods from Traversable

  override def toStream: Stream[A] = this

  override def hasDefiniteSize = {
    def loop(s: Stream[A]): Boolean = s.isEmpty || s.tailDefined && loop(s.tail)
    loop(this)
  }

  /** Create a new stream which contains all elements of this stream
   *  followed by all elements of Traversable `that`.
   *  @note It's subtle why this works. We know that if the target type
   *  of the Builder That is either a Stream, or one of its supertypes, or undefined,
   *  then StreamBuilder will be chosen for the implicit.
   *  we recognize that fact and optimize to get more laziness.
   */
  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[A]
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) that.toStream
      else cons(head, asStream[A](tail ++ that))
    )
    else super.++(that)(bf)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(cons(elem, this))
    else super.+:(elem)(bf)

  /**
   * Create a new stream which contains all intermediate results of applying the operator
   * to subsequent elements left to right.
   * @note This works because the target type of the Builder That is a Stream.
   */
  override final def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream(z)
      else cons(z, asStream[B](tail.scanLeft(op(z, head))(op)))
    )
    else super.scanLeft(z)(op)(bf)

  /** Returns the stream resulting from applying the given function
   *  `f` to each element of this stream.
   *
   *  @param f function to apply to each element.
   *  @return  <code>f(a<sub>0</sub>), ..., f(a<sub>n</sub>)</code> if this
   *           sequence is <code>a<sub>0</sub>, ..., a<sub>n</sub></code>.
   */
  override final def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream.Empty
      else cons(f(head), asStream[B](tail map f))
    )
    else super.map(f)(bf)
  }

  override final def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (!isStreamBuilder(bf)) super.collect(pf)(bf)
    else {
      // this implementation avoids:
      // 1) stackoverflows (could be achieved with tailrec, too)
      // 2) out of memory errors for big streams (`this` reference can be eliminated from the stack)
      var rest: Stream[A] = this
      while (rest.nonEmpty && !pf.isDefinedAt(rest.head)) rest = rest.tail

      //  without the call to the companion object, a thunk is created for the tail of the new stream,
      //  and the closure of the thunk will reference `this`
      if (rest.isEmpty) Stream.Empty.asInstanceOf[That]
      else Stream.collectedTail(rest, pf, bf).asInstanceOf[That]
    }
  }

  /** Applies the given function `f` to each element of
   *  this stream, then concatenates the results.
   *
   *  @param f  the function to apply on each element.
   *  @param bf $bfinfo
   *  @return  <code>f(a<sub>0</sub>) ::: ... ::: f(a<sub>n</sub>)</code> if
   *           this stream is <code>[a<sub>0</sub>, ..., a<sub>n</sub>]</code>.
   */
  override final def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[B]
    // optimisations are not for speed, but for functionality
    // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream.Empty
      else {
        // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
        var nonEmptyPrefix = this
        var prefix = f(nonEmptyPrefix.head).toStream
        while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
          nonEmptyPrefix = nonEmptyPrefix.tail
          if(!nonEmptyPrefix.isEmpty)
            prefix = f(nonEmptyPrefix.head).toStream
        }

        if (nonEmptyPrefix.isEmpty) Stream.empty
        else prefix append asStream[B](nonEmptyPrefix.tail flatMap f)
      }
    )
    else super.flatMap(f)(bf)

  /** Returns all the elements of this stream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the stream.
   *  @return the elements of this stream satisfying <code>p</code>.
   */
  override def filter(p: A => Boolean): Stream[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest = this
    while (!rest.isEmpty && !p(rest.head)) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) Stream.filteredTail(rest, p)
    else Stream.Empty
  }

  override final def withFilter(p: A => Boolean): StreamWithFilter = new StreamWithFilter(p)

  /** A lazier implementation of WithFilter than TraversableLike's.
   */
  final class StreamWithFilter(p: A => Boolean) extends WithFilter(p) {

    override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
      def tailMap = asStream[B](tail withFilter p map f)
      if (isStreamBuilder(bf)) asThat(
        if (isEmpty) Stream.Empty
        else if (p(head)) cons(f(head), tailMap)
        else tailMap
      )
      else super.map(f)(bf)
    }

    override def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
      def tailFlatMap = asStream[B](tail withFilter p flatMap f)
      if (isStreamBuilder(bf)) asThat(
        if (isEmpty) Stream.Empty
        else if (p(head)) f(head).toStream append tailFlatMap
        else tailFlatMap
      )
      else super.flatMap(f)(bf)
    }

    override def foreach[B](f: A => B) =
      for (x <- self)
        if (p(x)) f(x)

    override def withFilter(q: A => Boolean): StreamWithFilter =
      new StreamWithFilter(x => p(x) && q(x))
  }

  /** A lazier Iterator than LinearSeqLike's. */
  override def iterator: Iterator[A] = new StreamIterator(self)

  /** Apply the given function <code>f</code> to each element of this linear sequence
   *  (while respecting the order of the elements).
   *
   *  @param f the treatment to apply to each element.
   *  @note  Overridden here as final to trigger tail-call optimization, which replaces
   *         'this' with 'tail' at each iteration. This is absolutely necessary
   *         for allowing the GC to collect the underlying stream as elements are
   *         consumed.
   */
  @tailrec
  override final def foreach[B](f: A => B) {
    if (!this.isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  /** Stream specialization of foldLeft which allows GC to collect
   *  along the way.
   */
  @tailrec
  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  /** Stream specialization of reduceLeft which allows GC to collect
   *  along the way.
   */
  override final def reduceLeft[B >: A](f: (B, A) => B): B = {
    if (this.isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else {
      var reducedRes: B = this.head
      var left = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  /** Returns all the elements of this stream that satisfy the
   *  predicate <code>p</code>. The order of the elements is preserved.
   *
   *  @param p the predicate used to filter the stream.
   *  @return the elements of this stream satisfying <code>p</code>.
   */
  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), filterNot(p(_)))

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
  override final def zip[A1 >: A, B, That](that: collection.GenIterable[B])(implicit bf: CanBuildFrom[Stream[A], (A1, B), That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[(A1, B)]
    if (isStreamBuilder(bf)) asThat(
      if (this.isEmpty || that.isEmpty) Stream.Empty
      else cons((this.head, that.head), asStream[(A1, B)](this.tail zip that.tail))
    )
    else super.zip(that)(bf)

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to
   *  `s zip s.indices`
   */
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Stream[A], (A1, Int), That]): That =
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

  override def mkString(sep: String): String = mkString("", sep, "")
  override def mkString: String = mkString("")
  override def mkString(start: String, sep: String, end: String): String = {
    this.force
    super.mkString(start, sep, end)
  }
  override def toString = super.mkString(stringPrefix + "(", ", ", ")")

  override def splitAt(n: Int): (Stream[A], Stream[A]) = (take(n), drop(n))

  /** Returns the <code>n</code> first elements of this stream, or else the whole
   *  stream, if it has less than <code>n</code> elements.
   *
   *  @param n the number of elements to take.
   *  @return the <code>n</code> first elements of this stream.
   */
  override def take(n: Int): Stream[A] =
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) cons(head, Stream.empty)
    else cons(head, tail take n-1)

  @tailrec final override def drop(n: Int): Stream[A] =
    if (n <= 0 || isEmpty) this
    else tail drop n-1

  /** A substream starting at index `from` and extending up to (but not including)
   *  index `until`.
   *
   *  @param start   The index of the first element of the returned subsequence
   *  @param end     The index of the element following the returned subsequence
   */
  override def slice(from: Int, until: Int): Stream[A] = {
    val lo = from max 0
    if (until <= lo || isEmpty) Stream.empty
    else this drop lo take (until - lo)
  }

  /** The stream without its last element.
   *  @throws Predef.UnsupportedOperationException if the stream is empty.
   */
  override def init: Stream[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) Stream.Empty
    else cons(head, tail.init)

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

  // there's nothing we can do about dropRight, so we just keep the definition in LinearSeq

  /** Returns the longest prefix of this stream whose elements satisfy
   *  the predicate <code>p</code>.
   *
   *  @param p the test predicate.
   */
  override def takeWhile(p: A => Boolean): Stream[A] =
    if (!isEmpty && p(head)) cons(head, tail takeWhile p)
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
  override def distinct: Stream[A] =
    if (isEmpty) this
    else cons(head, tail.filter(head !=).distinct)

  /** Returns a new sequence of given length containing the elements of this sequence followed by zero
   *  or more occurrences of given elements.
   */
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    def loop(len: Int, these: Stream[A]): Stream[B] =
      if (these.isEmpty) Stream.fill(len)(elem)
      else cons(these.head, loop(len - 1, these.tail))

    if (isStreamBuilder(bf)) asThat(loop(len, this))
    else super.padTo(len, elem)(bf)
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

  override def flatten[B](implicit asTraversable: A => /*<:<!!!*/ GenTraversableOnce[B]): Stream[B] = {
    def flatten1(t: Traversable[B]): Stream[B] =
      if (!t.isEmpty)
        cons(t.head, flatten1(t.tail))
      else
        tail.flatten

    if (isEmpty) Stream.empty
    else flatten1(asTraversable(head).seq.toTraversable)
  }

  override def view = new StreamView[A, Stream[A]] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  /** Defines the prefix of this object's `toString` representation as `Stream`.
   */
  override def stringPrefix = "Stream"

}

/** See #3273 and test case run/bug3273 for motivation. */
final class StreamIterator[+A](self: Stream[A]) extends Iterator[A] {
  // A call-by-need cell.
  class LazyCell(st: => Stream[A]) {
    lazy val v = st
  }

  private var these = new LazyCell(self)
  def hasNext: Boolean = these.v.nonEmpty
  def next: A =
    if (isEmpty) Iterator.empty.next
    else {
      val cur    = these.v
      val result = cur.head
      these = new LazyCell(cur.tail)
      result
    }
  override def toStream = {
    val result = these.v
    these = new LazyCell(Stream.empty)
    result
  }
  override def toList   = toStream.toList
}

/**
 * The object <code>Stream</code> provides helper functions
 * to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 * @since   2.8
 */
object Stream extends SeqFactory[Stream] {

  /** The factory for streams.
   *  @note Methods such as map/flatMap will not invoke the Builder factory,
   *        but will return a new stream directly, to preserve laziness.
   *        The new stream is then cast to the factory's result type.
   *        This means that every CanBuildFrom that takes a
   *        Stream as its From type parameter must yield a stream as its result parameter.
   *        If that assumption is broken, cast errors might result.
   */
  class StreamCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Stream[A]] = new StreamCanBuildFrom[A]

  /** Creates a new builder for a stream */
  def newBuilder[A]: Builder[A, Stream[A]] = new StreamBuilder[A]

  import scala.collection.{Iterable, Seq, IndexedSeq}

  /** A builder for streams
   *  @note This builder is lazy only in the sense that it does not go downs the spine
   *        of traversables that are added as a whole. If more laziness can be achieved,
   *        this builder should be bypassed.
   */
  class StreamBuilder[A] extends scala.collection.mutable.LazyBuilder[A, Stream[A]] {
    def result: Stream[A] = parts.toStream flatMap (_.toStream)
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
    def #::(hd: A): Stream[A] = cons(hd, tl)
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

  @deprecated("use #:: instead", "2.8.0")
  lazy val lazy_:: = #::

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
  @SerialVersionUID(-602202424901551803L)
  final class Cons[+A](hd: A, tl: => Stream[A]) extends Stream[A] with Serializable {
    override def isEmpty = false
    override def head = hd
    @volatile private[this] var tlVal: Stream[A] = _
    def tailDefined: Boolean = tlVal ne null
    override def tail: Stream[A] = {
      if (!tailDefined)
        synchronized {
          if (!tailDefined) tlVal = tl
        }

      tlVal
    }
  }

  /** An infinite stream that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the stream
   *  @param f     the function that's repeatedly applied
   *  @return      the stream returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A)(f: A => A): Stream[A] = cons(start, iterate(f(start))(f))

  override def iterate[A](start: A, len: Int)(f: A => A): Stream[A] =
    iterate(start)(f) take len

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
   * Create an infinite stream containing the given element expression (which is computed for each
   * occurrence)
   *
   * @param elem the element composing the resulting stream
   * @return the stream containing an infinite number of elem
   */
  def continually[A](elem: => A): Stream[A] = cons(elem, continually(elem))

  override def fill[A](n: Int)(elem: => A): Stream[A] =
    if (n <= 0) Empty else cons(elem, fill(n-1)(elem))

  override def tabulate[A](n: Int)(f: Int => A): Stream[A] = {
    def loop(i: Int): Stream[A] =
      if (i >= n) Empty else cons(f(i), loop(i+1))
    loop(0)
  }

  override def range[T: Integral](start: T, end: T, step: T): Stream[T] = {
    val num = implicitly[Integral[T]]
    import num._

    if (if (step < zero) start <= end else end <= start) Empty
    else cons(start, range(start + step, end, step))
  }

  private[immutable] def filteredTail[A](stream: Stream[A], p: A => Boolean) = {
    cons(stream.head, stream.tail filter p)
  }

  private[immutable] def collectedTail[A, B, That](stream: Stream[A], pf: PartialFunction[A, B], bf: CanBuildFrom[Stream[A], B, That]) = {
    cons(pf(stream.head), stream.tail.collect(pf)(bf).asInstanceOf[Stream[B]])
  }

  /** A stream containing all elements of a given iterator, in the order they are produced.
   *  @param it   The iterator producing the stream's elements
   */
  @deprecated("use it.toStream instead", "2.8.0")
  def fromIterator[A](it: Iterator[A]): Stream[A] = it.toStream

  /** The concatenation of a sequence of streams
   */
  @deprecated("use xs.flatten instead", "2.8.0")
  def concat[A](xs: Iterable[Stream[A]]): Stream[A] = concat(xs.iterator)

  /** The concatenation of all streams returned by an iterator
   */
  @deprecated("use xs.toStream.flatten instead", "2.8.0")
  def concat[A](xs: Iterator[Stream[A]]): Stream[A] = xs.toStream.flatten //(conforms[Stream[A], scala.collection.Traversable[A]])

  /**
   * Create a stream with element values
   * <code>v<sub>n+1</sub> = step(v<sub>n</sub>)</code>
   * where <code>v<sub>0</sub> = start</code>
   * and elements are in the range between <code>start</code> (inclusive)
   * and <code>end</code> (exclusive)
   * @param start the start value of the stream
   * @param end the end value of the stream
   * @param step the increment function of the stream, must be monotonically increasing or decreasing
   * @return the stream starting at value <code>start</code>.
   */
  @deprecated("use `iterate` instead.", "2.8.0")
  def range(start: Int, end: Int, step: Int => Int): Stream[Int] =
    iterate(start, end - start)(step)

  /**
   * Create an infinite stream containing the given element.
   *
   * @param elem the element composing the resulting stream
   * @return the stream containing an infinite number of elem
   */
  @deprecated("use `continually` instead", "2.8.0")
  def const[A](elem: A): Stream[A] = cons(elem, const(elem))

  /** Create a stream containing several copies of an element.
   *
   *  @param n    the length of the resulting stream
   *  @param elem the element composing the resulting stream
   *  @return     the stream composed of n elements all equal to elem
   */
  @deprecated("use fill(n, elem) instead", "2.8.0")
  def make[A](n: Int, elem: A): Stream[A] = fill(n)(elem)
}


