/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import mutable.{ Buffer, ListBuffer, ArrayBuffer }
import annotation.unchecked.{ uncheckedVariance => uV }

/** A template trait for collections which can be traversed either once only
 *  or one or more times.
 *  $traversableonceinfo
 *
 *  @tparam A    the element type of the collection
 *
 *  @author Martin Odersky
 *  @author Paul Phillips
 *  @version 2.8
 *  @since   2.8
 *
 *  @define coll traversable or iterator
 *
 *  @tparam A    the element type of the collection
 *
 *  @define traversableonceinfo
 *  This trait exists primarily to eliminate code duplication between
 *  `Iterator` and `Traversable`, and thus implements some of the common
 *  methods that can be implemented solely in terms of foreach without
 *  access to a `Builder`. It also includes a number of abstract methods
 *  whose implementations are provided by `Iterator`, `Traversable`, etc.
 *  It contains implementations common to `Iterators` and
 *  `Traversables`, such as folds, conversions, and other operations which
 *  traverse some or all of the elements and return a derived value.
 *  Directly subclassing `TraversableOnce` is not recommended - instead,
 *  consider declaring an `Iterator` with a `next` and `hasNext` method,
 *  creating an `Iterator` with one of the methods on the `Iterator` object,
 *  or declaring a subclass of `Traversable`.
 *
 *  @define coll traversable or iterator
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the
 *    underlying collection type is ordered or the operator is associative
 *    and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 */
trait TraversableOnce[+A] extends Any with GenTraversableOnce[A] {
  self =>

  /** Self-documenting abstract methods. */
  def foreach[U](f: A => U): Unit
  def isEmpty: Boolean
  def hasDefiniteSize: Boolean

  // Note: We could redefine this in TraversableLike to always return `repr`
  // of type `Repr`, only if `Repr` had type bounds, which it doesn't, because
  // not all `Repr` are a subtype `TraversableOnce[A]`.
  // The alternative is redefining it for maps, sets and seqs. For concrete implementations
  // we don't have to do this anyway, since they are leaves in the inheritance hierarchy.
  // Note 2: This is implemented in all collections _not_ inheriting `Traversable[A]`
  //         at least indirectly. Currently, these are `ArrayOps` and `StringOps`.
  //         It is also implemented in `TraversableOnce[A]`.
  /** A version of this collection with all
   *  of the operations implemented sequentially (i.e. in a single-threaded manner).
   *
   *  This method returns a reference to this collection. In parallel collections,
   *  it is redefined to return a sequential implementation of this collection. In
   *  both cases, it has O(1) complexity.
   *
   *  @return a sequential view of the collection.
   */
  def seq: TraversableOnce[A]

  /** Presently these are abstract because the Traversable versions use
   *  breakable/break, and I wasn't sure enough of how that's supposed to
   *  function to consolidate them with the Iterator versions.
   */
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  def find(p: A => Boolean): Option[A]
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  // for internal use
  protected[this] def reversed = {
    var elems: List[A] = Nil
    self.seq foreach (elems ::= _)
    elems
  }

  def size: Int = {
    var result = 0
    for (x <- self) result += 1
    result
  }

  def nonEmpty: Boolean = !isEmpty

  def count(p: A => Boolean): Int = {
    var cnt = 0
    for (x <- this)
      if (p(x)) cnt += 1

    cnt
  }

  /** Finds the first element of the $coll for which the given partial
   *  function is defined, and applies the partial function to it.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param pf   the partial function
   *  @return     an option value containing pf applied to the first
   *              value for which it is defined, or `None` if none exists.
   *  @example    `Seq("a", 1, 5L).collectFirst({ case x: Int => x*10 }) = Some(10)`
   */
  def collectFirst[B](pf: PartialFunction[A, B]): Option[B] = {
    for (x <- self.toIterator) { // make sure to use an iterator or `seq`
      if (pf isDefinedAt x)
        return Some(pf(x))
    }
    None
  }

  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    this.seq foreach (x => result = op(result, x))
    result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    reversed.foldLeft(z)((x, y) => op(y, x))

  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

    for (x <- self) {
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.reduceRight")

    reversed.reduceLeft[B]((x, y) => op(y, x))
  }

  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (isEmpty) None else Some(reduceLeft(op))

  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    if (isEmpty) None else Some(reduceRight(op))

  def reduce[A1 >: A](op: (A1, A1) => A1): A1 = reduceLeft(op)

  def reduceOption[A1 >: A](op: (A1, A1) => A1): Option[A1] = reduceLeftOption(op)

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  def aggregate[B](z: B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

  def sum[B >: A](implicit num: Numeric[B]): B = foldLeft(num.zero)(num.plus)

  def product[B >: A](implicit num: Numeric[B]): B = foldLeft(num.one)(num.times)

  def min[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")

    reduceLeft((x, y) => if (cmp.lteq(x, y)) x else y)
  }

  def max[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")

    reduceLeft((x, y) => if (cmp.gteq(x, y)) x else y)
  }

  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    reduceLeft((x, y) => if (cmp.gteq(f(x), f(y))) x else y)
  }
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    reduceLeft((x, y) => if (cmp.lteq(f(x), f(y))) x else y)
  }

  /** Copies all elements of this $coll to a buffer.
   *  $willNotTerminateInf
   *  @param  dest   The buffer to which elements are copied.
   */
  def copyToBuffer[B >: A](dest: Buffer[B]): Unit = dest ++= seq

  def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    copyToArray(xs, start, xs.length - start)

  def copyToArray[B >: A](xs: Array[B]): Unit =
    copyToArray(xs, 0, xs.length)

  def toArray[B >: A : ClassManifest]: Array[B] = {
    if (isTraversableAgain) {
      val result = new Array[B](size)
      copyToArray(result, 0)
      result
    }
    else toBuffer.toArray
  }

  def toTraversable: Traversable[A]

  def toList: List[A] = new ListBuffer[A] ++= seq toList

  def toIterable: Iterable[A] = toStream

  def toSeq: Seq[A] = toStream

  def toIndexedSeq: immutable.IndexedSeq[A] = immutable.IndexedSeq() ++ seq

  def toBuffer[B >: A]: mutable.Buffer[B] = new ArrayBuffer[B] ++= seq

  def toSet[B >: A]: immutable.Set[B] = immutable.Set() ++ seq

  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    for (x <- self)
      b += x

    b.result
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
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> a.addString(b, "LinkedList(", ", ", ")")
   *      res1: StringBuilder = LinkedList(1, 2, 3, 4)
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

    b append start
    for (x <- self) {
      if (first) {
        b append x
        first = false
      }
      else {
        b append sep
        b append x
      }
    }
    b append end

    b
  }

  /** Appends all elements of this $coll to a string builder using a separator string.
   *  The written text consists of the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll, separated by the string `sep`.
   *
   * Example:
   *
   * {{{
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> a.addString(b, ", ")
   *      res0: StringBuilder = 1, 2, 3, 4
   * }}}
   *
   *  @param  b    the string builder to which elements are appended.
   *  @param sep   the separator string.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  /** Appends all elements of this $coll to a string builder.
   *  The written text consists of the string representations (w.r.t. the method
   * `toString`) of all elements of this $coll without any separator string.
   *
   * Example:
   *
   * {{{
   *      scala> val a = LinkedList(1,2,3,4)
   *      a: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> val h = a.addString(b)
   *      b: StringBuilder = 1234
   * }}}

   *  @param  b    the string builder to which elements are appended.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")
}



object TraversableOnce {
  implicit def traversableOnceCanBuildFrom[T] = new OnceCanBuildFrom[T]
  implicit def wrapTraversableOnce[A](trav: TraversableOnce[A]) = new MonadOps(trav)
  implicit def alternateImplicit[A](trav: TraversableOnce[A]) = new ForceImplicitAmbiguity
  implicit def flattenTraversableOnce[A, CC[_]](travs: TraversableOnce[CC[A]])(implicit ev: CC[A] => TraversableOnce[A]) =
    new FlattenOps[A](travs map ev)

  /** With the advent of TraversableOnce, it can be useful to have a builder which
   *  operates on Iterators so they can be treated uniformly along with the collections.
   *  See scala.util.Random.shuffle for an example.
   */
  class OnceCanBuildFrom[A] extends generic.CanBuildFrom[TraversableOnce[A], A, TraversableOnce[A]] {
    def newIterator = new ArrayBuffer[A] mapResult (_.iterator)

    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: TraversableOnce[A]) = newIterator

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newIterator
  }

  class FlattenOps[A](travs: TraversableOnce[TraversableOnce[A]]) {
    def flatten: Iterator[A] = new AbstractIterator[A] {
      val its = travs.toIterator
      private var it: Iterator[A] = Iterator.empty
      def hasNext: Boolean = it.hasNext || its.hasNext && { it = its.next.toIterator; hasNext }
      def next(): A = if (hasNext) it.next() else Iterator.empty.next()
    }
  }

  class ForceImplicitAmbiguity

  class MonadOps[+A](trav: TraversableOnce[A]) {
    def map[B](f: A => B): TraversableOnce[B] = trav.toIterator map f
    def flatMap[B](f: A => GenTraversableOnce[B]): TraversableOnce[B] = trav.toIterator flatMap f
    def withFilter(p: A => Boolean) = trav.toIterator filter p
    def filter(p: A => Boolean): TraversableOnce[A] = withFilter(p)
  }
}
