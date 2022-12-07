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

import mutable.{ArrayBuffer, Buffer, Builder}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag
import scala.runtime.AbstractFunction1
import scala.collection.{ Iterable, IterableOnce }

/** A template trait for collections which can be traversed either once only
 *  or one or more times.
 *  $traversableonceinfo
 *
 *  @author Martin Odersky
 *  @author Paul Phillips
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
 *  consider declaring an `Iterator` with a `next` and `hasNext` method or
 *  creating an `Iterator` with one of the methods on the `Iterator` object.
 *  Consider declaring a subclass of `Traversable` instead if the elements
 *  can be traversed repeatedly.
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
trait IterableOnce[+A] extends Any with GenTraversableOnce[A] {
  self =>

  // A note on `isEmpty`: it is documented in GenTraversableOnce as required to
  // not consume any elements.  However, when (in scala/scala#8732) we tried
  // to add some `isEmpty` checks in this file for efficiency, we found that:
  // * in our own standard library, at least one subclass implemented
  //   `isEmpty` as `size == 0`, making it problematic to call `isEmpty`
  //   from `size` in this file
  // * in the community build, at least one repo had a subclass where `isEmpty`
  //   consumed elements, making it problematic to call `isEmpty` in this file
  //   from other methods such as `count`, `foldLeft`, and `addString`
  // Because it is now so late (2.12.11) in the 2.12.x series, we have chosen
  // to avoid adding `isEmpty` calls here, figuring that the breakage isn't
  // worth the presumably slight performance gain.  (And note that in 2.13.x,
  // `TraversableOnce` no longer even exists, and `IterableOnce#isEmpty` is
  // deprecated.)

  //TODO 2.12: Remove these methods. They are already defined in GenTraversableOnce
  /* Self-documenting abstract methods. */
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
   *  of the operations implemented sequentially (i.e., in a single-threaded manner).
   *
   *  This method returns a reference to this collection. In parallel collections,
   *  it is redefined to return a sequential implementation of this collection. In
   *  both cases, it has O(1) complexity.
   *
   *  @return a sequential view of the collection.
   */
  def seq: IterableOnceIterableOnce[A]

  // Presently these are abstract because the Traversable versions use
  // breakable/break, and I wasn't sure enough of how that's supposed to
  // function to consolidate them with the Iterator versions.
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  def find(p: A => Boolean): Option[A]
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  // for internal use
  protected[this] def reversed = {
    //avoid the LazyRef as we don't have an @eager object
    class reverser extends AbstractFunction1[A, Unit] {
      var elems: List[A] = Nil
      override def apply(v1: A): Unit = elems ::= v1
    }
    val reverser = new reverser
    IterableOnce foreach reverser
    reverser.elems
  }

  def size: Int = {
    //we can't guard with isEmpty as some implementation have
    // def isEmpty = size == 0

    //avoid the LazyRef as we don't have an @eager object
    class counter extends AbstractFunction1[A, Unit] {
      var result = 0
      override def apply(v1: A): Unit = result += 1
    }
    val counter = new counter
    IterableOnce foreach counter
    counter.result
  }

  def nonEmpty: Boolean = !isEmpty

  def count(p: A => Boolean): Int = {
    //avoid the LazyRef as we don't have an @eager object
    class counter extends AbstractFunction1[A, Unit] {
      var result = 0
      override def apply(v1: A): Unit = if (p(v1)) result += 1
    }
    val counter = new counter
    this foreach counter
    counter.result
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
    // TODO 2.12 -- move out alternate implementations into child classes
    val i: Iterator[A] = IterableOnce match {
      case it: Iterator[A] => it
      case _: GenIterable[_] => IterableOnce.toIterator   // If it might be parallel, be sure to .seq or use iterator!
      case _ =>                                   // Not parallel, not iterable--just traverse
        IterableOnce.foreach(pf.runWith(b => return Some(b)))
        return None
    }
    // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself
    // (Tested to be lower-overhead than runWith.  Would be better yet to not need to (formally) allocate it--change in 2.12.)
    val sentinel: Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any]{ def apply(a: A) = this }
    while (i.hasNext) {
      val x = pf.applyOrElse(i.next, sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) return Some(x.asInstanceOf[B])
    }
    None
  }

  @deprecated("Use foldLeft instead of /:", "2.12.10")
  def /:[B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  @deprecated("Use foldRight instead of :\\", "2.12.10")
  def :\[B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    //avoid the LazyRef as we don't have an @eager object
    class folder extends AbstractFunction1[A, Unit] {
      var result = z
      override def apply(v1: A): Unit = result = op(result,v1)
    }
    val folder = new folder
    this foreach folder
    folder.result
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    reversed.foldLeft(z)((x, y) => op(y, x))

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

    //avoid the LazyRef as we don't have an @eager object
    class reducer extends AbstractFunction1[A, Unit] {
      var first = true
      var acc: B = null.asInstanceOf[B]

      override def apply(x: A): Unit =
        if (first) {
          acc = x
          first = false
        }
        else acc = op(acc, x)
    }
    val reducer = new reducer
    IterableOnce foreach reducer
    reducer.acc
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

  def aggregate[B](z: =>B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

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

    //avoid the LazyRef as we don't have an @eager object
    class maxer extends AbstractFunction1[A, Unit] {
      var maxF: B = null.asInstanceOf[B]
      var maxElem: A = null.asInstanceOf[A]
      var first = true
      override def apply(elem: A): Unit = {
        val fx = f(elem)
        if (first || cmp.gt(fx, maxF)) {
          maxElem = elem
          maxF = fx
          first = false
        }
      }

    }
    val maxer = new maxer
    IterableOnce foreach maxer
    maxer.maxElem
  }
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    //avoid the LazyRef as we don't have an @eager object
    class miner extends AbstractFunction1[A, Unit] {
      var minF: B = null.asInstanceOf[B]
      var minElem: A = null.asInstanceOf[A]
      var first = true
      override def apply(elem: A): Unit = {
        val fx = f(elem)
        if (first || cmp.lt(fx, minF)) {
          minElem = elem
          minF = fx
          first = false
        }
      }

    }
    val miner = new miner
    IterableOnce foreach miner
    miner.minElem
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

  def toArray[B >: A : ClassTag]: Array[B] = {
    if (isTraversableAgain) {
      val result = new Array[B](size)
      copyToArray(result, 0)
      result
    }
    else toBuffer.toArray
  }

  def toTraversable: Iterable[A]

  def toList: List[A] = to[List]

  def toIterable: Iterable[A] = toStream

  def toSeq: Seq[A] = toStream

  def toIndexedSeq: immutable.IndexedSeq[A] = to[immutable.IndexedSeq]

  def toBuffer[B >: A]: mutable.Buffer[B] = to[ArrayBuffer].asInstanceOf[mutable.Buffer[B]]

  def toSet[B >: A]: immutable.Set[B] = to[immutable.Set].asInstanceOf[immutable.Set[B]]

  def toVector: Vector[A] = to[Vector]

  def to[Col[_]](implicit cbf: Factory[A, Col[A @uV]]): Col[A @uV] = {
    val b = cbf.newBuilder
    b ++= seq
    b.result()
  }

  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    b ++= seq.asInstanceOf[IterableOnceIterableOnce[(T, U)]]
    b.result()
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
    b append start

    class appender extends AbstractFunction1[A, Unit] {
      var first = true
      override def apply(x: A): Unit = {
        if (first) {
          b append x
          first = false
        }
        else {
          b append sep
          b append x
        }
      }
    }
    val appender = new appender
    IterableOnce foreach appender
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
   *      scala> val a = List(1,2,3,4)
   *      a: List[Int] = List(1, 2, 3, 4)
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
   *      scala> val a = List(1,2,3,4)
   *      a: List[Int] = List(1, 2, 3, 4)
   *
   *      scala> val b = new StringBuilder()
   *      b: StringBuilder =
   *
   *      scala> val h = a.addString(b)
   *      h: StringBuilder = 1234
   * }}}

   *  @param  b    the string builder to which elements are appended.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")
}


object IterableOnce {
  implicit def alternateImplicit[A](trav: IterableOnceIterableOnce[A]) = new ForceImplicitAmbiguity
  implicit def flattenTraversableOnce[A, CC[_]](travs: IterableOnceIterableOnce[CC[A]])(implicit ev: CC[A] => IterableOnceIterableOnce[A]) =
    new FlattenOps[A](travs map ev)

  /* Functionality reused in Iterator.CanBuildFrom */
  private[collection] abstract class BufferedCanBuildFrom[A, CC[X] <: IterableOnceIterableOnce[X]] extends generic.CanBuildFrom[CC[_], A, CC[A]] {
    def bufferToColl[B](buff: ArrayBuffer[B]): CC[B]
    def traversableToColl[B](t: GenTraversable[B]): CC[B]

    def newIterator: Builder[A, CC[A]] = new ArrayBuffer[A] mapResult bufferToColl

    /** Creates a new builder on request of a collection.
     *  @param from  the collection requesting the builder to be created.
     *  @return the result of invoking the `genericBuilder` method on `from`.
     */
    def apply(from: CC[_]): Builder[A, CC[A]] = from match {
      case xs: generic.GenericTraversableTemplate[_, _] => xs.genericBuilder.asInstanceOf[Builder[A, Iterable[A]]] mapResult {
        case res => traversableToColl(res.asInstanceOf[GenTraversable[A]])
      }
      case _ => newIterator
    }

    /** Creates a new builder from scratch
     *  @return the result of invoking the `newBuilder` method of this factory.
     */
    def apply() = newIterator
  }

  /** With the advent of `TraversableOnce`, it can be useful to have a builder which
   *  operates on `Iterator`s so they can be treated uniformly along with the collections.
   *  See `scala.util.Random.shuffle` or `scala.concurrent.Future.sequence` for an example.
   */
  class OnceCanBuildFrom[A] extends BufferedCanBuildFrom[A, IterableOnce] {
    def bufferToColl[B](buff: ArrayBuffer[B]) = buff.iterator
    def traversableToColl[B](t: GenTraversable[B]) = t.seq
  }

  /** Evidence for building collections from `TraversableOnce` collections */
  implicit def OnceCanBuildFrom[A] = new OnceCanBuildFrom[A]

  class FlattenOps[A](travs: IterableOnceIterableOnce[IterableOnceIterableOnce[A]]) {
    def flatten: Iterator[A] = new AbstractIterator[A] {
      val its = travs.toIterator
      private var it: Iterator[A] = Iterator.empty
      def hasNext: Boolean = it.hasNext || its.hasNext && { it = its.next().toIterator; hasNext }
      def next(): A = if (hasNext) it.next() else Iterator.empty.next()
    }
  }

  class ForceImplicitAmbiguity

  implicit class MonadOps[+A](trav: IterableOnceIterableOnce[A]) {
    def map[B](f: A => B): IterableOnceIterableOnce[B] = trav.toIterator map f
    def flatMap[B](f: A => GenTraversableOnce[B]): IterableOnceIterableOnce[B] = trav.toIterator flatMap f
    def withFilter(p: A => Boolean) = trav.toIterator filter p
    def filter(p: A => Boolean): IterableOnceIterableOnce[A] = withFilter(p)
  }
}
