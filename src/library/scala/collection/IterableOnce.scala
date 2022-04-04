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

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.StringBuilder
import scala.language.implicitConversions
import scala.math.{Numeric, Ordering}
import scala.reflect.ClassTag

/**
  * A template trait for collections which can be traversed either once only
  * or one or more times.
  *
  * Note: `IterableOnce` does not extend [[IterableOnceOps]]. This is different than the general
  * design of the collections library, which uses the following pattern:
  * {{{
  *   trait Seq extends Iterable with SeqOps
  *   trait SeqOps extends IterableOps
  *
  *   trait IndexedSeq extends Seq with IndexedSeqOps
  *   trait IndexedSeqOps extends SeqOps
  * }}}
  *
  * The goal is to provide a minimal interface without any sequential operations. This allows
  * third-party extension like Scala parallel collections to integrate at the level of IterableOnce
  * without inheriting unwanted implementations.
  *
  * @define coll collection
  */
trait IterableOnce[+A] extends Any {
  /** Iterator can be used only once */
  def iterator: Iterator[A]

  /** Returns a [[scala.collection.Stepper]] for the elements of this collection.
    *
    * The Stepper enables creating a Java stream to operate on the collection, see
    * [[scala.jdk.StreamConverters]]. For collections holding primitive values, the Stepper can be
    * used as an iterator which doesn't box the elements.
    *
    * The implicit [[scala.collection.StepperShape]] parameter defines the resulting Stepper type according to the
    * element type of this collection.
    *
    *   - For collections of `Int`, `Short`, `Byte` or `Char`, an [[scala.collection.IntStepper]] is returned
    *   - For collections of `Double` or `Float`, a [[scala.collection.DoubleStepper]] is returned
    *   - For collections of `Long` a [[scala.collection.LongStepper]] is returned
    *   - For any other element type, an [[scala.collection.AnyStepper]] is returned
    *
    * Note that this method is overridden in subclasses and the return type is refined to
    * `S with EfficientSplit`, for example [[scala.collection.IndexedSeqOps.stepper]]. For Steppers marked with
    * [[scala.collection.Stepper.EfficientSplit]], the converters in [[scala.jdk.StreamConverters]]
    * allow creating parallel streams, whereas bare Steppers can be converted only to sequential
    * streams.
    */
  def stepper[S <: Stepper[_]](implicit shape: StepperShape[A, S]): S = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntIteratorStepper   (iterator.asInstanceOf[Iterator[Int]])
      case StepperShape.LongShape   => new LongIteratorStepper  (iterator.asInstanceOf[Iterator[Long]])
      case StepperShape.DoubleShape => new DoubleIteratorStepper(iterator.asInstanceOf[Iterator[Double]])
      case _                        => shape.seqUnbox(new AnyIteratorStepper[A](iterator))
    }
    s.asInstanceOf[S]
  }

  /** @return The number of elements in this $coll, if it can be cheaply computed,
    *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
    */
  def knownSize: Int = -1
}

final class IterableOnceExtensionMethods[A](private val it: IterableOnce[A]) extends AnyVal {
  @deprecated("Use .iterator.withFilter(...) instead", "2.13.0")
  def withFilter(f: A => Boolean): Iterator[A] = it.iterator.withFilter(f)

  @deprecated("Use .iterator.reduceLeftOption(...) instead", "2.13.0")
  def reduceLeftOption(f: (A, A) => A): Option[A] = it.iterator.reduceLeftOption(f)

  @deprecated("Use .iterator.min instead", "2.13.0")
  def min(implicit ord: Ordering[A]): A = it.iterator.min

  @deprecated("Use .iterator.nonEmpty instead", "2.13.0")
  def nonEmpty: Boolean = it.iterator.nonEmpty

  @deprecated("Use .iterator.max instead", "2.13.0")
  def max(implicit ord: Ordering[A]): A = it.iterator.max

  @deprecated("Use .iterator.reduceRight(...) instead", "2.13.0")
  def reduceRight(f: (A, A) => A): A = it.iterator.reduceRight(f)

  @deprecated("Use .iterator.maxBy(...) instead", "2.13.0")
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = it.iterator.maxBy(f)

  @deprecated("Use .iterator.reduceLeft(...) instead", "2.13.0")
  def reduceLeft(f: (A, A) => A): A = it.iterator.reduceLeft(f)

  @deprecated("Use .iterator.sum instead", "2.13.0")
  def sum(implicit num: Numeric[A]): A = it.iterator.sum

  @deprecated("Use .iterator.product instead", "2.13.0")
  def product(implicit num: Numeric[A]): A = it.iterator.product

  @deprecated("Use .iterator.count(...) instead", "2.13.0")
  def count(f: A => Boolean): Int = it.iterator.count(f)

  @deprecated("Use .iterator.reduceOption(...) instead", "2.13.0")
  def reduceOption(f: (A, A) => A): Option[A] = it.iterator.reduceOption(f)

  @deprecated("Use .iterator.minBy(...) instead", "2.13.0")
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = it.iterator.minBy(f)

  @deprecated("Use .iterator.size instead", "2.13.0")
  def size: Int = it.iterator.size

  @deprecated("Use .iterator.forall(...) instead", "2.13.0")
  def forall(f: A => Boolean): Boolean = it.iterator.forall(f)

  @deprecated("Use .iterator.collectFirst(...) instead", "2.13.0")
  def collectFirst[B](f: PartialFunction[A, B]): Option[B] = it.iterator.collectFirst(f)

  @deprecated("Use .iterator.filter(...) instead", "2.13.0")
  def filter(f: A => Boolean): Iterator[A] = it.iterator.filter(f)

  @deprecated("Use .iterator.exists(...) instead", "2.13.0")
  def exists(f: A => Boolean): Boolean = it.iterator.exists(f)

  @deprecated("Use .iterator.copyToBuffer(...) instead", "2.13.0")
  def copyToBuffer(dest: mutable.Buffer[A]): Unit = it.iterator.copyToBuffer(dest)

  @deprecated("Use .iterator.reduce(...) instead", "2.13.0")
  def reduce(f: (A, A) => A): A = it.iterator.reduce(f)

  @deprecated("Use .iterator.reduceRightOption(...) instead", "2.13.0")
  def reduceRightOption(f: (A, A) => A): Option[A] = it.iterator.reduceRightOption(f)

  @deprecated("Use .iterator.toIndexedSeq instead", "2.13.0")
  def toIndexedSeq: IndexedSeq[A] = it.iterator.toIndexedSeq

  @deprecated("Use .iterator.foreach(...) instead", "2.13.0")
  @`inline` def foreach[U](f: A => U): Unit = it match {
    case it: Iterable[A] => it.foreach(f)
    case _ => it.iterator.foreach(f)
  }

  @deprecated("Use .iterator.to(factory) instead", "2.13.0")
  def to[C1](factory: Factory[A, C1]): C1 = factory.fromSpecific(it)

  @deprecated("Use .iterator.to(ArrayBuffer) instead", "2.13.0")
  def toBuffer[B >: A]: mutable.Buffer[B] = mutable.ArrayBuffer.from(it)

  @deprecated("Use .iterator.toArray", "2.13.0")
  def toArray[B >: A: ClassTag]: Array[B] = it match {
    case it: Iterable[B] => it.toArray[B]
    case _ => it.iterator.toArray[B]
  }

  @deprecated("Use .iterator.to(List) instead", "2.13.0")
  def toList: immutable.List[A] = immutable.List.from(it)

  @deprecated("Use .iterator.to(Set) instead", "2.13.0")
  @`inline` def toSet[B >: A]: immutable.Set[B] = immutable.Set.from(it)

  @deprecated("Use .iterator.to(Iterable) instead", "2.13.0")
  @`inline` final def toTraversable: Traversable[A] = toIterable

  @deprecated("Use .iterator.to(Iterable) instead", "2.13.0")
  @`inline` final def toIterable: Iterable[A] = Iterable.from(it)

  @deprecated("Use .iterator.to(Seq) instead", "2.13.0")
  @`inline` def toSeq: immutable.Seq[A] = immutable.Seq.from(it)

  @deprecated("Use .iterator.to(LazyList) instead", "2.13.0")
  @`inline` def toStream: immutable.Stream[A] = immutable.Stream.from(it)

  @deprecated("Use .iterator.to(Vector) instead", "2.13.0")
  @`inline` def toVector: immutable.Vector[A] = immutable.Vector.from(it)

  @deprecated("Use .iterator.to(Map) instead", "2.13.0")
  def toMap[K, V](implicit ev: A <:< (K, V)): immutable.Map[K, V] =
    immutable.Map.from(it.asInstanceOf[IterableOnce[(K, V)]])

  @deprecated("Use .iterator instead", "2.13.0")
  @`inline` def toIterator: Iterator[A] = it.iterator

  @deprecated("Use .iterator.isEmpty instead", "2.13.0")
  def isEmpty: Boolean = it match {
    case it: Iterable[A] => it.isEmpty
    case _ => it.iterator.isEmpty
  }

  @deprecated("Use .iterator.mkString instead", "2.13.0")
  def mkString(start: String, sep: String, end: String): String = it match {
    case it: Iterable[A] => it.mkString(start, sep, end)
    case _ => it.iterator.mkString(start, sep, end)
  }

  @deprecated("Use .iterator.mkString instead", "2.13.0")
  def mkString(sep: String): String = it match {
    case it: Iterable[A] => it.mkString(sep)
    case _ => it.iterator.mkString(sep)
  }

  @deprecated("Use .iterator.mkString instead", "2.13.0")
  def mkString: String = it match {
    case it: Iterable[A] => it.mkString
    case _ => it.iterator.mkString
  }

  @deprecated("Use .iterator.find instead", "2.13.0")
  def find(p: A => Boolean): Option[A] = it.iterator.find(p)

  @deprecated("Use .iterator.foldLeft instead", "2.13.0")
  @`inline` def foldLeft[B](z: B)(op: (B, A) => B): B = it.iterator.foldLeft(z)(op)

  @deprecated("Use .iterator.foldRight instead", "2.13.0")
  @`inline` def foldRight[B](z: B)(op: (A, B) => B): B = it.iterator.foldRight(z)(op)

  @deprecated("Use .iterator.fold instead", "2.13.0")
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = it.iterator.fold(z)(op)

  @deprecated("Use .iterator.foldLeft instead", "2.13.0")
  @`inline` def /: [B](z: B)(op: (B, A) => B): B = foldLeft[B](z)(op)

  @deprecated("Use .iterator.foldRight instead", "2.13.0")
  @`inline` def :\ [B](z: B)(op: (A, B) => B): B = foldRight[B](z)(op)

  @deprecated("Use .iterator.map instead or consider requiring an Iterable", "2.13.0")
  def map[B](f: A => B): IterableOnce[B] = it match {
    case it: Iterable[A] => it.map(f)
    case _ => it.iterator.map(f)
  }

  @deprecated("Use .iterator.flatMap instead or consider requiring an Iterable", "2.13.0")
  def flatMap[B](f: A => IterableOnce[B]): IterableOnce[B] = it match {
    case it: Iterable[A] => it.flatMap(f)
    case _ => it.iterator.flatMap(f)
  }

  @deprecated("Use .iterator.sameElements instead", "2.13.0")
  def sameElements[B >: A](that: IterableOnce[B]): Boolean = it.iterator.sameElements(that)
}

object IterableOnce {
  @`inline` implicit def iterableOnceExtensionMethods[A](it: IterableOnce[A]): IterableOnceExtensionMethods[A] =
    new IterableOnceExtensionMethods[A](it)

  /** Computes the number of elements to copy to an array from a source IterableOnce
    *
    * @param srcLen the length of the source collection
    * @param destLen the length of the destination array
    * @param start the index in the destination array at which to start copying elements to
    * @param len the requested number of elements to copy (we may only be able to copy less than this)
    * @return the number of elements that will be copied to the destination array
    */
  @inline private[collection] def elemsToCopyToArray(srcLen: Int, destLen: Int, start: Int, len: Int): Int =
    math.max(math.min(math.min(len, srcLen), destLen - start), 0)

  /** Calls `copyToArray` on the given collection, regardless of whether or not it is an `Iterable`. */
  @inline private[collection] def copyElemsToArray[A, B >: A](elems: IterableOnce[A],
                                                              xs: Array[B],
                                                              start: Int = 0,
                                                              len: Int = Int.MaxValue): Int =
    elems match {
      case src: Iterable[A] => src.copyToArray[B](xs, start, len)
      case src              => src.iterator.copyToArray[B](xs, start, len)
    }

  @inline private[collection] def checkArraySizeWithinVMLimit(size: Int): Unit = {
    import scala.runtime.PStatics.VM_MaxArraySize
    if (size > VM_MaxArraySize) {
      throw new Exception(s"Size of array-backed collection exceeds VM array size limit of ${VM_MaxArraySize}")
    }
  }
}

/** This implementation trait can be mixed into an `IterableOnce` to get the basic methods that are shared between
  * `Iterator` and `Iterable`. The `IterableOnce` must support multiple calls to `iterator` but may or may not
  * return the same `Iterator` every time.
  *
  * @define orderDependent
  *
  *              Note: might return different results for different runs, unless the underlying collection type is ordered.
  * @define orderDependentFold
  *
  *              Note: might return different results for different runs, unless the
  *              underlying collection type is ordered or the operator is associative
  *              and commutative.
  * @define mayNotTerminateInf
  *
  *              Note: may not terminate for infinite-sized collections.
  * @define willNotTerminateInf
  *
  *              Note: will not terminate for infinite-sized collections.
  * @define willForceEvaluation
  *              Note: Even when applied to a view or a lazy collection it will always force the elements.
  * @define consumesIterator
  *              After calling this method, one should discard the iterator it was called
  * on. Using it is undefined and subject to change.
  * @define undefinedorder
  *              The order in which operations are performed on elements is unspecified
  *              and may be nondeterministic.
  * @define coll collection
  *
  */
trait IterableOnceOps[+A, +CC[_], +C] extends Any { this: IterableOnce[A] =>
  /////////////////////////////////////////////////////////////// Abstract methods that must be implemented

  /** Produces a $coll containing cumulative results of applying the
    * operator going left to right, including the initial value.
    *
    *  $willNotTerminateInf
    *  $orderDependent
    *
    *  @tparam B      the type of the elements in the resulting collection
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    */
  def scanLeft[B](z: B)(op: (B, A) => B): CC[B]

  /** Selects all elements of this $coll which satisfy a predicate.
    *
    *  @param p     the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
    *               predicate `p`. The order of the elements is preserved.
    */
  def filter(p: A => Boolean): C

  /** Selects all elements of this $coll which do not satisfy a predicate.
    *
    *  @param pred  the predicate used to test elements.
    *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
    *               predicate `pred`. Their order may not be preserved.
    */
  def filterNot(pred: A => Boolean): C

  /** Selects the first ''n'' elements.
    *  $orderDependent
    *  @param  n    the number of elements to take from this $coll.
    *  @return a $coll consisting only of the first `n` elements of this $coll,
    *          or else the whole $coll, if it has less than `n` elements.
    *          If `n` is negative, returns an empty $coll.
    */
  def take(n: Int): C

  /** Takes longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest prefix of this $coll whose elements all satisfy
    *           the predicate `p`.
    */
  def takeWhile(p: A => Boolean): C

  /** Selects all elements except first ''n'' ones.
    *  $orderDependent
    *  @param  n    the number of elements to drop from this $coll.
    *  @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
    *          empty $coll, if this $coll has less than `n` elements.
    *          If `n` is negative, don't drop any elements.
    */
  def drop(n: Int): C

  /** Drops longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest suffix of this $coll whose first element
    *           does not satisfy the predicate `p`.
    */
  def dropWhile(p: A => Boolean): C

  /** Selects an interval of elements.  The returned $coll is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  $orderDependent
    *
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    *  @return  a $coll containing the elements greater than or equal to
    *           index `from` extending up to (but not including) index `until`
    *           of this $coll.
    */
  def slice(from: Int, until: Int): C

  /** Builds a new $coll by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    */
  def map[B](f: A => B): CC[B]

  /** Builds a new $coll by applying a function to all elements of this $coll
    *  and using the elements of the resulting collections.
    *
    *    For example:
    *
    *    {{{
    *      def getWords(lines: Seq[String]): Seq[String] = lines flatMap (line => line split "\\W+")
    *    }}}
    *
    *    The type of the resulting collection is guided by the static type of $coll. This might
    *    cause unexpected results sometimes. For example:
    *
    *    {{{
    *      // lettersOf will return a Seq[Char] of likely repeated letters, instead of a Set
    *      def lettersOf(words: Seq[String]) = words flatMap (word => word.toSet)
    *
    *      // lettersOf will return a Set[Char], not a Seq
    *      def lettersOf(words: Seq[String]) = words.toSet flatMap ((word: String) => word.toSeq)
    *
    *      // xs will be an Iterable[Int]
    *      val xs = Map("a" -> List(11,111), "b" -> List(22,222)).flatMap(_._2)
    *
    *      // ys will be a Map[Int, Int]
    *      val ys = Map("a" -> List(1 -> 11,1 -> 111), "b" -> List(2 -> 22,2 -> 222)).flatMap(_._2)
    *    }}}
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll resulting from applying the given collection-valued function
    *                `f` to each element of this $coll and concatenating the results.
    */
  def flatMap[B](f: A => IterableOnce[B]): CC[B]

  /** Converts this $coll of traversable collections into
    *  a $coll formed by the elements of these traversable
    *  collections.
    *
    *    The resulting collection's type will be guided by the
    *    type of $coll. For example:
    *
    *    {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(1, 2, 3)
    *             ).flatten
    *    // xs == List(1, 2, 3, 1, 2, 3)
    *
    *    val ys = Set(
    *               List(1, 2, 3),
    *               List(3, 2, 1)
    *             ).flatten
    *    // ys == Set(1, 2, 3)
    *    }}}
    *
    *  @tparam B the type of the elements of each traversable collection.
    *  @param asIterable an implicit conversion which asserts that the element
    *          type of this $coll is a `GenTraversable`.
    *  @return a new $coll resulting from concatenating all element ${coll}s.
    */
  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B]

  /** Builds a new $coll by applying a partial function to all elements of this $coll
    *  on which the function is defined.
    *
    *  @param pf     the partial function which filters and maps the $coll.
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new $coll resulting from applying the given partial function
    *                `pf` to each element on which it is defined and collecting the results.
    *                The order of the elements is preserved.
    */
  def collect[B](pf: PartialFunction[A, B]): CC[B]

  /** Zips this $coll with its indices.
    *
    *  @return        A new $coll containing pairs consisting of all elements of this $coll paired with their index.
    *                 Indices start at `0`.
    *  @example
    *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
    */
  def zipWithIndex: CC[(A @uncheckedVariance, Int)]

  /** Splits this $coll into a prefix/suffix pair according to a predicate.
    *
    *  Note: `c span p`  is equivalent to (but possibly more efficient than)
    *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
    *  predicate `p` does not cause any side-effects.
    *  $orderDependent
    *
    *  @param p the test predicate
    *  @return  a pair consisting of the longest prefix of this $coll whose
    *           elements all satisfy `p`, and the rest of this $coll.
    */
  def span(p: A => Boolean): (C, C)

  /** Splits this $coll into a prefix/suffix pair at a given position.
   *
   *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
   *         `(c take n, c drop n)`.
   *  $orderDependent
   *
   *  @param n the position at which to split.
   *  @return  a pair of ${coll}s consisting of the first `n`
   *           elements of this $coll, and the other elements.
   */
  def splitAt(n: Int): (C, C) = {
    class Spanner extends runtime.AbstractFunction1[A, Boolean] {
      var i = 0
      def apply(a: A) = i < n && { i += 1 ; true }
    }
    val spanner = new Spanner
    span(spanner)
  }

  /** Applies a side-effecting function to each element in this collection.
    * Strict collections will apply `f` to their elements immediately, while lazy collections
    * like Views and LazyLists will only apply `f` on each element if and when that element
    * is evaluated, and each time that element is evaluated.
    *
    * @param f a function to apply to each element in this $coll
    * @tparam U the return type of f
    * @return The same logical collection as this
    */
  def tapEach[U](f: A => U): C

  /////////////////////////////////////////////////////////////// Concrete methods based on iterator

  /** Tests whether this $coll is known to have a finite size.
    *  All strict collections are known to have finite size. For a non-strict
    *  collection such as `Stream`, the predicate returns `'''true'''` if all
    *  elements have been computed. It returns `'''false'''` if the stream is
    *  not yet evaluated to the end. Non-empty Iterators usually return
    *  `'''false'''` even if they were created from a collection with a known
    *  finite size.
    *
    *  Note: many collection methods will not work on collections of infinite sizes.
    *  The typical failure mode is an infinite loop. These methods always attempt a
    *  traversal without checking first that `hasDefiniteSize` returns `'''true'''`.
    *  However, checking `hasDefiniteSize` can provide an assurance that size is
    *  well-defined and non-termination is not a concern.
    *
    *  @deprecated This method is deprecated in 2.13 because it does not provide any
    *    actionable information. As noted above, even the collection library itself
    *    does not use it. When there is no guarantee that a collection is finite, it
    *    is generally best to attempt a computation anyway and document that it will
    *    not terminate for infinite collections rather than backing out because this
    *    would prevent performing the computation on collections that are in fact
    *    finite even though `hasDefiniteSize` returns `false`.
    *
    *  @see method `knownSize` for a more useful alternative
    *
    *  @return  `'''true'''` if this collection is known to have finite size,
    *           `'''false'''` otherwise.
    */
  @deprecated("Check .knownSize instead of .hasDefiniteSize for more actionable information (see scaladoc for details)", "2.13.0")
  def hasDefiniteSize: Boolean = true

  /** Tests whether this $coll can be repeatedly traversed.  Always
   *  true for Iterables and false for Iterators unless overridden.
   *
   *  @return   `true` if it is repeatedly traversable, `false` otherwise.
   */
  def isTraversableAgain: Boolean = false

  /** Apply `f` to each element for its side effects
    *  Note: [U] parameter needed to help scalac's type inference.
    */
  def foreach[U](f: A => U): Unit = {
    val it = iterator
    while(it.hasNext) f(it.next())
  }

  /** Tests whether a predicate holds for all elements of this $coll.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if this $coll is empty or the given predicate `p`
    *                 holds for all elements of this $coll, otherwise `false`.
    */
  def forall(p: A => Boolean): Boolean = {
    var res = true
    val it = iterator
    while (res && it.hasNext) res = p(it.next())
    res
  }

  /** Tests whether a predicate holds for at least one element of this $coll.
    *
    *  $mayNotTerminateInf
    *
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` is satisfied by at least one element of this $coll, otherwise `false`
    */
  def exists(p: A => Boolean): Boolean = {
    var res = false
    val it = iterator
    while (!res && it.hasNext) res = p(it.next())
    res
  }

  /** Counts the number of elements in the $coll which satisfy a predicate.
    *
    *  $willNotTerminateInf
    * 
    *  @param p     the predicate  used to test elements.
    *  @return      the number of elements satisfying the predicate `p`.
    */
  def count(p: A => Boolean): Int = {
    var res = 0
    val it = iterator
    while (it.hasNext) if (p(it.next())) res += 1
    res
  }

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  $mayNotTerminateInf
    *  $orderDependent
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    */
  def find(p: A => Boolean): Option[A] = {
    val it = iterator
    while (it.hasNext) {
      val a = it.next()
      if (p(a)) return Some(a)
    }
    None
  }

  /** Applies a binary operator to a start value and all elements of this $coll,
    *  going left to right.
    *
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going left to right with the start value `z` on the left:
    *           `op(...op(z, x,,1,,), x,,2,,, ..., x,,n,,)` where `x,,1,,, ..., x,,n,,`
   *            are the elements of this $coll.
    *           Returns `z` if this $coll is empty.
    */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    val it = iterator
    while (it.hasNext) {
      result = op(result, it.next())
    }
    result
  }

  /** Applies a binary operator to all elements of this $coll and a start value,
    *  going right to left.
    *
    *  $willNotTerminateInf
    *  $orderDependentFold
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left with the start value `z` on the right:
    *           `op(x,,1,,, op(x,,2,,, ... op(x,,n,,, z)...))` where `x,,1,,, ..., x,,n,,`
    *           are the elements of this $coll.
    *           Returns `z` if this $coll is empty.
    */
  def foldRight[B](z: B)(op: (A, B) => B): B = reversed.foldLeft(z)((b, a) => op(a, b))

  @deprecated("Use foldLeft instead of /:", "2.13.0")
  @`inline` final def /: [B](z: B)(op: (B, A) => B): B = foldLeft[B](z)(op)

  @deprecated("Use foldRight instead of :\\", "2.13.0")
  @`inline` final def :\ [B](z: B)(op: (A, B) => B): B = foldRight[B](z)(op)

  /** Folds the elements of this $coll using the specified associative binary operator.
    * The default implementation in `IterableOnce` is equivalent to `foldLeft` but may be
    * overridden for more efficient traversal orders.
    *
    *  $undefinedorder
    *  $willNotTerminateInf
    *
    *  @tparam A1     a type parameter for the binary operator, a supertype of `A`.
    *  @param z       a neutral element for the fold operation; may be added to the result
    *                 an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
    *                 0 for addition, or 1 for multiplication).
    *  @param op      a binary operator that must be associative.
    *  @return        the result of applying the fold operator `op` between all the elements and `z`, or `z` if this $coll is empty.
    */
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  /** Reduces the elements of this $coll using the specified associative binary operator.
    *
    *  $undefinedorder
    *
    *  @tparam B      A type parameter for the binary operator, a supertype of `A`.
    *  @param op       A binary operator that must be associative.
    *  @return         The result of applying reduce operator `op` between all the elements if the $coll is nonempty.
    *  @throws UnsupportedOperationException if this $coll is empty.
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
    *           `op( op( ... op(x,,1,,, x,,2,,) ..., x,,n-1,,), x,,n,,)` where `x,,1,,, ..., x,,n,,`
    *           are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val it = iterator
    if (it.isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = null.asInstanceOf[B]

    while (it.hasNext) {
      val x = it.next()
      if (first) {
        acc = x
        first = false
      }
      else acc = op(acc, x)
    }
    acc
  }

  /** Applies a binary operator to all elements of this $coll, going right to left.
    *  $willNotTerminateInf
    *  $orderDependentFold
    *
    *  @param  op    the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this $coll,
    *           going right to left:
    *           `op(x,,1,,, op(x,,2,,, ..., op(x,,n-1,,, x,,n,,)...))` where `x,,1,,, ..., x,,n,,`
    *           are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.
    */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    val it = iterator
    if (it.isEmpty)
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

  /** Tests whether the $coll is empty.
    *
    *  Note: Implementations in subclasses that are not repeatedly traversable must take
    *  care not to consume any elements when `isEmpty` is called.
    *
    *  @return    `true` if the $coll contains no elements, `false` otherwise.
    */
  def isEmpty: Boolean = !iterator.hasNext

  /** Tests whether the $coll is not empty.
    *
    *  @return    `true` if the $coll contains at least one element, `false` otherwise.
    */
  @deprecatedOverriding("nonEmpty is defined as !isEmpty; override isEmpty instead", "2.13.0")
  def nonEmpty: Boolean = !isEmpty

  /** The size of this $coll.
    *
    *  $willNotTerminateInf
    *
    *  @return    the number of elements in this $coll.
    */
  def size: Int = {
    if (knownSize >= 0) knownSize
    else {
      val it = iterator
      var len = 0
      while (it.hasNext) { len += 1; it.next() }
      len
    }
  }

  @deprecated("Use `dest ++= coll` instead", "2.13.0")
  @inline final def copyToBuffer[B >: A](dest: mutable.Buffer[B]): Unit = dest ++= this

  /** Copy elements to an array, returning the number of elements written.
   *
   *  Fills the given array `xs` starting at index `start` with values of this $coll.
   *
   *  Copying will stop once either all the elements of this $coll have been copied,
   *  or the end of the array is reached.
   *
   *  @param  xs     the array to fill.
   *  @tparam B      the type of the elements of the array.
   *  @return        the number of elements written to the array
   *
   *  @note    Reuse: $consumesIterator
   */
  @deprecatedOverriding("This should always forward to the 3-arg version of this method", since = "2.13.4")
  def copyToArray[B >: A](xs: Array[B]): Int = copyToArray(xs, 0, Int.MaxValue)

  /** Copy elements to an array, returning the number of elements written.
    *
    *  Fills the given array `xs` starting at index `start` with values of this $coll.
    *
    *  Copying will stop once either all the elements of this $coll have been copied,
    *  or the end of the array is reached.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index of xs.
    *  @tparam B      the type of the elements of the array.
    *  @return        the number of elements written to the array
    *
    *  @note    Reuse: $consumesIterator
    */
  @deprecatedOverriding("This should always forward to the 3-arg version of this method", since = "2.13.4")
  def copyToArray[B >: A](xs: Array[B], start: Int): Int = copyToArray(xs, start, Int.MaxValue)

  /** Copy elements to an array, returning the number of elements written.
    *
    *  Fills the given array `xs` starting at index `start` with at most `len` elements of this $coll.
    *
    *  Copying will stop once either all the elements of this $coll have been copied,
    *  or the end of the array is reached, or `len` elements have been copied.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index of xs.
    *  @param  len    the maximal number of elements to copy.
    *  @tparam B      the type of the elements of the array.
    *  @return        the number of elements written to the array
    *
    *  @note    Reuse: $consumesIterator
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Int = {
    val it = iterator
    var i = start
    val end = start + math.min(len, xs.length - start)
    while (i < end && it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    i - start
  }

  /** Sums up the elements of this collection.
    *
    *   $willNotTerminateInf
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `+` operator to be used in forming the sum.
    *   @tparam  B    the result type of the `+` operator.
    *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
    */
  def sum[B >: A](implicit num: Numeric[B]): B = if (isEmpty) num.zero else reduce(num.plus)

  /** Multiplies up the elements of this collection.
    *
    *  $willNotTerminateInf
    *
    *   @param   num  an implicit parameter defining a set of numeric operations
    *                 which includes the `*` operator to be used in forming the product.
    *   @tparam  B   the result type of the `*` operator.
    *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
    */
  def product[B >: A](implicit num: Numeric[B]): B = if (isEmpty) num.one else reduce(num.times)

  /** Finds the smallest element.
    *
    *  $willNotTerminateInf
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @throws   UnsupportedOperationException if this $coll is empty.
    *  @return   the smallest element of this $coll with respect to the ordering `ord`.
    *
    */
  def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    reduceLeft(ord.min)
  }

  /** Finds the smallest element.
    *
    *  $willNotTerminateInf
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   an option value containing the smallest element of this $coll
    *            with respect to the ordering `ord`.
    */
  def minOption[B >: A](implicit ord: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(min(ord))
  }

  /** Finds the largest element.
    *
    *  $willNotTerminateInf
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @throws   UnsupportedOperationException if this $coll is empty.
    *  @return   the largest element of this $coll with respect to the ordering `ord`.
    */
  def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    reduceLeft(ord.max)
  }

  /** Finds the largest element.
    *
    *  $willNotTerminateInf
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   an option value containing the largest element of this $coll with
    *            respect to the ordering `ord`.
    */
  def maxOption[B >: A](implicit ord: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(max(ord))
  }

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  $willNotTerminateInf
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @throws   UnsupportedOperationException if this $coll is empty.
    *  @return   the first element of this $coll with the largest value measured by function f
    *            with respect to the ordering `cmp`.
    */
  def maxBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.maxBy")

    class Maximizer extends runtime.AbstractFunction1[A, Unit] {
      var maxF: B = null.asInstanceOf[B]
      var maxElem: A = null.asInstanceOf[A]
      var first = true
      def apply(elem: A) = {
        val fx = f(elem)
        if (first && { first = false ; true } || cmp.gt(fx, maxF)) {
          maxElem = elem
          maxF = fx
        }
      }
    }
    val maximizer = new Maximizer
    foreach(maximizer)
    maximizer.maxElem
  }

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  $willNotTerminateInf
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   an option value containing the first element of this $coll with the
    *            largest value measured by function f with respect to the ordering `cmp`.
    */
  def maxByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(maxBy(f)(cmp))
  }

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  $willNotTerminateInf
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @throws   UnsupportedOperationException if this $coll is empty.
    *  @return   the first element of this $coll with the smallest value measured by function f
    *            with respect to the ordering `cmp`.
    */
  def minBy[B](f: A => B)(implicit cmp: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.minBy")

    class Minimizer extends runtime.AbstractFunction1[A, Unit] {
      var minF: B = null.asInstanceOf[B]
      var minElem: A = null.asInstanceOf[A]
      var first = true
      def apply(elem: A) = {
        val fx = f(elem)
        if (first && { first = false ; true } || cmp.lt(fx, minF)) {
          minElem = elem
          minF = fx
        }
      }
    }
    val minimizer = new Minimizer
    foreach(minimizer)
    minimizer.minElem
  }

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  $willNotTerminateInf
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   an option value containing the first element of this $coll
    *            with the smallest value measured by function f
    *            with respect to the ordering `cmp`.
    */
  def minByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(minBy(f)(cmp))
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
    // Presumably the fastest way to get in and out of a partial function is for a sentinel function to return itself
    // (Tested to be lower-overhead than runWith.  Would be better yet to not need to (formally) allocate it)
    val sentinel: scala.Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any] {
      def apply(a: A) = this
    }
    val it = iterator
    while (it.hasNext) {
      val x = pf.applyOrElse(it.next(), sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) return Some(x.asInstanceOf[B])
    }
    None
  }

  @deprecated("`aggregate` is not relevant for sequential collections. Use `foldLeft(z)(seqop)` instead.", "2.13.0")
  def aggregate[B](z: => B)(seqop: (B, A) => B, combop: (B, B) => B): B = foldLeft(z)(seqop)

  /** Tests whether every element of this collection's iterator relates to the
   *  corresponding element of another collection by satisfying a test predicate.
   *
   *  $willNotTerminateInf
   *
   *  @param   that    the other collection
   *  @param   p       the test predicate, which relates elements from both collections
   *  @tparam  B       the type of the elements of `that`
   *  @return          `true` if both collections have the same length and
   *                   `p(x, y)` is `true` for all corresponding elements `x` of this iterator
   *                   and `y` of `that`, otherwise `false`
   */
  def corresponds[B](that: IterableOnce[B])(p: (A, B) => Boolean): Boolean = {
    val a = iterator
    val b = that.iterator

    while (a.hasNext && b.hasNext) {
      if (!p(a.next(), b.next())) return false
    }

    a.hasNext == b.hasNext
  }

  /** Displays all elements of this $coll in a string using start, end, and
    *  separator strings.
    *
    * Delegates to addString, which can be overridden.
    *
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      a string representation of this $coll. The resulting string
    *               begins with the string `start` and ends with the string
    *               `end`. Inside, the string representations (w.r.t. the method
    *               `toString`) of all elements of this $coll are separated by
    *               the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
    */
  final def mkString(start: String, sep: String, end: String): String =
    if (isEmpty) start + end
    else addString(new StringBuilder(), start, sep, end).result()

  /** Displays all elements of this $coll in a string using a separator string.
    *
    * Delegates to addString, which can be overridden.
    *
    *  @param sep   the separator string.
    *  @return      a string representation of this $coll. In the resulting string
    *               the string representations (w.r.t. the method `toString`)
    *               of all elements of this $coll are separated by the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
    */
  @inline final def mkString(sep: String): String = mkString("", sep, "")

  /** Displays all elements of this $coll in a string.
    *
    * Delegates to addString, which can be overridden.
    *
    *  @return a string representation of this $coll. In the resulting string
    *          the string representations (w.r.t. the method `toString`)
    *          of all elements of this $coll follow each other without any
    *          separator string.
    */
  @inline final def mkString: String = mkString("")

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
    val jsb = b.underlying
    if (start.length != 0) jsb.append(start)
    val it = iterator
    if (it.hasNext) {
      jsb.append(it.next())
      while (it.hasNext) {
        jsb.append(sep)
        jsb.append(it.next())
      }
    }
    if (end.length != 0) jsb.append(end)
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
  @inline final def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

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
  @inline final def addString(b: StringBuilder): StringBuilder = addString(b, "")

  /** Given a collection factory `factory`, convert this collection to the appropriate
    * representation for the current element type `A`. Example uses:
    *
    *      xs.to(List)
    *      xs.to(ArrayBuffer)
    *      xs.to(BitSet) // for xs: Iterable[Int]
    */
  def to[C1](factory: Factory[A, C1]): C1 = factory.fromSpecific(this)

  @deprecated("Use .iterator instead of .toIterator", "2.13.0")
  @`inline` final def toIterator: Iterator[A] = iterator

  def toList: immutable.List[A] = immutable.List.from(this)

  def toVector: immutable.Vector[A] = immutable.Vector.from(this)

  def toMap[K, V](implicit ev: A <:< (K, V)): immutable.Map[K, V] =
    immutable.Map.from(this.asInstanceOf[IterableOnce[(K, V)]])

  def toSet[B >: A]: immutable.Set[B] = immutable.Set.from(this)

  /**
    * @return This collection as a `Seq[A]`. This is equivalent to `to(Seq)` but might be faster.
    */
  def toSeq: immutable.Seq[A] = immutable.Seq.from(this)

  def toIndexedSeq: immutable.IndexedSeq[A] = immutable.IndexedSeq.from(this)

  @deprecated("Use .to(LazyList) instead of .toStream", "2.13.0")
  @`inline` final def toStream: immutable.Stream[A] = to(immutable.Stream)

  @`inline` final def toBuffer[B >: A]: mutable.Buffer[B] = mutable.Buffer.from(this)

  /** Convert collection to array.
    *
    * Implementation note: DO NOT call [[Array.from]] from this method.
    */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) {
      val destination = new Array[B](knownSize)
      copyToArray(destination, 0)
      destination
    }
    else mutable.ArrayBuilder.make[B].addAll(this).result()

  // For internal use
  protected def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = iterator
    while (it.hasNext) xs = it.next() :: xs
    xs
  }
}
