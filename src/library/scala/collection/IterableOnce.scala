package scala
package collection

import scala.language.{higherKinds, implicitConversions}
import scala.annotation.unchecked.uncheckedVariance
import scala.math.{Ordering, Numeric}
import scala.reflect.ClassTag
import scala.collection.mutable.StringBuilder

/**
  * A template trait for collections which can be traversed either once only
  * or one or more times.
  *
  * @define orderDependent
  *
  *    Note: might return different results for different runs, unless the underlying collection type is ordered.
  * @define orderDependentFold
  *
  *    Note: might return different results for different runs, unless the
  *    underlying collection type is ordered or the operator is associative
  *    and commutative.
  * @define mayNotTerminateInf
  *
  *    Note: may not terminate for infinite-sized collections.
  * @define willNotTerminateInf
  *
  *    Note: will not terminate for infinite-sized collections.
  *
  * @define coll collection
  */
trait IterableOnce[+A] extends Any {
  /** Iterator can be used only once */
  def iterator: Iterator[A]

  /** @return The number of elements of this $coll if it can be computed in O(1) time, otherwise -1 */
  def knownSize: Int
}

final class IterableOnceExtensionMethods[A](private val it: IterableOnce[A]) extends AnyVal {
  @deprecated("Use .iterator.foreach(...) instead of .foreach(...) on IterableOnce", "2.13.0")
  @`inline` def foreach[U](f: A => U): Unit = it match {
    case it: Iterable[A] => it.foreach(f)
    case _ => it.iterator.foreach(f)
  }

  @deprecated("Use factory.from(it) instead of it.to(factory) for IterableOnce", "2.13.0")
  def to[C1](factory: Factory[A, C1]): C1 = factory.fromSpecific(it)

  @deprecated("Use ArrayBuffer.from(it) instead of it.toBuffer", "2.13.0")
  def toBuffer[B >: A]: mutable.Buffer[B] = mutable.ArrayBuffer.from(it)

  @deprecated("Use ArrayBuffer.from(it).toArray", "2.13.0")
  def toArray[B >: A: ClassTag]: Array[B] = it match {
    case it: Iterable[B] => it.toArray[B]
    case _ => mutable.ArrayBuffer.from(it).toArray
  }

  @deprecated("Use List.from(it) instead of it.toList", "2.13.0")
  def toList: immutable.List[A] = immutable.List.from(it)

  @deprecated("Use Set.from(it) instead of it.toSet", "2.13.0")
  @`inline` def toSet[B >: A]: immutable.Set[B] = immutable.Set.from(it)

  @deprecated("Use Iterable.from(it) instead of it.toIterable", "2.13.0")
  @`inline` final def toIterable: Iterable[A] = Iterable.from(it)

  @deprecated("Use Seq.from(it) instead of it.toSeq", "2.13.0")
  @`inline` def toSeq: immutable.Seq[A] = immutable.Seq.from(it)

  @deprecated("Use Stream.from(it) instead of it.toStream", "2.13.0")
  @`inline` def toStream: immutable.Stream[A] = immutable.Stream.from(it)

  @deprecated("Use Vector.from(it) instead of it.toVector on IterableOnce", "2.13.0")
  @`inline` def toVector: immutable.Vector[A] = immutable.Vector.from(it)

  @deprecated("Use Map.from(it) instead of it.toVector on IterableOnce", "2.13.0")
  def toMap[K, V](implicit ev: A <:< (K, V)): immutable.Map[K, V] =
    immutable.Map.from(it.asInstanceOf[IterableOnce[(K, V)]])

  @deprecated("toIterator has been renamed to iterator", "2.13.0")
  @`inline` def toIterator: Iterator[A] = it.iterator

  @deprecated("Use .iterator.isEmpty instead of .isEmpty on IterableOnce", "2.13.0")
  def isEmpty: Boolean = it match {
    case it: Iterable[A] => it.isEmpty
    case _ => it.iterator.isEmpty
  }

  @deprecated("Use .iterator.mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(start: String, sep: String, end: String): String = it match {
    case it: Iterable[A] => it.mkString(start, sep, end)
    case _ => it.iterator.mkString(start, sep, end)
  }

  @deprecated("Use .iterator.mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString(sep: String): String = it match {
    case it: Iterable[A] => it.mkString(sep)
    case _ => it.iterator.mkString(sep)
  }

  @deprecated("Use .iterator.mkString instead of .mkString on IterableOnce", "2.13.0")
  def mkString: String = it match {
    case it: Iterable[A] => it.mkString
    case _ => it.iterator.mkString
  }

  @deprecated("Use .iterator.find instead of .find on IterableOnce", "2.13.0")
  def find(p: A => Boolean): Option[A] = it.iterator.find(p)

  @deprecated("Use .iterator.foldLeft instead of .foldLeft on IterableOnce", "2.13.0")
  @`inline` def foldLeft[B](z: B)(op: (B, A) => B): B = it.iterator.foldLeft(z)(op)

  @deprecated("Use .iterator.foldRight instead of .foldLeft on IterableOnce", "2.13.0")
  @`inline` def foldRight[B](z: B)(op: (A, B) => B): B = it.iterator.foldRight(z)(op)

  @deprecated("Use .iterator.fold instead of .fold on IterableOnce", "2.13.0")
  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = it.iterator.fold(z)(op)

  @deprecated("Use .iterator.foldLeft instead of /: on IterableOnce", "2.13.0")
  @`inline` def /: [B](z: B)(op: (B, A) => B): B = foldLeft[B](z)(op)

  @deprecated("Use .iterator.foldRight instead of :\\ on IterableOnce", "2.13.0")
  @`inline` def :\ [B](z: B)(op: (A, B) => B): B = foldRight[B](z)(op)

  @deprecated("Use .iterator.map instead of .map on IterableOnce or consider requiring an Iterable", "2.13.0")
  def map[B](f: A => B): IterableOnce[B] = it match {
    case it: Iterable[A] => it.asInstanceOf[Iterable[A]].map(f)
    case _ => it.iterator.map(f)
  }

  @deprecated("Use .iterator.flatMap instead of .flatMap on IterableOnce or consider requiring an Iterable", "2.13.0")
  def flatMap[B](f: A => IterableOnce[B]): IterableOnce[B] = it match {
    case it: Iterable[A] => it.asInstanceOf[Iterable[A]].flatMap(f)
    case _ => it.iterator.flatMap(f)
  }

  @deprecated("Use .iterator.sameElements for sameElements on Iterable or IterableOnce", "2.13.0")
  def sameElements[B >: A](that: IterableOnce[B]): Boolean = it.iterator.sameElements(that)
}

object IterableOnce {
  @`inline` implicit def iterableOnceExtensionMethods[A](it: IterableOnce[A]): IterableOnceExtensionMethods[A] =
    new IterableOnceExtensionMethods[A](it)
}

/** This implementation trait can be mixed into an `IterableOnce` to get the basic methods that are shared between
  * `Iterator` and `Iterable`. The `IterableOnce` must support multiple calls to `iterator` but may or may not
  * return the same `Iterator` every time.
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
    *  @note          Reuse: $consumesAndProducesIterator
    */
  def scanLeft[B](z: B)(op: (B, A) => B): CC[B]

  /** Selects all elements of this $coll which satisfy a predicate.
    *
    *  @param p     the predicate used to test elements.
    *  @return      a new iterator consisting of all elements of this $coll that satisfy the given
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

  /** Selects first ''n'' elements.
    *  $orderDependent
    *  @param  n    the number of elements to take from this $coll.
    *  @return a $coll consisting only of the first `n` elements of this $coll,
    *          or else the whole $coll, if it has less than `n` elements.
    *          If `n` is negative, returns an empty $coll.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def take(n: Int): C

  /** Takes longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest prefix of this $coll whose elements all satisfy
    *           the predicate `p`.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def takeWhile(p: A => Boolean): C

  /** Selects all elements except first ''n'' ones.
    *  $orderDependent
    *  @param  n    the number of elements to drop from this $coll.
    *  @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
    *          empty $coll, if this $coll has less than `n` elements.
    *          If `n` is negative, don't drop any elements.
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def drop(n: Int): C

  /** Drops longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest suffix of this $coll whose first element
    *           does not satisfy the predicate `p`.
    *  @note    Reuse: $consumesAndProducesIterator
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
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def slice(from: Int, until: Int): C

  /** Builds a new $coll by applying a function to all elements of this $coll.
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new $coll resulting from applying the given function
    *                `f` to each element of this $coll and collecting the results.
    *  @note    Reuse: $consumesAndProducesIterator
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
    *      def lettersOf(words: Seq[String]) = words.toSet flatMap (word => word.toSeq)
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
    *  @note    Reuse: $consumesAndProducesIterator
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
    *  @note    Reuse: $consumesAndProducesIterator
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
    *  @note    Reuse: $consumesAndProducesIterator
    */
  def collect[B](pf: PartialFunction[A, B]): CC[B]

  /** Zips this $coll with its indices.
    *
    *  @return        A new $coll containing pairs consisting of all elements of this $coll paired with their index.
    *                 Indices start at `0`.
    *  @example
    *    `List("a", "b", "c").zipWithIndex == List(("a", 0), ("b", 1), ("c", 2))`
    *  @note    Reuse: $consumesAndProducesIterator
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
    *  @note    Reuse: $consumesOneAndProducesTwoIterators
    */
  def span(p: A => Boolean): (C, C)

  /////////////////////////////////////////////////////////////// Concrete methods based on iterator

  /** The number of elements in this $coll, if it can be cheaply computed,
    *  -1 otherwise. Cheaply usually means: Not requiring a collection traversal.
    */
  def knownSize: Int = -1

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
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
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
    *           {{{
    *             op(x_1, op(x_2, ... op(x_n, z)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
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
    *           {{{
    *             op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
    *  @throws UnsupportedOperationException if this $coll is empty.   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    val it = iterator
    if (it.isEmpty)
      throw new UnsupportedOperationException("empty.reduceLeft")

    var first = true
    var acc: B = 0.asInstanceOf[B]

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
    *           {{{
    *             op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
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

  /** Copy elements to an array.
    *
    *  Fills the given array `xs` starting at index `start` with values of this $coll.
    *
    *  Copying will stop once either all the elements of this $coll have been copied,
    *  or the end of the array is reached.
    *
    *  @param  xs     the array to fill.
    *  @param  start  the starting index of xs.
    *  @tparam B      the type of the elements of the array.
    *
    *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
    *
    *  $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int = 0): xs.type = {
    val it = iterator
    var i = start
    while (i < xs.length && it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    xs
  }

  /** Copy elements to an array.
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
    *
    *  @note    Reuse: $consumesIterator
    *
    *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
    *
    *    $willNotTerminateInf
    */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): xs.type = {
    val it = iterator
    var i = start
    val end = start + math.min(len, xs.length - start)
    while (i < end && it.hasNext) {
      xs(i) = it.next()
      i += 1
    }
    xs
  }

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
    *    @throws   UnsupportedOperationException if this $coll is empty.
    */
  def min[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.min")
    reduceLeft((x, y) => if (ord.lteq(x, y)) x else y)
  }

  /** Finds the smallest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   an option value containing the smallest element of this $coll
    *            with respect to the ordering `ord`.
    *
    *  @usecase def min: A
    *    @inheritdoc
    *
    *    @return   an option value containing the smallest element of this $coll.
    */
  def minOption[B >: A](implicit ord: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(min(ord))
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
    *    @throws   UnsupportedOperationException if this $coll is empty.
    */
  def max[B >: A](implicit ord: Ordering[B]): A = {
    if (isEmpty)
      throw new UnsupportedOperationException("empty.max")
    reduceLeft((x, y) => if (ord.gteq(x, y)) x else y)
  }

  /** Finds the largest element.
    *
    *  @param    ord   An ordering to be used for comparing elements.
    *  @tparam   B    The type over which the ordering is defined.
    *  @return   an option value containing the largest element of this $coll with
    *            respect to the ordering `ord`.
    *
    *  @usecase def max: A
    *    @inheritdoc
    *
    *    @return   an option value containing the largest element of this $coll.
    */
  def maxOption[B >: A](implicit ord: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(max(ord))
  }

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the largest value measured by function f
    *            with respect to the ordering `cmp`.
    *
    *  @usecase def maxBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the largest value measured by function f.
    *    @throws   UnsupportedOperationException if this $coll is empty.
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

  /** Finds the first element which yields the largest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   an option value containing the first element of this $coll with the
    *            largest value measured by function f with respect to the ordering `cmp`.
    *
    *  @usecase def maxBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   an option value containing the first element of this $coll with
    *              the largest value measured by function f.
    */
  def maxByOption[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] = {
    if (isEmpty)
      None
    else
      Some(maxBy(f)(cmp))
  }

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   the first element of this $coll with the smallest value measured by function f
    *            with respect to the ordering `cmp`.
    *
    *  @usecase def minBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return   the first element of this $coll with the smallest value measured by function f.
    *    @throws   UnsupportedOperationException if this $coll is empty.
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

  /** Finds the first element which yields the smallest value measured by function f.
    *
    *  @param    cmp   An ordering to be used for comparing elements.
    *  @tparam   B     The result type of the function f.
    *  @param    f     The measuring function.
    *  @return   an option value containing the first element of this $coll
    *            with the smallest value measured by function f
    *            with respect to the ordering `cmp`.
    *
    *  @usecase def minBy[B](f: A => B): A
    *    @inheritdoc
    *
    *    @return  an option value containing the first element of this $coll with
    *             the smallest value measured by function f.
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
    val sentinel: scala.Function1[A, Any] = new scala.runtime.AbstractFunction1[A, Any]{ def apply(a: A) = this }
    val it = iterator
    while (it.hasNext) {
      val x = pf.applyOrElse(it.next(), sentinel)
      if (x.asInstanceOf[AnyRef] ne sentinel) return Some(x.asInstanceOf[B])
    }
    None
  }

  /** Displays all elements of this $coll in a string using start, end, and
    *  separator strings.
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
  def mkString(start: String, sep: String, end: String): String =
    addString(new StringBuilder(), start, sep, end).toString

  /** Displays all elements of this $coll in a string using a separator string.
    *
    *  @param sep   the separator string.
    *  @return      a string representation of this $coll. In the resulting string
    *               the string representations (w.r.t. the method `toString`)
    *               of all elements of this $coll are separated by the string `sep`.
    *
    *  @example  `List(1, 2, 3).mkString("|") = "1|2|3"`
    */
  def mkString(sep: String): String = mkString("", sep, "")

  /** Displays all elements of this $coll in a string.
    *
    *  @return a string representation of this $coll. In the resulting string
    *          the string representations (w.r.t. the method `toString`)
    *          of all elements of this $coll follow each other without any
    *          separator string.
    */
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
  def addString(b: StringBuilder, start: String, sep: String, end: String): b.type = {
    var first = true
    b.append(start)
    for (x <- this) {
      if (first) {
        b.append(x)
        first = false
      }
      else {
        b.append(sep)
        b.append(x)
      }
    }
    b.append(end)
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

  @deprecated("Use Stream.from(it) instead of it.toStream", "2.13.0")
  @`inline` final def toStream: immutable.Stream[A] = immutable.Stream.from(this)

  @deprecated("Use ArrayBuffer.from(it) instead of it.toBuffer", "2.13.0")
  @`inline` final def toBuffer[B >: A]: mutable.Buffer[B] = mutable.ArrayBuffer.from(this)

  /** Convert collection to array. */
  def toArray[B >: A: ClassTag]: Array[B] =
    if (knownSize >= 0) copyToArray(new Array[B](knownSize), 0)
    else mutable.ArrayBuffer.from(this).toArray[B]

  // For internal use
  protected def reversed: Iterable[A] = {
    var xs: immutable.List[A] = immutable.Nil
    val it = iterator
    while (it.hasNext) xs = it.next() :: xs
    xs
  }
}
