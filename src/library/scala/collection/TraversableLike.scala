/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
import generic._

import scala.reflect.ClassManifest
import mutable.{Builder, StringBuilder, Buffer, ArrayBuffer, ListBuffer}
import immutable.{List, Stream, Nil, ::}

/** A template trait for traversable collections of type `Traversable[A]`.
 *  $traversableinfo
 *
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @define traversableinfo
 *  This is a base trait of all kinds of Scala collections. It implements
 *  the behavior common to all collections, in terms of a method
 *  `foreach` with signature:
 *  {{{
 *     def foreach[U](f: Elem => U): Unit</pre>
 *  }}}
 *  Collection classes mixing in this trait provide a concrete
 *  `foreach` method which traverses all the
 *  elements contained in the collection, applying a given function to each.
 *  They also need to provide a method `newBuilder`
 *  which creates a builder for collections of the same kind.
 *
 *  A traversable class might or might not have two properties: strictness
 *  and orderedness. Neither is represented as a type.
 *
 *  The instances of a strict collection class have all their elements
 *  computed before they can be used as values. By contrast, instances of
 *  a non-strict collection class may defer computation of some of their
 *  elements until after the instance is available as a value.
 *  A typical example of a non-strict collection class is a
 *  <a href="../immutable/Stream.html" target="ContentFrame">
 *  `scala.collection.immutable.Stream`</a>.
 *  A more general class of examples are `TraversableViews`.
 *
 *  If a collection is an instance of an ordered collection class, traversing
 *  its elements with `foreach` will always visit elements in the
 *  same order, even for different runs of the program. If the class is not
 *  ordered, `foreach` can visit elements in different orders for
 *  different runs (but it will keep the same order in the same run).'
 *
 *  A typical example of a collection class which is not ordered is a
 *  `HashMap` of objects. The traversal order for hash maps will
 *  depend on the hash codes of its elements, and these hash codes might
 *  differ from one run to the next. By contrast, a `LinkedHashMap`
 *  is ordered because it's `foreach` method visits elements in the
 *  order they were inserted into the `HashMap`.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8

 *  @define Coll Traversable
 *  @define coll traversable collection
 *  @define thatinfo the class of the returned collection. Where possible, `That` is
 *    the same class as the current collection class `Repr`, but this
 *    depends on the element type `B` being admissible for that class,
 *    which means that an implicit instance of type `CanBuildFrom[Repr, B, That]`
 *    is found.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr` and
 *    and the new element type `B`.
 *  @define orderDependent
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *  @define orderDependentFold
 *
 *    Note: might return different results for different runs, unless the underlying collection type is ordered.
 *    or the operator is associative and commutative.
 *  @define mayNotTerminateInf
 *
 *    Note: may not terminate for infinite-sized collections.
 *  @define willNotTerminateInf
 *
 *    Note: will not terminate for infinite-sized collections.
 */
trait TraversableLike[+A, +Repr] extends HasNewBuilder[A, Repr] {
self =>

  import Traversable.breaks._

  /** The type implementing this traversable */
  protected type Self = Repr

  /** The collection of type $coll underlying this `TraversableLike` object.
   *  By default this is implemented as the `TraversableLike` object itself, but this can be overridden.
   */
  def repr: Repr = this.asInstanceOf[Repr]

  /** The underlying collection seen as an instance of `$Coll`.
   *  By default this is implemented as the current collection object itself,
   *  but this can be overridden.
   */
  protected[this] def thisCollection: Traversable[A] = this.asInstanceOf[Traversable[A]]

  /** A conversion from collections of type `Repr` to `$Coll` objects.
   *  By default this is implemented as just a cast, but this can be overridden.
   */
  protected[this] def toCollection(repr: Repr): Traversable[A] = repr.asInstanceOf[Traversable[A]]

  /** Creates a new builder for this collection type.
   */
  protected[this] def newBuilder: Builder[A, Repr]

  /** Applies a function `f` to all elements of this $coll.
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    It's important to implement this method in an efficient way.
   *
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   */
  def foreach[U](f: A => U): Unit

  /** Tests whether this $coll is empty.
   *
   *  @return    `true` if the $coll contain no elements, `false` otherwise.
   */
  def isEmpty: Boolean = {
    var result = true
    breakable {
      for (x <- this) {
        result = false
        break
      }
    }
    result
  }

  /** Tests whether the $coll is not empty.
   *
   *  @return    `true` if the $coll contains at least one element, `false` otherwise.
   */
  def nonEmpty: Boolean = !isEmpty

  /** The size of this $coll.
   *
   *  $willNotTerminateInf
   *
   *  @return    the number of elements in this $coll.
   */
  def size: Int = {
    var result = 0
    for (x <- this) result += 1
    result
  }

  /** Tests whether this $coll is known to have a finite size.
   *  All strict collections are known to have finite size. For a non-strict collection
   *  such as `Stream`, the predicate returns `true` if all elements have been computed.
   *  It returns `false` if the stream is not yet evaluated to the end.
   *
   *  Note: many collection methods will not work on collections of infinite sizes.
   *
   *  @return  `true` if this collection is known to have finite size, `false` otherwise.
   */
  def hasDefiniteSize = true

  /** Concatenates this $coll with the elements of a traversable collection.
   *
   *  @param that   the traversable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *
   *  @usecase def ++(that: Traversable[A]): $Coll[A]
   *
   *  @return       a new $coll which contains all elements of this $coll
   *                followed by all elements of `that`.
   */
  def ++[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b ++= that
    b.result
  }

  /** Concatenates this $coll with the elements of an iterator.
   *
   *  @param that   the iterator to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements of this $coll
   *                followed by all elements of `that`.
   *
   *  @usecase def ++(that: Iterator[A]): $Coll[A]
   *
   *  @return       a new $coll which contains all elements of this $coll
   *                followed by all elements of `that`.
   */
  def ++[B >: A, That](that: Iterator[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b ++= thisCollection
    b ++= that
    b.result
  }

  /** Builds a new collection by applying a function to all elements of this $coll.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   *
   *  @usecase def map[B](f: A => B): $Coll[B]
   *
   *  @return       a new $coll resulting from applying the given function
   *                `f` to each element of this $coll and collecting the results.
   */
  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) b += f(x)
    b.result
  }

  /** Builds a new collection by applying a function to all elements of this $coll
   *  and concatenating the results.
   *
   *  @param f      the function to apply to each element.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   *
   *  @usecase def flatMap[B](f: A => Traversable[B]): $Coll[B]
   *
   *  @return       a new $coll resulting from applying the given collection-valued function
   *                `f` to each element of this $coll and concatenating the results.
   */
  def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) b ++= f(x)
    b.result
  }

  /** Selects all elements of this $coll which satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filter(p: A => Boolean): Repr = {
    val b = newBuilder
    for (x <- this)
      if (p(x)) b += x
    b.result
  }

  /** Selects all elements of this $coll which do not satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filterNot(p: A => Boolean): Repr = filter(!p(_))

  /** Builds a new collection by applying a partial function to all elements of this $coll
   *  on which the function is defined.
   *
   *  @param pf     the partial function which filters and maps the $coll.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the partial function
   *                `pf` to each element on which it is defined and collecting the results.
   *                The order of the elements is preserved.
   *
   *  @usecase def partialMap[B](pf: PartialFunction[A, B]): $Coll[B]
   *
   *  @return       a new $coll resulting from applying the given partial function
   *                `pf` to each element on which it is defined and collecting the results.
   *                The order of the elements is preserved.
   */
  def partialMap[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this) if (pf.isDefinedAt(x)) b += pf(x)
    b.result
  }

  /** Builds a new collection by applying an option-valued function to all elements of this $coll
   *  on which the function is defined.
   *
   *  @param f      the option-valued function which filters and maps the $coll.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` resulting from applying the option-valued function
   *                `f` to each element and collecting all defined results.
   *                The order of the elements is preserved.
   *
   *  @usecase def filterMap[B](f: A => Option[B]): $Coll[B]
   *
   *  @param pf     the partial function which filters and maps the $coll.
   *  @return       a new $coll resulting from applying the given option-valued function
   *                `f` to each element and collecting all defined results.
   *                The order of the elements is preserved.
  def filterMap[B, That](f: A => Option[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    for (x <- this)
      f(x) match {
        case Some(y) => b += y
        case _ =>
      }
    b.result
  }
   */

  /** Partitions this $coll in two ${coll}s according to a predicate.
   *
   *  @param p the predicate on which to partition.
   *  @return  a pair of ${coll}s: the first $coll consists of all elements that
   *           satisfy the predicate `p` and the second $coll consists of all elements
   *           that don't. The relative order of the elements in the resulting ${coll}s
   *           is the same as in the original $coll.
   */
  def partition(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    for (x <- this) (if (p(x)) l else r) += x
    (l.result, r.result)
  }

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
   *
   *  Note: this method is not re-implemented by views. This means
   *        when applied to a view it will always force the view and
   *        return a new $coll.
   *
   *  @param f     the discriminator function.
   *  @tparam K    the type of keys returned by the discriminator function.
   *  @return      A map from keys to ${coll}s such that the following invariant holds:
   *               {{{
   *                 (xs partition f)(k) = xs filter (x => f(x) == k)
   *               }}}
   *               That is, every key `k` is bound to a $coll of those elements `x`
   *               for which `f(x)` equals `k`.
   *
   */
  def groupBy[K](f: A => K): Map[K, Repr] = {
    var m = Map[K, Builder[A, Repr]]()
    for (elem <- this) {
      val key = f(elem)
      val bldr = m get key match {
        case None => val b = newBuilder; m = m updated (key, b); b
        case Some(b) => b
      }
      bldr += elem
    }
    m mapValues (_.result)
  }

  /** Tests whether a predicate holds for all elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for all elements
   *                 of this $coll, otherwise `false`.
   */
  def forall(p: A => Boolean): Boolean = {
    var result = true
    breakable {
      for (x <- this)
        if (!p(x)) { result = false; break }
    }
    result
  }

  /** Tests whether a predicate holds for some of the elements of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `true` if the given predicate `p` holds for some of the elements
   *                 of this $coll, otherwise `false`.
   */
  def exists(p: A => Boolean): Boolean = {
    var result = false
    breakable {
      for (x <- this)
        if (p(x)) { result = true; break }
    }
    result
  }

  /** Counts the number of elements in the $coll which satisfy a predicate.
   *
   *  @param p     the predicate  used to test elements.
   *  @return      the number of elements satisfying the predicate `p`.
   *
   *
   */
  def count(p: A => Boolean): Int = {
    var cnt = 0
    for (x <- this) {
      if (p(x)) cnt += 1
    }
    cnt
  }

  /** Finds the first element of the $coll satisfying a predicate, if any.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param p    the predicate used to test elements.
   *  @return     an option value containing the first element in the $coll
   *              that satisfies `p`, or `None` if none exists.
   */
  def find(p: A => Boolean): Option[A] = {
    var result: Option[A] = None
    breakable {
      for (x <- this)
        if (p(x)) { result = Some(x); break }
    }
    result
  }

  /** Applies option-valued function to successive elements of this $coll
   *  until a defined value is found.
   *
   *  $mayNotTerminateInf
   *  $orderDependent
   *
   *  @param f    the function to be applied to successive elements.
   *  @return     an option value containing the first defined result of
   *              `f`, or `None` if `f` returns `None` for all all elements.
  def mapFind[B](f: A => Option[B]): Option[B] = {
    var result: Option[B] = None
    breakable {
      for (x <- this)
        f(x) match {
          case s @ Some(_) => result = s; break
          case _ =>
        }
    }
    result
  }
   */

  /** Applies a binary operator to a start value and all elements of this $coll, going left to right.
   *
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(z, x,,1,,), x,,2,,, ..., x,,n,,)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def foldLeft[B](z: B)(op: (B, A) => B): B = {
    var result = z
    for (x <- this)
      result = op(result, x)
    result
  }

  /** Applies a binary operator to a start value and all elements of this $coll, going left to right.
   *
   *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as `xs foldLeft z`.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going left to right with the start value `z` on the left:
   *           {{{
   *             op(...op(op(z, x,,1,,), x,,2,,), ..., x,,n,,)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def /: [B](z: B)(op: (B, A) => B): B = foldLeft(z)(op)

  /** Applies a binary operator to all elements of this $coll and a start value, going right to left.
   *
   *  $willNotTerminateInf
   *  $orderDependentFold
   *  @param   z    the start value.
   *  @param   op   the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going right to left with the start value `z` on the right:
   *           {{{
   *             op(x,,1,,, op(x,,2,,, ... op(x,,n,,, z)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def foldRight[B](z: B)(op: (A, B) => B): B = {
    var elems: List[A] = Nil
    for (x <- this) elems = x :: elems
    elems.foldLeft(z)((x, y) => op(y, x))
  }

  /** Applies a binary operator to all elements of this $coll and a start value, going right to left.
   *
   *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as `xs foldRight z`.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param   z    the start value
   *  @param   op   the binary operator
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going right to left with the start value `z` on the right:
   *           {{{
   *             op(x,,1,,, op(x,,2,,, ... op(x,,n,,, z)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   */
  def :\ [B](z: B)(op: (A, B) => B): B = foldRight(z)(op)

  /** Applies a binary operator to all elements of this $coll, going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going left to right:
   *           {{{
   *             op(...(op(x,,1,,, x,,2,,), ... ) , x,,n,,)
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *  @throws `UnsupportedOperationException` if this $coll is empty.
   */
  def reduceLeft[B >: A](op: (B, A) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    var result: B = head
    var first = true
    for (x <- this)
      if (first) first = false
      else result = op(result, x)
    result
  }

  /** Optionally applies a binary operator to all elements of this $coll, going left to right.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceLeft(op)` is this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] = {
    if (isEmpty) None else Some(reduceLeft(op))
  }

  /** Applies a binary operator to all elements of this $coll, going right to left.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  the result of inserting `op` between consecutive elements of this $coll$,
   *           going right to left:
   *           {{{
   *             op(x,,1,,, op(x,,2,,, ..., op(x,,n-1,,, x,,n,,)...))
   *           }}}
   *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
   *  @throws `UnsupportedOperationException` if this $coll is empty.
   */
  def reduceRight[B >: A](op: (A, B) => B): B = {
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceRight")
    var elems: List[A] = Nil
    for (x <- this) elems = x :: elems
    elems.reduceLeft[B]((x, y) => op(y, x))
  }

  /** Optionally applies a binary operator to all elements of this $coll, going right to left.
   *  $willNotTerminateInf
   *  $orderDependentFold
   *
   *  @param  op    the binary operator.
   *  @tparam  B    the result type of the binary operator.
   *  @return  an option value containing the result of `reduceRight(op)` is this $coll is nonempty,
   *           `None` otherwise.
   */
  def reduceRightOption[B >: A](op: (A, B) => B): Option[B] =
    if (isEmpty) None else Some(reduceRight(op))

  /** Sums up the elements of this collection.
   *
   *   @param   num  an implicit parameter defining a set of numeric operations
   *                 which includes the `+` operator to be used in forming the sum.
   *   @tparam  B    the result type of the `+` operator.
   *   @return       the sum of all elements of this $coll with respect to the `+` operator in `num`.
   *
   *   @usecase def sum: Int
   *
   *   @return       the sum of all elements in this $coll of numbers of type `Int`.
   *   Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
   *   can be used as element type of the $coll and as result type of `sum`.
   *   Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
   *
   */
  def sum[B >: A](implicit num: Numeric[B]): B = {
    var acc = num.zero
    for (x <- self) acc = num.plus(acc, x)
    acc
  }

  /** Multiplies up the elements of this collection.
   *
   *   @param   num  an implicit parameter defining a set of numeric operations
   *                 which includes the `*` operator to be used in forming the product.
   *   @tparam  B    the result type of the `*` operator.
   *   @return       the product of all elements of this $coll with respect to the `*` operator in `num`.
   *
   *   @usecase def product: Int
   *
   *   @return       the product of all elements in this $coll of numbers of type `Int`.
   *   Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
   *   can be used as element type of the $coll and as result type of `product`.
   *   Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
   */
  def product[B >: A](implicit num: Numeric[B]): B = {
    var acc = num.one
    for (x <- self) acc = num.times(acc, x)
    acc
  }

  /** Finds the smallest element.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The type over which the ordering is defined.
   *  @return   the smallest element of this $coll with respect to the ordering `cmp`.
   *
   *  @usecase def min: A
   *  @return   the smallest element of this $coll
   */
  def min[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty) throw new UnsupportedOperationException("empty.min")
    var acc = self.head
    for (x <- self)
      if (cmp.lt(x, acc)) acc = x
    acc
  }

  /** Finds the largest element.
   *
   *  @param    cmp   An ordering to be used for comparing elements.
   *  @tparam   B     The type over which the ordering is defined.
   *  @return   the largest element of this $coll with respect to the ordering `cmp`.
   *
   *  @usecase def min: A
   *  @return   the largest element of this $coll.
   */
  def max[B >: A](implicit cmp: Ordering[B]): A = {
    if (isEmpty) throw new UnsupportedOperationException("empty.max")
    var acc = self.head
    for (x <- self)
      if (cmp.gt(x, acc)) acc = x
    acc
  }

  /** Selects the first element of this $coll.
   *  $orderDependent
   *  @return  the first element of this $coll.
   *  @throws `NoSuchElementException` if the $coll is empty.
   */
  def head: A = {
    var result: () => A = () => throw new NoSuchElementException
    breakable {
      for (x <- this) {
        result = () => x
        break
      }
    }
    result()
  }

  /** Optionally selects the first element.
   *  $orderDependent
   *  @return  the first element of this $coll$ if it is nonempty, `None` if it is empty.
   */
  def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** Selects all elements except the first.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the first one.
   *  @throws `UnsupportedOperationException` if the $coll is empty.
   */
  def tail: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    drop(1)
  }

  /** Selects the last element.
   *  $orderDependent
   *  @return  the first element of this $coll.
   *  @throws `NoSuchElementException` if the $coll is empty.
   */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Optionally selects the last element.
   *  $orderDependent
   *  @return  the last element of this $coll$ if it is nonempty, `None` if it is empty.
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** Selects all elements except the last.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the last one.
   *  @throws `UnsupportedOperationException` if the $coll is empty.
   */
  def init: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    var lst = head
    var follow = false
    val b = newBuilder
    for (x <- this) {
      if (follow) b += lst
      else follow = true
      lst = x
    }
    b.result
  }

  /** Selects first ''n'' elements.
   *  $orderDependent
   *  @param  n    Tt number of elements to take from this $coll.
   *  @return a $coll consisting only of the first `n` elements of this $coll, or else the
   *          whole $coll, if it has less than `n` elements.
   */
  def take(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= n) break
        b += x
        i += 1
      }
    }
    b.result
  }

  /** Selects all elements except first ''n'' ones.
   *  $orderDependent
   *  @param  n    the number of elements to drop from this $coll.
   *  @return a $coll consisting of all elements of this $coll except the first `n` ones, or else the
   *          empty $coll, if this $coll has less than `n` elements.
   */
  def drop(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    for (x <- this) {
      if (i >= n) b += x
      i += 1
    }
    b.result
  }

  /** Selects an interval of elements.
   *
   *  Note: `c.slice(from, to)`  is equivalent to (but possibly more efficient than)
   *  `c.drop(from).take(to - from)`
   *  $orderDependent
   *
   *  @param from   the index of the first returned element in this $coll.
   *  @param until  the index one past the last returned element in this $coll.
   *  @return  a $coll containing the elements starting at index `from`
   *           and extending up to (but not including) index `until` of this $coll.
   */
  def slice(from: Int, until: Int): Repr = {
    val b = newBuilder
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from) b += x
        i += 1
        if (i == until) break
      }
    }
    b.result
  }

  /** Takes longest prefix of elements that satisfy a predicate.
   *  $orderDependent
   *  @param   p  The predicate used to test elements.
   *  @return  the longest prefix of this $coll whose elements all satisfy
   *           the predicate `p`.
   */
  def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    breakable {
      for (x <- this) {
        if (!p(x)) break
        b += x
      }
    }
    b.result
  }

  /** Drops longest prefix of elements that satisfy a predicate.
   *  $orderDependent
   *  @param   p  The predicate used to test elements.
   *  @return  the longest suffix of this $coll whose first element
   *           does not satisfy the predicate `p`.
   */
  def dropWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    var go = false
    for (x <- this) {
      if (!p(x)) go = true
      if (go) b += x
    }
    b.result
  }

  /** Spits this $coll into a prefix/suffix pair according to a predicate.
   *
   *  Note: `c span p`  is equivalent to (but possibly more efficient than)
   *  `(c takeWhile p, c dropWhile p)`, provided the evaluation of the predicate `p`
   *  does not cause any side-effects.
   *  $orderDependent
   *
   *  @param p the test predicate
   *  @return  a pair consisting of the longest prefix of this $coll whose
   *           elements all satisfy `p`, and the rest of this $coll.
   */
  def span(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  /** Splits this $coll into two at a given position.
   *  Note: `c splitAt n` is equivalent to (but possibly more efficient than)
   *         `(c take n, c drop n)`.
   *  $orderDependent
   *
   *  @param n the position at which to split.
   *  @return  a pair of ${coll}s consisting of the first `n`
   *           elements of this $coll, and the other elements.
   */
  def splitAt(n: Int): (Repr, Repr) = {
    val l, r = newBuilder
    var i = 0
    for (x <- this) {
      (if (i < n) l else r) += x
      i += 1
    }
    (l.result, r.result)
  }

  /** Copies all elements of this $coll to a buffer.
   *  $willNotTerminateInf
   *  @param  dest   The buffer to which elements are copied.
   */
  def copyToBuffer[B >: A](dest: Buffer[B]) {
    for (x <- this) dest += x
  }

  /** Copies elements of this $coll to an array.
   *  Fills the given array `xs` with at most `len` elements of
   *  this $coll, starting at position `start`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the array is reached, or `len` elements have been copied.
   *
   *  $willNotTerminateInf
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @param  len    the maximal number of elements to copy.
   *  @tparam B      the type of the elements of the array.
   *
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int, len: Int): Unit
   */
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
    var i = start
    val end = (start + len) min xs.length
    breakable {
      for (x <- this) {
        if (i >= end) break
        xs(i) = x
        i += 1
      }
    }
  }

  /** Copies elements of this $coll to an array.
   *  Fills the given array `xs` with all elements of
   *  this $coll, starting at position `start`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the array is reached.
   *
   *  $willNotTerminateInf
   *
   *  @param  xs     the array to fill.
   *  @param  start  the starting index.
   *  @tparam B      the type of the elements of the array.
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
   */
  def copyToArray[B >: A](xs: Array[B], start: Int) {
    copyToArray(xs, start, xs.length - start)
  }

  /** Copies elements of this $coll to an array.
   *  Fills the given array `xs` with all elements of
   *  this $coll, starting at position `0`.
   *  Copying will stop once either the end of the current $coll is reached,
   *  or the end of the array is reached.
   *
   *  $willNotTerminateInf
   *
   *  @param  xs     the array to fill.
   *  @tparam B      the type of the elements of the array.
   *
   *  @usecase def copyToArray(xs: Array[A], start: Int): Unit
   */
  def copyToArray[B >: A](xs: Array[B]) {
    copyToArray(xs, 0)
  }

  /** Converts this $coll to an array.
   *  $willNotTerminateInf
   *
   *  @tparam B    the type of the elements of the array. A `ClassManifest` for this type must
   *               be available.
   *  @return  an array containing all elements of this $coll.
   *
   *  @usecase def toArray: Array[A]
   *  @return  an array containing all elements of this $coll.
   *           A `ClassManifest` must be available for the element type of this $coll.
   */
  def toArray[B >: A : ClassManifest]: Array[B] = {
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }

  /** Converts this $coll to a list.
   *  $willNotTerminateInf
   *  @return a list containing all elements of this $coll.
   */
  def toList: List[A] = (new ListBuffer[A] ++= thisCollection).toList

  /** Converts this $coll to an iterable collection.
   *  $willNotTerminateInf
   *  @return an `Iterable` containing all elements of this $coll.
   */
  def toIterable: Iterable[A] = toStream

  /** Converts this $coll to a sequence.
   *  $willNotTerminateInf
   *  @return a sequence containing all elements of this $coll.
   */
  def toSeq: Seq[A] = toList

  /** Converts this $coll to an indexed sequence.
   *  $willNotTerminateInf
   *  @return an indexed sequence containing all elements of this $coll.
   */
  def toIndexedSeq[B >: A]: mutable.IndexedSeq[B] = (new ArrayBuffer[B] ++= thisCollection)

  /** Converts this $coll to a stream.
   *  $willNotTerminateInf
   *  @return a stream containing all elements of this $coll.
   */
  def toStream: Stream[A] = toList.toStream

  /** Converts this $coll to a set.
   *  $willNotTerminateInf
   *  @return      a set containing all elements of this $coll.
   */
  def toSet[B >: A]: immutable.Set[B] = immutable.Set() ++ thisCollection

  /** Converts this $coll to a map.  This method is unavailable unless
   *  the elements are members of Tuple2, each ((K, V)) becoming a key-value
   *  pair in the map.  Duplicate keys will be overwritten by later keys:
   *  if this is an unordered collection, which key is in the resulting map
   *  is undefined.
   *  $willNotTerminateInf
   *  @return      a map containing all elements of this $coll.
   */
  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    val b = immutable.Map.newBuilder[T, U]
    for (x <- this)
      b += x

    b.result
  }

  /** Displays all elements of this $coll in a string using start, end, and separator strings.
   *
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      a string representation of this $coll. The resulting string
   *               begins with the string `start` and ends with the string
   *               `end`. Inside, the string representations (w.r.t. the method `toString`)
   *               of all elements of this $coll are separated by the string `sep`.
   *
   *  @ex  `List(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
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
   *  @ex  `List(1, 2, 3).mkString("|") = "1|2|3"`
   */
  def mkString(sep: String): String =
    addString(new StringBuilder(), sep).toString

  /** Displays all elements of this $coll in a string.
   *  @return a string representation of this $coll. In the resulting string
   *          the string representations (w.r.t. the method `toString`)
   *          of all elements of this $coll follow each other without any separator string.
   */
  def mkString: String =
    addString(new StringBuilder()).toString

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
   *  The written text begins with the string `start` and ends with the string
   *  `end`. Inside, the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll are separated by the string `sep`.
   *
   *  @param  b    the string builder to which elements are appended.
   *  @param start the starting string.
   *  @param sep   the separator string.
   *  @param end   the ending string.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b append start
    var first = true
    for (x <- this) {
      if (first) first = false
      else b append sep
      b append x
    }
    b append end
  }

  /** Appends all elements of this $coll to a string builder using a separator string.
   *  The written text consists of the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll, separated by the string `sep`.
   *
   *  @param  b    the string builder to which elements are appended.
   *  @param sep   the separator string.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder, sep: String): StringBuilder = addString(b, "", sep, "")

  /** Appends all elements of this $coll to a string builder.
   *  The written text consists of the string representations (w.r.t. the method `toString`)
   *  of all elements of this $coll without any separator string.
   *
   *  @param  b    the string builder to which elements are appended.
   *  @return      the string builder `b` to which elements were appended.
   */
  def addString(b: StringBuilder): StringBuilder = addString(b, "")

  /** Converts this $coll to a string
   *  @returns  a string representation of this collection. By default this
   *            string consists of the `stringPrefix` of this $coll,
   *            followed by all elements separated by commas and enclosed in parentheses.
   */
  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's `toString` representation.
   *  @return  a string representation which starts the result of `toString` applied to this $coll.
   *           By default the string prefix is the simple name of the collection class $coll.
   */
  def stringPrefix : String = {
    var string = repr.asInstanceOf[AnyRef].getClass.getName
    val idx1 = string.lastIndexOf('.' : Int)
    if (idx1 != -1) string = string.substring(idx1 + 1)
    val idx2 = string.indexOf('$')
    if (idx2 != -1) string = string.substring(0, idx2)
    string
  }

  /** Creates a non-strict view of this $coll.
   *
   *  @return a non-strict view of this $coll.
   */
  def view = new TraversableView[A, Repr] {
    protected lazy val underlying = self.repr
    override def foreach[B](f: A => B) = self foreach f
  }

  /** Creates a non-strict view of a slice of this $coll.
   *
   *  Note: the difference between `view` and `slice` is that `view` produces
   *        a view of the current $coll, whereas `slice` produces a new $coll.
   *
   *  Note: `view(from, to)` is equivalent to `view.slice(from, to)`
   *  $orderDependent
   *
   *  @param from   the index of the first element of the view
   *  @param until  the index of the element following the view
   *  @return a non-strict view of a slice of this $coll, starting at index `from`
   *  and extending up to (but not including) index `until`.
   */
  def view(from: Int, until: Int): TraversableView[A, Repr] = view.slice(from, until)

  /** Creates a non-strict filter of this $coll.
   *
   *  Note: the difference between `c filter p` and `c withFilter p` is that
   *        the former creates a new collection, whereas the latter only restricts
   *        the domain of subsequent `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *  $orderDependent
   *
   *  @param p   the predicate used to test elements.
   *  @return    an object of class `WithFilter`, which supports
   *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
   *             All these operations apply to those elements of this $coll which
   *             satify the predicate `p`.
   */
  def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** A class supporting filtered operations. Instances of this class are returned by
   *  method `withFilter`.
   */
  class WithFilter(p: A => Boolean) {

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy predicate `p`.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying the given function
     *                `f` to each element of the outer $coll that satisfies predicate `p`
     *                and collecting the results.
     *
     *  @usecase def map[B](f: A => B): $Coll[B]
     *
     *  @return       a new $coll resulting from applying the given function
     *                `f` to each element of the outer $coll that satisfies predicate `p`
     *                and collecting the results.
     */
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b += f(x)
      b.result
    }

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy predicate `p` and concatenating the results.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying the given collection-valued function
     *                `f` to each element of the outer $coll that satisfies predicate `p` and concatenating the results.
     *
     *  @usecase def flatMap[B](f: A => Traversable[B]): $Coll[B]
     *
     *  @return       a new $coll resulting from applying the given collection-valued function
     *                `f` to each element of the outer $coll that satisfies predicate `p` and concatenating the results.
     */
    def flatMap[B, That](f: A => Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b ++= f(x)
      b.result
    }

    /** Applies a function `f` to all elements of the outer $coll containing this `WithFilter` instance
     *  that satisfy predicate `p`.
     *
     *  @param  f   the function that is applied for its side-effect to every element.
     *              The result of function `f` is discarded.
     *
     *  @tparam  U  the type parameter describing the result of function `f`.
     *              This result will always be ignored. Typically `U` is `Unit`,
     *              but this is not necessary.
     *
     *  @usecase def foreach(f: A => Unit): Unit
     */
    def foreach[U](f: A => U): Unit =
      for (x <- self)
        if (p(x)) f(x)

    /** Further refines the filter for this $coll.
     *
     *  @param q   the predicate used to test elements.
     *  @return    an object of class `WithFilter`, which supports
     *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
     *             All these operations apply to those elements of this $coll which
     *             satify the predicate `q` in addition to the predicate `p`.
     */
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }
}
