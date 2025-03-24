/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import scala.annotation.nowarn
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.Builder
import scala.collection.View.{LeftPartitionMapped, RightPartitionMapped}

/** Base trait for generic collections.
  *
  * @tparam A the element type of the collection
  *
  * @define Coll `Iterable`
  * @define coll iterable collection
  */
trait Iterable[+A] extends IterableOnce[A]
  with IterableOps[A, Iterable, Iterable[A]]
  with IterableFactoryDefaults[A, Iterable] {

  // The collection itself
  @deprecated("toIterable is internal and will be made protected; its name is similar to `toList` or `toSeq`, but it doesn't copy non-immutable collections", "2.13.7")
  final def toIterable: this.type = this

  final protected def coll: this.type = this

  def iterableFactory: IterableFactory[Iterable] = Iterable

  @deprecated("Iterable.seq always returns the iterable itself", "2.13.0")
  def seq: this.type = this

  /** Defines the prefix of this object's `toString` representation.
    *
    * It is recommended to return the name of the concrete collection type, but
    * not implementation subclasses. For example, for `ListMap` this method should
    * return `"ListMap"`, not `"Map"` (the supertype) or `"Node"` (an implementation
    * subclass).
    *
    * The default implementation returns "Iterable". It is overridden for the basic
    * collection kinds "Seq", "IndexedSeq", "LinearSeq", "Buffer", "Set", "Map",
    * "SortedSet", "SortedMap" and "View".
    *
    *  @return  a string representation which starts the result of `toString`
    *           applied to this $coll. By default the string prefix is the
    *           simple name of the collection class $coll.
    */
  protected[this] def className: String = stringPrefix

  /** Forwarder to `className` for use in `scala.runtime.ScalaRunTime`.
    *
    * This allows the proper visibility for `className` to be
    * published, but provides the exclusive access needed by
    * `scala.runtime.ScalaRunTime.stringOf` (and a few tests in
    * the test suite).
    */
  private[scala] final def collectionClassName: String = className

  @deprecatedOverriding("Override className instead", "2.13.0")
  protected[this] def stringPrefix: String = "Iterable"

  /** Converts this $coll to a string.
    *
    *  @return   a string representation of this collection. By default this
    *            string consists of the `className` of this $coll, followed
    *            by all elements separated by commas and enclosed in parentheses.
    */
  override def toString = mkString(className + "(", ", ", ")")

  /** Analogous to `zip` except that the elements in each collection are not consumed until a strict operation is
    * invoked on the returned `LazyZip2` decorator.
    *
    * Calls to `lazyZip` can be chained to support higher arities (up to 4) without incurring the expense of
    * constructing and deconstructing intermediary tuples.
    *
    * {{{
    *    val xs = List(1, 2, 3)
    *    val res = (xs lazyZip xs lazyZip xs lazyZip xs).map((a, b, c, d) => a + b + c + d)
    *    // res == List(4, 8, 12)
    * }}}
    *
    * @param that the iterable providing the second element of each eventual pair
    * @tparam B   the type of the second element in each eventual pair
    * @return a decorator `LazyZip2` that allows strict operations to be performed on the lazily evaluated pairs
    *         or chained calls to `lazyZip`. Implicit conversion to `Iterable[(A, B)]` is also supported.
    */
  def lazyZip[B](that: Iterable[B]): LazyZip2[A, B, this.type] = new LazyZip2(this, this, that)
}

/** Base trait for Iterable operations
  *
  * =VarianceNote=
  *
  * We require that for all child classes of Iterable the variance of
  * the child class and the variance of the `C` parameter passed to `IterableOps`
  * are the same. We cannot express this since we lack variance polymorphism. That's
  * why we have to resort at some places to write `C[A @uncheckedVariance]`.
  *
  * @tparam CC type constructor of the collection (e.g. `List`, `Set`). Operations returning a collection
  *            with a different type of element `B` (e.g. `map`) return a `CC[B]`.
  * @tparam C  type of the collection (e.g. `List[Int]`, `String`, `BitSet`). Operations returning a collection
  *            with the same type of element (e.g. `drop`, `filter`) return a `C`.
  *
  * @define Coll Iterable
  * @define coll iterable collection
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
  *  @define undefinedorder
  *  The order in which operations are performed on elements is unspecified
  *  and may be nondeterministic.
  */
trait IterableOps[+A, +CC[_], +C] extends Any with IterableOnce[A] with IterableOnceOps[A, CC, C] {
  /**
    * @return This collection as an `Iterable[A]`. No new collection will be built if `this` is already an `Iterable[A]`.
    */
  // Should be `protected def asIterable`, or maybe removed altogether if it's not needed
  @deprecated("toIterable is internal and will be made protected; its name is similar to `toList` or `toSeq`, but it doesn't copy non-immutable collections", "2.13.7")
  def toIterable: Iterable[A]

  /** Converts this $coll to an unspecified Iterable.  Will return
    *  the same collection if this instance is already Iterable.
    *  @return An Iterable containing all elements of this $coll.
    */
  @deprecated("toTraversable is internal and will be made protected; its name is similar to `toList` or `toSeq`, but it doesn't copy non-immutable collections", "2.13.0")
  final def toTraversable: Traversable[A] = toIterable

  override def isTraversableAgain: Boolean = true

  /**
    * @return This collection as a `C`.
    */
  protected def coll: C

  @deprecated("Use coll instead of repr in a collection implementation, use the collection value itself from the outside", "2.13.0")
  final def repr: C = coll

  /**
    * Defines how to turn a given `Iterable[A]` into a collection of type `C`.
    *
    * This process can be done in a strict way or a non-strict way (ie. without evaluating
    * the elements of the resulting collections). In other words, this methods defines
    * the evaluation model of the collection.
    *
    * @note When implementing a custom collection type and refining `C` to the new type, this
    *       method needs to be overridden (the compiler will issue an error otherwise). In the
    *       common case where `C =:= CC[A]`, this can be done by mixing in the
    *       [[scala.collection.IterableFactoryDefaults]] trait, which implements the method using
    *       [[iterableFactory]].
    *
    * @note As witnessed by the `@uncheckedVariance` annotation, using this method
    *       might be unsound. However, as long as it is called with an
    *       `Iterable[A]` obtained from `this` collection (as it is the case in the
    *       implementations of operations where we use a `View[A]`), it is safe.
    */
  protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): C

  /** The companion object of this ${coll}, providing various factory methods.
    *
    * @note When implementing a custom collection type and refining `CC` to the new type, this
    *       method needs to be overridden to return a factory for the new type (the compiler will
    *       issue an error otherwise).
    */
  def iterableFactory: IterableFactory[CC]

  @deprecated("Use iterableFactory instead", "2.13.0")
  @deprecatedOverriding("Use iterableFactory instead", "2.13.0")
  @`inline` def companion: IterableFactory[CC] = iterableFactory

  /**
    * @return a strict builder for the same collection type.
    *
    * Note that in the case of lazy collections (e.g. [[scala.collection.View]] or [[scala.collection.immutable.LazyList]]),
    * it is possible to implement this method but the resulting `Builder` will break laziness.
    * As a consequence, operations should preferably be implemented with `fromSpecific`
    * instead of this method.
    *
    * @note When implementing a custom collection type and refining `C` to the new type, this
    *       method needs to be overridden (the compiler will issue an error otherwise). In the
    *       common case where `C =:= CC[A]`, this can be done by mixing in the
    *       [[scala.collection.IterableFactoryDefaults]] trait, which implements the method using
    *       [[iterableFactory]].
    *
    * @note As witnessed by the `@uncheckedVariance` annotation, using this method might
    *       be unsound. However, as long as the returned builder is only fed
    *       with `A` values taken from `this` instance, it is safe.
    */
  protected def newSpecificBuilder: Builder[A @uncheckedVariance, C]

  /** The empty iterable of the same type as this iterable.
    *
    * @return an empty iterable of type `C`.
    */
  def empty: C = fromSpecific(Nil)

  /** Selects the first element of this $coll.
    *  $orderDependent
    *  @return  the first element of this $coll.
    *  @throws NoSuchElementException if the $coll is empty.
    */
  def head: A = iterator.next()

  /** Optionally selects the first element.
    *  $orderDependent
    *  @return  the first element of this $coll if it is nonempty,
    *           `None` if it is empty.
    */
  def headOption: Option[A] = {
    val it = iterator
    if (it.hasNext) Some(it.next()) else None
  }

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    val it = iterator
    var lst = it.next()
    while (it.hasNext) lst = it.next()
    lst
  }

  /** Optionally selects the last element.
    *  $orderDependent
    *  @return  the last element of this $coll$ if it is nonempty,
    *           `None` if it is empty.
    */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** A view over the elements of this collection. */
  def view: View[A] = View.fromIteratorProvider(() => iterator)

  /** Compares the size of this $coll to a test value.
    *
    *   @param   otherSize the test value that gets compared with the size.
    *   @return  A value `x` where
    *   {{{
    *        x <  0       if this.size <  otherSize
    *        x == 0       if this.size == otherSize
    *        x >  0       if this.size >  otherSize
    *   }}}
    *
    *  The method as implemented here does not call `size` directly; its running time
    *  is `O(size min otherSize)` instead of `O(size)`. The method should be overridden
    *  if computing `size` is cheap and `knownSize` returns `-1`.
    *
    *  @see [[sizeIs]]
    */
  def sizeCompare(otherSize: Int): Int = {
    if (otherSize < 0) 1
    else {
      val known = knownSize
      if (known >= 0) Integer.compare(known, otherSize)
      else {
        var i = 0
        val it = iterator
        while (it.hasNext) {
          if (i == otherSize) return 1
          it.next()
          i += 1
        }
        i - otherSize
      }
    }
  }

  /** Returns a value class containing operations for comparing the size of this $coll to a test value.
    *
    * These operations are implemented in terms of [[sizeCompare(Int) `sizeCompare(Int)`]], and
    * allow the following more readable usages:
    *
    * {{{
    * this.sizeIs < size     // this.sizeCompare(size) < 0
    * this.sizeIs <= size    // this.sizeCompare(size) <= 0
    * this.sizeIs == size    // this.sizeCompare(size) == 0
    * this.sizeIs != size    // this.sizeCompare(size) != 0
    * this.sizeIs >= size    // this.sizeCompare(size) >= 0
    * this.sizeIs > size     // this.sizeCompare(size) > 0
    * }}}
    */
  @inline final def sizeIs: IterableOps.SizeCompareOps = new IterableOps.SizeCompareOps(this)

  /** Compares the size of this $coll to the size of another `Iterable`.
    *
    *   @param   that the `Iterable` whose size is compared with this $coll's size.
    *   @return  A value `x` where
    *   {{{
    *        x <  0       if this.size <  that.size
    *        x == 0       if this.size == that.size
    *        x >  0       if this.size >  that.size
    *   }}}
    *
    *  The method as implemented here does not call `size` directly; its running time
    *  is `O(this.size min that.size)` instead of `O(this.size + that.size)`.
    *  The method should be overridden if computing `size` is cheap and `knownSize` returns `-1`.
    */
  def sizeCompare(that: Iterable[_]): Int = {
    val thatKnownSize = that.knownSize

    if (thatKnownSize >= 0) this sizeCompare thatKnownSize
    else {
      val thisKnownSize = this.knownSize

      if (thisKnownSize >= 0) {
        val res = that sizeCompare thisKnownSize
        // can't just invert the result, because `-Int.MinValue == Int.MinValue`
        if (res == Int.MinValue) 1 else -res
      } else {
        val thisIt = this.iterator
        val thatIt = that.iterator
        while (thisIt.hasNext && thatIt.hasNext) {
          thisIt.next()
          thatIt.next()
        }
        java.lang.Boolean.compare(thisIt.hasNext, thatIt.hasNext)
      }
    }
  }

  /** A view over a slice of the elements of this collection. */
  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  def view(from: Int, until: Int): View[A] = view.slice(from, until)

  /** Transposes this $coll of iterable collections into
    *  a $coll of ${coll}s.
    *
    *    The resulting collection's type will be guided by the
    *    static type of $coll. For example:
    *
    *    {{{
    *    val xs = List(
    *               Set(1, 2, 3),
    *               Set(4, 5, 6)).transpose
    *    // xs == List(
    *    //         List(1, 4),
    *    //         List(2, 5),
    *    //         List(3, 6))
    *
    *    val ys = Vector(
    *               List(1, 2, 3),
    *               List(4, 5, 6)).transpose
    *    // ys == Vector(
    *    //         Vector(1, 4),
    *    //         Vector(2, 5),
    *    //         Vector(3, 6))
    *    }}}
    *
    *  $willForceEvaluation
    *
    *  @tparam B the type of the elements of each iterable collection.
    *  @param  asIterable an implicit conversion which asserts that the
    *          element type of this $coll is an `Iterable`.
    *  @return a two-dimensional $coll of ${coll}s which has as ''n''th row
    *          the ''n''th column of this $coll.
    *  @throws IllegalArgumentException if all collections in this $coll
    *          are not of the same size.
    */
  def transpose[B](implicit asIterable: A => /*<:<!!!*/ Iterable[B]): CC[CC[B] @uncheckedVariance] = {
    if (isEmpty)
      return iterableFactory.empty[CC[B]]

    def fail = throw new IllegalArgumentException("transpose requires all collections have the same size")

    val headSize = asIterable(head).size
    val bs: scala.collection.immutable.IndexedSeq[Builder[B, CC[B]]] = scala.collection.immutable.IndexedSeq.fill(headSize)(iterableFactory.newBuilder[B])
    for (xs <- iterator) {
      var i = 0
      for (x <- asIterable(xs)) {
        if (i >= headSize) fail
        bs(i) += x
        i += 1
      }
      if (i != headSize)
        fail
    }
    iterableFactory.from(bs.map(_.result()))
  }

  def filter(pred: A => Boolean): C = fromSpecific(new View.Filter(this, pred, isFlipped = false))

  def filterNot(pred: A => Boolean): C = fromSpecific(new View.Filter(this, pred, isFlipped = true))

  /** Creates a non-strict filter of this $coll.
    *
    *  Note: the difference between `c filter p` and `c withFilter p` is that
    *        the former creates a new collection, whereas the latter only
    *        restricts the domain of subsequent `map`, `flatMap`, `foreach`,
    *        and `withFilter` operations.
    *  $orderDependent
    *
    *  @param p   the predicate used to test elements.
    *  @return    an object of class `WithFilter`, which supports
    *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
    *             All these operations apply to those elements of this $coll
    *             which satisfy the predicate `p`.
    */
  def withFilter(p: A => Boolean): collection.WithFilter[A, CC] = new IterableOps.WithFilter(this, p)

  /** A pair of, first, all elements that satisfy predicate `p` and, second,
   *  all elements that do not.
   *
   *  The two $coll correspond to the result of [[filter]] and [[filterNot]], respectively.
   *
   *  The default implementation provided here needs to traverse the collection twice.
   *  Strict collections have an overridden version of `partition` in `StrictOptimizedIterableOps`,
   *  which requires only a single traversal.
   */
  def partition(p: A => Boolean): (C, C) = {
    val first = new View.Filter(this, p, isFlipped = false)
    val second = new View.Filter(this, p, isFlipped = true)
    (fromSpecific(first), fromSpecific(second))
  }

  override def splitAt(n: Int): (C, C) = (take(n), drop(n))

  def take(n: Int): C = fromSpecific(new View.Take(this, n))

  /** Selects the last ''n'' elements.
    *  $orderDependent
    *  @param  n    the number of elements to take from this $coll.
    *  @return a $coll consisting only of the last `n` elements of this $coll,
    *          or else the whole $coll, if it has less than `n` elements.
    *          If `n` is negative, returns an empty $coll.
    */
  def takeRight(n: Int): C = fromSpecific(new View.TakeRight(this, n))

  /** Takes longest prefix of elements that satisfy a predicate.
    *  $orderDependent
    *  @param   p  The predicate used to test elements.
    *  @return  the longest prefix of this $coll whose elements all satisfy
    *           the predicate `p`.
    */
  def takeWhile(p: A => Boolean): C = fromSpecific(new View.TakeWhile(this, p))

  def span(p: A => Boolean): (C, C) = (takeWhile(p), dropWhile(p))

  def drop(n: Int): C = fromSpecific(new View.Drop(this, n))

  /** Selects all elements except last ''n'' ones.
    *  $orderDependent
    *  @param  n    the number of elements to drop from this $coll.
    *  @return a $coll consisting of all elements of this $coll except the last `n` ones, or else the
    *          empty $coll, if this $coll has less than `n` elements.
    *          If `n` is negative, don't drop any elements.
    */
  def dropRight(n: Int): C = fromSpecific(new View.DropRight(this, n))

  def dropWhile(p: A => Boolean): C = fromSpecific(new View.DropWhile(this, p))

  /** Partitions elements in fixed size ${coll}s.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be less than size `size` if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[C] =
    iterator.grouped(size).map(fromSpecific)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in `grouped`).
    *
    *  An empty collection returns an empty iterator, and a non-empty
    *  collection containing fewer elements than the window size returns
    *  an iterator that will produce the original collection as its only
    *  element.
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @return An iterator producing ${coll}s of size `size`, except for a
    *          non-empty collection with less than `size` elements, which
    *          returns an iterator that produces the source collection itself
    *          as its only element.
    *  @example `List().sliding(2) = empty iterator`
    *  @example `List(1).sliding(2) = Iterator(List(1))`
    *  @example `List(1, 2).sliding(2) = Iterator(List(1, 2))`
    *  @example `List(1, 2, 3).sliding(2) = Iterator(List(1, 2), List(2, 3))`
    */
  def sliding(size: Int): Iterator[C] = sliding(size, 1)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in `grouped`).
    *
    *  The returned iterator will be empty when called on an empty collection.
    *  The last element the iterator produces may be smaller than the window
    *  size when the original collection isn't exhausted by the window before
    *  it and its last element isn't skipped by the step before it.
    *
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @param step the distance between the first elements of successive
    *         groups
    *  @return An iterator producing ${coll}s of size `size`, except the last
    *          element (which may be the only element) will be smaller
    *          if there are fewer than `size` elements remaining to be grouped.
    *  @example `List(1, 2, 3, 4, 5).sliding(2, 2) = Iterator(List(1, 2), List(3, 4), List(5))`
    *  @example `List(1, 2, 3, 4, 5, 6).sliding(2, 3) = Iterator(List(1, 2), List(4, 5))`
    */
  def sliding(size: Int, step: Int): Iterator[C] =
    iterator.sliding(size, step).map(fromSpecific)

  /** The rest of the collection without its first element. */
  def tail: C = {
    if (isEmpty) throw new UnsupportedOperationException
    drop(1)
  }

  /** The initial part of the collection without its last element.
    * $willForceEvaluation
    */
  def init: C = {
    if (isEmpty) throw new UnsupportedOperationException
    dropRight(1)
  }

  def slice(from: Int, until: Int): C =
    fromSpecific(new View.Drop(new View.Take(this, until), from))

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    *  $willForceEvaluation
    *
    *  @param f     the discriminator function.
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @return      A map from keys to ${coll}s such that the following invariant holds:
    *               {{{
    *                 (xs groupBy f)(k) = xs filter (x => f(x) == k)
    *               }}}
    *               That is, every key `k` is bound to a $coll of those elements `x`
    *               for which `f(x)` equals `k`.
    *
    */
  def groupBy[K](f: A => K): immutable.Map[K, C] = {
    val m = mutable.Map.empty[K, Builder[A, C]]
    val it = iterator
    while (it.hasNext) {
      val elem = it.next()
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newSpecificBuilder)
      bldr += elem
    }
    var result = immutable.HashMap.empty[K, C]
    val mapIt = m.iterator
    while (mapIt.hasNext) {
      val (k, v) = mapIt.next()
      result = result.updated(k, v.result())
    }
    result
  }

  /**
    * Partitions this $coll into a map of ${coll}s according to a discriminator function `key`.
    * Each element in a group is transformed into a value of type `B` using the `value` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f))`, but more efficient.
    *
    * {{{
    *   case class User(name: String, age: Int)
    *
    *   def namesByAge(users: Seq[User]): Map[Int, Seq[String]] =
    *     users.groupMap(_.age)(_.name)
    * }}}
    *
    * $willForceEvaluation
    *
    * @param key the discriminator function
    * @param f the element transformation function
    * @tparam K the type of keys returned by the discriminator function
    * @tparam B the type of values returned by the transformation function
    */
  def groupMap[K, B](key: A => K)(f: A => B): immutable.Map[K, CC[B]] = {
    val m = mutable.Map.empty[K, Builder[B, CC[B]]]
    for (elem <- this) {
      val k = key(elem)
      val bldr = m.getOrElseUpdate(k, iterableFactory.newBuilder[B])
      bldr += f(elem)
    }
    class Result extends runtime.AbstractFunction1[(K, Builder[B, CC[B]]), Unit] {
      var built = immutable.Map.empty[K, CC[B]]
      def apply(kv: (K, Builder[B, CC[B]])) =
        built = built.updated(kv._1, kv._2.result())
    }
    val result = new Result
    m.foreach(result)
    result.built
  }

  /**
    * Partitions this $coll into a map according to a discriminator function `key`. All the values that
    * have the same discriminator are then transformed by the `f` function and then reduced into a
    * single value with the `reduce` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f).reduce(reduce))`, but more efficient.
    *
    * {{{
    *   def occurrences[A](as: Seq[A]): Map[A, Int] =
    *     as.groupMapReduce(identity)(_ => 1)(_ + _)
    * }}}
    *
    * $willForceEvaluation
    */
  def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): immutable.Map[K, B] = {
    val m = mutable.Map.empty[K, B]
    for (elem <- this) {
      val k = key(elem)
      val v =
        m.get(k) match {
          case Some(b) => reduce(b, f(elem))
          case None => f(elem)
        }
      m.put(k, v)
    }
    m.to(immutable.Map)
  }

  /** Computes a prefix scan of the elements of the collection.
    *
    *  Note: The neutral element `z` may be applied more than once.
    *
    *  @tparam B         element type of the resulting collection
    *  @param z          neutral element for the operator `op`
    *  @param op         the associative operator for the scan
    *
    *  @return           a new $coll containing the prefix scan of the elements in this $coll
    */
  def scan[B >: A](z: B)(op: (B, B) => B): CC[B] = scanLeft(z)(op)

  def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = iterableFactory.from(new View.ScanLeft(this, z, op))

  /** Produces a collection containing cumulative results of applying the operator going right to left.
    *  The head of the collection is the last cumulative result.
    *  $willNotTerminateInf
    *  $orderDependent
    *  $willForceEvaluation
    *
    *  Example:
    *  {{{
    *    List(1, 2, 3, 4).scanRight(0)(_ + _) == List(10, 9, 7, 4, 0)
    *  }}}
    *
    *  @tparam B      the type of the elements in the resulting collection
    *  @param z       the initial value
    *  @param op      the binary operator applied to the intermediate result and the element
    *  @return        collection with intermediate results
    */
  def scanRight[B](z: B)(op: (A, B) => B): CC[B] = {
    class Scanner extends runtime.AbstractFunction1[A, Unit] {
      var acc = z
      var scanned = acc :: immutable.Nil
      def apply(x: A) = {
        acc = op(x, acc)
        scanned ::= acc
      }
    }
    val scanner = new Scanner
    reversed.foreach(scanner)
    iterableFactory.from(scanner.scanned)
  }

  def map[B](f: A => B): CC[B] = iterableFactory.from(new View.Map(this, f))

  def flatMap[B](f: A => IterableOnce[B]): CC[B] = iterableFactory.from(new View.FlatMap(this, f))

  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B] = flatMap(asIterable)

  def collect[B](pf: PartialFunction[A, B]): CC[B] =
    iterableFactory.from(new View.Collect(this, pf))

  /** Applies a function `f` to each element of the $coll and returns a pair of ${coll}s: the first one
    *  made of those values returned by `f` that were wrapped in [[scala.util.Left]], and the second
    *  one made of those wrapped in [[scala.util.Right]].
    *
    *  Example:
    *  {{{
    *    val xs = $Coll(1, "one", 2, "two", 3, "three") partitionMap {
    *     case i: Int => Left(i)
    *     case s: String => Right(s)
    *    }
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three))
    *  }}}
    *
    *  @tparam A1  the element type of the first resulting collection
    *  @tparam A2  the element type of the second resulting collection
    *  @param f    the 'split function' mapping the elements of this $coll to an [[scala.util.Either]]
    *
    *  @return     a pair of ${coll}s: the first one made of those values returned by `f` that were wrapped in [[scala.util.Left]],
    *              and the second one made of those wrapped in [[scala.util.Right]].
    */
  def partitionMap[A1, A2](f: A => Either[A1, A2]): (CC[A1], CC[A2]) = {
    val left: View[A1] = new LeftPartitionMapped(this, f)
    val right: View[A2] = new RightPartitionMapped(this, f)
    (iterableFactory.from(left), iterableFactory.from(right))
  }

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix   the iterable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def concat[B >: A](suffix: IterableOnce[B]): CC[B] = iterableFactory.from(suffix match {
    case xs: Iterable[B] => new View.Concat(this, xs)
    case xs => iterator ++ suffix.iterator
  })

  /** Alias for `concat` */
  @`inline` final def ++ [B >: A](suffix: IterableOnce[B]): CC[B] = concat(suffix)

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   that  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new $coll containing pairs consisting of corresponding elements of this $coll and `that`.
    *                 The length of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](that: IterableOnce[B]): CC[(A @uncheckedVariance, B)] = iterableFactory.from(that match { // sound bcs of VarianceNote
    case that: Iterable[B] => new View.Zip(this, that)
    case _ => iterator.zip(that)
  })

  def zipWithIndex: CC[(A @uncheckedVariance, Int)] = iterableFactory.from(new View.ZipWithIndex(this))

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is shorter than the other,
    *  placeholder elements are used to extend the shorter collection to the length of the longer.
    *
    *  @param that     the iterable providing the second half of each result pair
    *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    *  @return        a new $coll containing pairs consisting of
    *                 corresponding elements of this $coll and `that`. The length
    *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
    *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    */
  def zipAll[A1 >: A, B](that: Iterable[B], thisElem: A1, thatElem: B): CC[(A1, B)] = iterableFactory.from(new View.ZipAll(this, that, thisElem, thatElem))

  /** Converts this $coll of pairs into two collections of the first and second
    *  half of each pair.
    *
    *    {{{
    *    val xs = $Coll(
    *               (1, "one"),
    *               (2, "two"),
    *               (3, "three")).unzip
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three))
    *    }}}
    *
    *  @tparam A1    the type of the first half of the element pairs
    *  @tparam A2    the type of the second half of the element pairs
    *  @param asPair an implicit conversion which asserts that the element type
    *                of this $coll is a pair.
    *  @return       a pair of ${coll}s, containing the first, respectively second
    *                half of each element pair of this $coll.
    */
  def unzip[A1, A2](implicit asPair: A => (A1, A2)): (CC[A1], CC[A2]) = {
    val first: View[A1] = new View.Map[A, A1](this, asPair(_)._1)
    val second: View[A2] = new View.Map[A, A2](this, asPair(_)._2)
    (iterableFactory.from(first), iterableFactory.from(second))
  }

  /** Converts this $coll of triples into three collections of the first, second,
    *  and third element of each triple.
    *
    *    {{{
    *    val xs = $Coll(
    *               (1, "one", '1'),
    *               (2, "two", '2'),
    *               (3, "three", '3')).unzip3
    *    // xs == ($Coll(1, 2, 3),
    *    //        $Coll(one, two, three),
    *    //        $Coll(1, 2, 3))
    *    }}}
    *
    *  @tparam A1       the type of the first member of the element triples
    *  @tparam A2       the type of the second member of the element triples
    *  @tparam A3       the type of the third member of the element triples
    *  @param asTriple  an implicit conversion which asserts that the element type
    *                   of this $coll is a triple.
    *  @return          a triple of ${coll}s, containing the first, second, respectively
    *                   third member of each element triple of this $coll.
    */
  def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (CC[A1], CC[A2], CC[A3]) = {
    val first: View[A1] = new View.Map[A, A1](this, asTriple(_)._1)
    val second: View[A2] = new View.Map[A, A2](this, asTriple(_)._2)
    val third: View[A3] = new View.Map[A, A3](this, asTriple(_)._3)
    (iterableFactory.from(first), iterableFactory.from(second), iterableFactory.from(third))
  }

  /** Iterates over the tails of this $coll. The first value will be this
    *  $coll and the final one will be an empty $coll, with the intervening
    *  values the results of successive applications of `tail`.
    *
    *  @return   an iterator over all the tails of this $coll
    *  @example  `List(1,2,3).tails = Iterator(List(1,2,3), List(2,3), List(3), Nil)`
    */
  def tails: Iterator[C] = iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this $coll. The first value will be this
    *  $coll and the final one will be an empty $coll, with the intervening
    *  values the results of successive applications of `init`.
    *
    *  $willForceEvaluation
    *
    *  @return  an iterator over all the inits of this $coll
    *  @example  `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
    */
  def inits: Iterator[C] = iterateUntilEmpty(_.init)

  override def tapEach[U](f: A => U): C = fromSpecific(new View.Map(this, { (a: A) => f(a); a }))

  // A helper for tails and inits.
  private[this] def iterateUntilEmpty(f: Iterable[A] => Iterable[A]): Iterator[C] = {
    // toIterable ties the knot between `this: IterableOnceOps[A, CC, C]` and `this.tail: C`
    // `this.tail.tail` doesn't compile as `C` is unbounded
    // `Iterable.from(this)` would eagerly copy non-immutable collections
    val it = Iterator.iterate(toIterable: @nowarn("cat=deprecation"))(f).takeWhile(_.nonEmpty)
    (it ++ Iterator.single(Iterable.empty)).map(fromSpecific)
  }

  @deprecated("Use ++ instead of ++: for collections of type Iterable", "2.13.0")
  def ++:[B >: A](that: IterableOnce[B]): CC[B] = iterableFactory.from(that match {
    case xs: Iterable[B] => new View.Concat(xs, this)
    case _ => that.iterator ++ iterator
  })
}

object IterableOps {

  /** Operations for comparing the size of a collection to a test value.
    *
    * These operations are implemented in terms of
    * [[scala.collection.IterableOps.sizeCompare(Int) `sizeCompare(Int)`]].
    */
  final class SizeCompareOps private[collection](val it: IterableOps[_, AnyConstr, _]) extends AnyVal {
    /** Tests if the size of the collection is less than some value. */
    @inline def <(size: Int): Boolean = it.sizeCompare(size) < 0
    /** Tests if the size of the collection is less than or equal to some value. */
    @inline def <=(size: Int): Boolean = it.sizeCompare(size) <= 0
    /** Tests if the size of the collection is equal to some value. */
    @inline def ==(size: Int): Boolean = it.sizeCompare(size) == 0
    /** Tests if the size of the collection is not equal to some value. */
    @inline def !=(size: Int): Boolean = it.sizeCompare(size) != 0
    /** Tests if the size of the collection is greater than or equal to some value. */
    @inline def >=(size: Int): Boolean = it.sizeCompare(size) >= 0
    /** Tests if the size of the collection is greater than some value. */
    @inline def >(size: Int): Boolean = it.sizeCompare(size) > 0
  }

  /** A trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
    * of trait `Iterable`.
    *
    * @tparam A Element type (e.g. `Int`)
    * @tparam CC Collection type constructor (e.g. `List`)
    *
    * @define coll collection
    */
  @SerialVersionUID(3L)
  class WithFilter[+A, +CC[_]](
    self: IterableOps[A, CC, _],
    p: A => Boolean
  ) extends collection.WithFilter[A, CC] with Serializable {

    protected def filtered: Iterable[A] =
      new View.Filter(self, p, isFlipped = false)

    def map[B](f: A => B): CC[B] =
      self.iterableFactory.from(new View.Map(filtered, f))

    def flatMap[B](f: A => IterableOnce[B]): CC[B] =
      self.iterableFactory.from(new View.FlatMap(filtered, f))

    def foreach[U](f: A => U): Unit = filtered.foreach(f)

    def withFilter(q: A => Boolean): WithFilter[A, CC] =
      new WithFilter(self, (a: A) => p(a) && q(a))

  }

}

@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable) {

  def single[A](a: A): Iterable[A] = new AbstractIterable[A] {
    override def iterator = Iterator.single(a)
    override def knownSize = 1
    override def head = a
    override def headOption: Some[A] = Some(a)
    override def last = a
    override def lastOption: Some[A] = Some(a)
    override def view: View.Single[A] = new View.Single(a)
    override def take(n: Int) = if (n > 0) this else Iterable.empty
    override def takeRight(n: Int) = if (n > 0) this else Iterable.empty
    override def drop(n: Int) = if (n > 0) Iterable.empty else this
    override def dropRight(n: Int) = if (n > 0) Iterable.empty else this
    override def tail: Iterable[Nothing] = Iterable.empty
    override def init: Iterable[Nothing] = Iterable.empty
  }
}

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[+A] extends Iterable[A]

/** This trait provides default implementations for the factory methods `fromSpecific` and
  * `newSpecificBuilder` that need to be refined when implementing a collection type that refines
  * the `CC` and `C` type parameters.
  *
  * The default implementations in this trait can be used in the common case when `CC[A]` is the
  * same as `C`.
  */
trait IterableFactoryDefaults[+A, +CC[x] <: IterableOps[x, CC, CC[x]]] extends IterableOps[A, CC, CC[A @uncheckedVariance]] {
  protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): CC[A @uncheckedVariance] = iterableFactory.from(coll)
  protected def newSpecificBuilder: Builder[A @uncheckedVariance, CC[A @uncheckedVariance]] = iterableFactory.newBuilder[A]

  // overridden for efficiency, since we know CC[A] =:= C
  override def empty: CC[A @uncheckedVariance] = iterableFactory.empty
}

/** This trait provides default implementations for the factory methods `fromSpecific` and
  * `newSpecificBuilder` that need to be refined when implementing a collection type that refines
  * the `CC` and `C` type parameters. It is used for collections that have an additional constraint,
  * expressed by the `evidenceIterableFactory` method.
  *
  * The default implementations in this trait can be used in the common case when `CC[A]` is the
  * same as `C`.
  */
trait EvidenceIterableFactoryDefaults[+A, +CC[x] <: IterableOps[x, CC, CC[x]], Ev[_]] extends IterableOps[A, CC, CC[A @uncheckedVariance]] {
  protected def evidenceIterableFactory: EvidenceIterableFactory[CC, Ev]
  implicit protected def iterableEvidence: Ev[A @uncheckedVariance]
  override protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): CC[A @uncheckedVariance] = evidenceIterableFactory.from(coll)
  override protected def newSpecificBuilder: Builder[A @uncheckedVariance, CC[A @uncheckedVariance]] = evidenceIterableFactory.newBuilder[A]
  override def empty: CC[A @uncheckedVariance] = evidenceIterableFactory.empty
}

/** This trait provides default implementations for the factory methods `fromSpecific` and
  * `newSpecificBuilder` that need to be refined when implementing a collection type that refines
  * the `CC` and `C` type parameters. It is used for sorted sets.
  *
  * Note that in sorted sets, the `CC` type of the set is not the same as the `CC` type for the
  * underlying iterable (which is fixed to `Set` in [[SortedSetOps]]). This trait has therefore
  * two type parameters `CC` and `WithFilterCC`. The `withFilter` method inherited from
  * `IterableOps` is overridden with a compatible default implementation.
  *
  * The default implementations in this trait can be used in the common case when `CC[A]` is the
  * same as `C`.
  */
trait SortedSetFactoryDefaults[+A,
    +CC[X] <: SortedSet[X] with SortedSetOps[X, CC, CC[X]],
    +WithFilterCC[x] <: IterableOps[x, WithFilterCC, WithFilterCC[x]] with Set[x]] extends SortedSetOps[A @uncheckedVariance, CC, CC[A @uncheckedVariance]] {
  self: IterableOps[A, WithFilterCC, _] =>

  override protected def fromSpecific(coll: IterableOnce[A @uncheckedVariance]): CC[A @uncheckedVariance]    = sortedIterableFactory.from(coll)(using ordering)
  override protected def newSpecificBuilder: mutable.Builder[A @uncheckedVariance, CC[A @uncheckedVariance]] = sortedIterableFactory.newBuilder[A](using ordering)
  override def empty: CC[A @uncheckedVariance] = sortedIterableFactory.empty(using ordering)

  override def withFilter(p: A => Boolean): SortedSetOps.WithFilter[A, WithFilterCC, CC] =
    new SortedSetOps.WithFilter[A, WithFilterCC, CC](this, p)
}


/** This trait provides default implementations for the factory methods `fromSpecific` and
  * `newSpecificBuilder` that need to be refined when implementing a collection type that refines
  * the `CC` and `C` type parameters. It is used for maps.
  *
  * Note that in maps, the `CC` type of the map is not the same as the `CC` type for the
  * underlying iterable (which is fixed to `Map` in [[MapOps]]). This trait has therefore
  * two type parameters `CC` and `WithFilterCC`. The `withFilter` method inherited from
  * `IterableOps` is overridden with a compatible default implementation.
  *
  * The default implementations in this trait can be used in the common case when `CC[A]` is the
  * same as `C`.
  */
trait MapFactoryDefaults[K, +V,
    +CC[x, y] <: IterableOps[(x, y), Iterable, Iterable[(x, y)]],
    +WithFilterCC[x] <: IterableOps[x, WithFilterCC, WithFilterCC[x]] with Iterable[x]] extends MapOps[K, V, CC, CC[K, V @uncheckedVariance]] with IterableOps[(K, V), WithFilterCC, CC[K, V @uncheckedVariance]] {
  override protected def fromSpecific(coll: IterableOnce[(K, V @uncheckedVariance)]): CC[K, V @uncheckedVariance] = mapFactory.from(coll)
  override protected def newSpecificBuilder: mutable.Builder[(K, V @uncheckedVariance), CC[K, V @uncheckedVariance]] = mapFactory.newBuilder[K, V]
  override def empty: CC[K, V @uncheckedVariance] = (this: AnyRef) match {
    // Implemented here instead of in TreeSeqMap since overriding empty in TreeSeqMap is not forwards compatible (should be moved)
    case self: immutable.TreeSeqMap[_, _] => immutable.TreeSeqMap.empty(self.orderedBy).asInstanceOf[CC[K, V]]
    case _ => mapFactory.empty
  }

  override def withFilter(p: ((K, V)) => Boolean): MapOps.WithFilter[K, V, WithFilterCC, CC] =
    new MapOps.WithFilter[K, V, WithFilterCC, CC](this, p)
}

/** This trait provides default implementations for the factory methods `fromSpecific` and
  * `newSpecificBuilder` that need to be refined when implementing a collection type that refines
  * the `CC` and `C` type parameters. It is used for sorted maps.
  *
  * Note that in sorted maps, the `CC` type of the map is not the same as the `CC` type for the
  * underlying map (which is fixed to `Map` in [[SortedMapOps]]). This trait has therefore
  * three type parameters `CC`, `WithFilterCC` and `UnsortedCC`. The `withFilter` method inherited
  * from `IterableOps` is overridden with a compatible default implementation.
  *
  * The default implementations in this trait can be used in the common case when `CC[A]` is the
  * same as `C`.
  */
trait SortedMapFactoryDefaults[K, +V,
    +CC[x, y] <:  Map[x, y] with SortedMapOps[x, y, CC, CC[x, y]] with UnsortedCC[x, y],
    +WithFilterCC[x] <: IterableOps[x, WithFilterCC, WithFilterCC[x]] with Iterable[x],
    +UnsortedCC[x, y] <: Map[x, y]] extends SortedMapOps[K, V, CC, CC[K, V @uncheckedVariance]] with MapOps[K, V, UnsortedCC, CC[K, V @uncheckedVariance]] {
  self: IterableOps[(K, V), WithFilterCC, _] =>

  override def empty: CC[K, V @uncheckedVariance] = sortedMapFactory.empty(using ordering)
  override protected def fromSpecific(coll: IterableOnce[(K, V @uncheckedVariance)]): CC[K, V @uncheckedVariance] = sortedMapFactory.from(coll)(using ordering)
  override protected def newSpecificBuilder: mutable.Builder[(K, V @uncheckedVariance), CC[K, V @uncheckedVariance]] = sortedMapFactory.newBuilder[K, V](using ordering)

  override def withFilter(p: ((K, V)) => Boolean): collection.SortedMapOps.WithFilter[K, V, WithFilterCC, UnsortedCC, CC] =
    new collection.SortedMapOps.WithFilter[K, V, WithFilterCC, UnsortedCC, CC](this, p)
}
