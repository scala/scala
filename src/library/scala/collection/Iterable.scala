package scala
package collection

import scala.annotation.unchecked.uncheckedVariance
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.ClassTag
import java.lang.{String, UnsupportedOperationException}

import scala.collection.mutable.{ArrayBuffer, Builder, StringBuilder}
import java.lang.String

/** Base trait for generic collections.
  *
  * @tparam A the element type of the collection
  *
  * @define Coll `Iterable`
  * @define coll iterable collection
  */
trait Iterable[+A] extends IterableOnce[A] with IterableOps[A, Iterable, Iterable[A]] {

  // The collection itself
  final def toIterable: this.type = this

  //TODO scalac generates an override for this in AbstractMap; Making it final leads to a VerifyError
  protected def coll: this.type = this

  protected def fromSpecificIterable(coll: Iterable[A @uncheckedVariance]): IterableCC[A] @uncheckedVariance = iterableFactory.from(coll)
  protected def newSpecificBuilder: Builder[A, IterableCC[A]] @uncheckedVariance = iterableFactory.newBuilder[A]

  /**
    * @note This operation '''has''' to be overridden by concrete collection classes to effectively
    *       return an `IterableFactory[IterableCC]`. The implementation in `Iterable` only returns
    *       an `IterableFactory[Iterable]`, but the compiler will '''not''' throw an error if the
    *       effective `IterableCC` type constructor is more specific than `Iterable`.
    *
    * @return The factory of this collection.
    */
  def iterableFactory: IterableFactory[IterableCC] = Iterable

  @deprecated("Iterable.seq always returns the iterable itself", "2.13.0")
  def seq: this.type = this
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
    * Type alias to `CC`. It is used to provide a default implementation of the `fromSpecificIterable`
    * and `newSpecificBuilder` operations.
    *
    * Due to the `@uncheckedVariance` annotation, usage of this type member can be unsound and is
    * therefore not recommended.
    */
  protected type IterableCC[X] = CC[X] @uncheckedVariance

  /**
    * @return This collection as an `Iterable[A]`. No new collection will be built if `this` is already an `Iterable[A]`.
    */
  def toIterable: Iterable[A]

  /**
    * @return This collection as a `C`.
    */
  protected def coll: C

  /**
    * Defines how to turn a given `Iterable[A]` into a collection of type `C`.
    *
    * This process can be done in a strict way or a non-strict way (ie. without evaluating
    * the elements of the resulting collections). In other words, this methods defines
    * the evaluation model of the collection.
    *
    * @note As witnessed by the `@uncheckedVariance` annotation, using this method
    *       might be unsound. However, as long as it is called with an
    *       `Iterable[A]` obtained from `this` collection (as it is the case in the
    *       implementations of operations where we use a `View[A]`), it is safe.
    */
  protected def fromSpecificIterable(coll: Iterable[A @uncheckedVariance]): C

  /** Similar to `fromSpecificIterable`, but for a (possibly) different type of element.
    * Note that the return type is now `CC[E]`.
    */
  @`inline` final protected def fromIterable[E](it: Iterable[E]): CC[E] = iterableFactory.from(it)

  /**
    * @return The companion object of this ${coll}, providing various factory methods.
    */
  def iterableFactory: IterableFactory[IterableCC]

  /**
    * @return a strict builder for the same collection type.
    *
    * Note that in the case of lazy collections (e.g. [[View]] or [[immutable.LazyList]]),
    * it is possible to implement this method but the resulting `Builder` will break laziness.
    * As a consequence, operations should preferably be implemented with `fromSpecificIterable`
    * instead of this method.
    *
    * @note As witnessed by the `@uncheckedVariance` annotation, using this method might
    *       be unsound. However, as long as the returned builder is only fed
    *       with `A` values taken from `this` instance, it is safe.
    */
  protected def newSpecificBuilder: Builder[A @uncheckedVariance, C]

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
    if(it.hasNext) Some(it.next()) else None
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

  @deprecated("Use .knownSize >=0 instead of .hasDefiniteSize", "2.13.0")
  @`inline` final def hasDefiniteSize = knownSize >= 0

  /** A view over the elements of this collection. */
  def view: View[A] = View.fromIteratorProvider(() => iterator)

  /** A view over a slice of the elements of this collection. */
  @deprecated("Use .view.slice(from, until) instead of .view(from, until)", "2.13.0")
  @`inline` final def view(from: Int, until: Int): View[A] = view.slice(from, until)

  /** Defines the prefix of this object's `toString` representation.
    *
    * It is recommended to return the name of the concrete collection type, but
    * not implementation subclasses. For example, for `ListMap` this method should
    * return `"ListMap"`, not `"Map"` (the supertype) or `"Node"` (an implementation
    * subclass).
    *
    * It is recommended to overwrite this method even if the default implementation
    * returns the correct name, to avoid the implementation using reflection.
    *
    *  @return  a string representation which starts the result of `toString`
    *           applied to this $coll. By default the string prefix is the
    *           simple name of the collection class $coll.
    */
  def className: String = {
    /* This method is written in a style that avoids calling `String.split()`
     * as well as methods of java.lang.Character that require the Unicode
     * database information. This is mostly important for Scala.js, so that
     * using the collection library does automatically bring java.util.regex.*
     * and the Unicode database in the generated code.
     *
     * This algorithm has the additional benefit that it won't allocate
     * anything except the result String in the common case, where the class
     * is not an inner class (i.e., when the result contains no '.').
     */
    val fqn = toIterable.getClass.getName
    var pos: Int = fqn.length - 1

    // Skip trailing $'s
    while (pos != -1 && fqn.charAt(pos) == '$') {
      pos -= 1
    }
    if (pos == -1 || fqn.charAt(pos) == '.') {
      return ""
    }

    var result: String = ""
    while (true) {
      // Invariant: if we enter the loop, there is a non-empty part

      // Look for the beginning of the part, remembering where was the last non-digit
      val partEnd = pos + 1
      while (pos != -1 && fqn.charAt(pos) <= '9' && fqn.charAt(pos) >= '0') {
        pos -= 1
      }
      val lastNonDigit = pos
      while (pos != -1 && fqn.charAt(pos) != '$' && fqn.charAt(pos) != '.') {
        pos -= 1
      }
      val partStart = pos + 1

      // A non-last part which contains only digits marks a method-local part -> drop the prefix
      if (pos == lastNonDigit && partEnd != fqn.length) {
        return result
      }

      // Skip to the next part, and determine whether we are the end
      while (pos != -1 && fqn.charAt(pos) == '$') {
        pos -= 1
      }
      val atEnd = pos == -1 || fqn.charAt(pos) == '.'

      // Handle the actual content of the part (we ignore parts that are likely synthetic)
      def isPartLikelySynthetic = {
        val firstChar = fqn.charAt(partStart)
        (firstChar > 'Z' && firstChar < 0x7f) || (firstChar < 'A')
      }
      if (atEnd || !isPartLikelySynthetic) {
        val part = fqn.substring(partStart, partEnd)
        result = if (result.isEmpty) part else part + '.' + result
        if (atEnd)
          return result
      }
    }

    // dead code
    result
  }

  @deprecated("Use className instead of stringPrefix", "2.13.0")
  @`inline` final def stringPrefix: String = className

  /** Converts this $coll to a string.
    *
    *  @return   a string representation of this collection. By default this
    *            string consists of the `className` of this $coll, followed
    *            by all elements separated by commas and enclosed in parentheses.
    */
  override def toString = mkString(className + "(", ", ", ")")

  //TODO Can there be a useful lazy implementation of this method? Otherwise mark it as being always strict
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
    fromIterable(bs.map(_.result()))
  }

  def filter(pred: A => Boolean): C = fromSpecificIterable(new View.Filter(this, pred, isFlipped = false))

  def filterNot(pred: A => Boolean): C = fromSpecificIterable(new View.Filter(this, pred, isFlipped = true))

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

  /** A pair of, first, all elements that satisfy prediacte `p` and, second,
    *  all elements that do not. Interesting because it splits a collection in two.
    *
    *  The default implementation provided here needs to traverse the collection twice.
    *  Strict collections have an overridden version of `partition` in `Buildable`,
    *  which requires only a single traversal.
    */
  def partition(p: A => Boolean): (C, C) = {
    val pn = new View.Partition(this, p)
    (fromSpecificIterable(pn.first), fromSpecificIterable(pn.second))
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
  def splitAt(n: Int): (C, C) = (take(n), drop(n))

  def take(n: Int): C = fromSpecificIterable(new View.Take(this, n))

  /** A collection containing the last `n` elements of this collection. */
  def takeRight(n: Int): C = {
    val b = newSpecificBuilder
    b.sizeHintBounded(n, toIterable)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      lead.next()
      it.next()
    }
    while (it.hasNext) b += it.next()
    b.result()
  }

  def takeWhile(p: A => Boolean): C = fromSpecificIterable(new View.TakeWhile(this, p))

  def span(p: A => Boolean): (C, C) = (takeWhile(p), dropWhile(p))

  def drop(n: Int): C = fromSpecificIterable(new View.Drop(this, n))

  /** The rest of the collection without its `n` last elements. For
    *  linear, immutable collections this should avoid making a copy.
    */
  def dropRight(n: Int): C = {
    val b = newSpecificBuilder
    if (n >= 0) b.sizeHint(toIterable, delta = -n)
    val lead = iterator drop n
    val it = iterator
    while (lead.hasNext) {
      b += it.next()
      lead.next()
    }
    b.result()
  }

  def dropWhile(p: A => Boolean): C = fromSpecificIterable(new View.DropWhile(this, p))

  /** Partitions elements in fixed size ${coll}s.
   *  @see [[scala.collection.Iterator]], method `grouped`
   *
   *  @param size the number of elements per group
   *  @return An iterator producing ${coll}s of size `size`, except the
   *          last will be less than size `size` if the elements don't divide evenly.
   */
  def grouped(size: Int): Iterator[C] =
    iterator.grouped(size).map(fromSpecificIterable)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in `grouped`.)
    *  The "sliding window" step is set to one.
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @return An iterator producing ${coll}s of size `size`, except the
    *          last element (which may be the only element) will be truncated
    *          if there are fewer than `size` elements remaining to be grouped.
    */
  def sliding(size: Int): Iterator[C] = sliding(size, 1)

  /** Groups elements in fixed size blocks by passing a "sliding window"
    *  over them (as opposed to partitioning them, as is done in grouped.)
    *  @see [[scala.collection.Iterator]], method `sliding`
    *
    *  @param size the number of elements per group
    *  @param step the distance between the first elements of successive
    *         groups
    *  @return An iterator producing ${coll}s of size `size`, except the
    *          last element (which may be the only element) will be truncated
    *          if there are fewer than `size` elements remaining to be grouped.
    */
  def sliding(size: Int, step: Int): Iterator[C] =
    iterator.sliding(size, step).map(fromSpecificIterable)

  /** The rest of the collection without its first element. */
  def tail: C = {
    if (isEmpty) throw new UnsupportedOperationException
    drop(1)
  }

  /** The initial part of the collection without its last element. */
  def init: C = {
    if (isEmpty) throw new UnsupportedOperationException
    dropRight(1)
  }

  def slice(from: Int, until: Int): C =
    fromSpecificIterable(new View.Drop(new View.Take(this, until), from))

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    *  Note: When applied to a view or a lazy collection it will always force the elements.
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
    var result = immutable.Map.empty[K, CC[B]]
    m.foreach { case (k, v) =>
      result = result + ((k, v.result()))
    }
    result
  }

  /**
    * Partitions this $coll into a map according to a discriminator function `key`. All the values that
    * have the same discriminator are then transformed by the `value` function and then reduced into a
    * single value with the `reduce` function.
    *
    * It is equivalent to `groupBy(key).mapValues(_.map(f).reduce(reduce))`, but more efficient.
    *
    * {{{
    *   def occurrences[A](as: Seq[A]): Map[A, Int] =
    *     as.groupMapReduce(identity)(_ => 1)(_ + _)
    * }}}
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

  def scanLeft[B](z: B)(op: (B, A) => B): CC[B] = fromIterable(new View.ScanLeft(this, z, op))

  /** Produces a collection containing cumulative results of applying the operator going right to left.
    *  The head of the collection is the last cumulative result.
    *  $willNotTerminateInf
    *  $orderDependent
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
    var scanned = z :: immutable.Nil
    var acc = z
    for (x <- reversed) {
      acc = op(x, acc)
      scanned ::= acc
    }
    fromIterable(scanned)
  }

  def map[B](f: A => B): CC[B] = fromIterable(new View.Map(this, f))

  def flatMap[B](f: A => IterableOnce[B]): CC[B] = fromIterable(new View.FlatMap(this, f))

  def flatten[B](implicit asIterable: A => IterableOnce[B]): CC[B] =
    fromIterable(new View.FlatMap(this, asIterable))

  def collect[B](pf: PartialFunction[A, B]): CC[B] =
    flatMap { a =>
      if (pf.isDefinedAt(a)) new View.Single(pf(a))
      else View.Empty
    }

  /** Returns a new $coll containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param suffix   the traversable to append.
    *  @tparam B     the element type of the returned collection.
    *  @return       a new $coll which contains all elements
    *                of this $coll followed by all elements of `suffix`.
    */
  def concat[B >: A](suffix: Iterable[B]): CC[B] = fromIterable(new View.Concat(this, suffix))

  /** Alias for `concat` */
  @`inline` final def ++ [B >: A](suffix: Iterable[B]): CC[B] = concat(suffix)

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is longer than the other, its remaining elements are ignored.
    *
    *  @param   that  The iterable providing the second half of each result pair
    *  @tparam  B     the type of the second half of the returned pairs
    *  @return        a new $coll containing pairs consisting of corresponding elements of this $coll and `that`.
    *                 The length of the returned collection is the minimum of the lengths of this $coll and `that`.
    */
  def zip[B](that: Iterable[B]): CC[(A @uncheckedVariance, B)] = fromIterable(new View.Zip(this, that))
  // sound bcs of VarianceNote

  def zipWithIndex: CC[(A @uncheckedVariance, Int)] = fromIterable(new View.ZipWithIndex(this))

  /** Returns a $coll formed from this $coll and another iterable collection
    *  by combining corresponding elements in pairs.
    *  If one of the two collections is shorter than the other,
    *  placeholder elements are used to extend the shorter collection to the length of the longer.
    *
    *  @param that     the iterable providing the second half of each result pair
    *  @param thisElem the element to be used to fill up the result if this $coll is shorter than `that`.
    *  @param thatElem the element to be used to fill up the result if `that` is shorter than this $coll.
    *  @return        a new collection of type `That` containing pairs consisting of
    *                 corresponding elements of this $coll and `that`. The length
    *                 of the returned collection is the maximum of the lengths of this $coll and `that`.
    *                 If this $coll is shorter than `that`, `thisElem` values are used to pad the result.
    *                 If `that` is shorter than this $coll, `thatElem` values are used to pad the result.
    */
  def zipAll[A1 >: A, B](that: Iterable[B], thisElem: A1, thatElem: B): CC[(A1, B)] = fromIterable(new View.ZipAll(this, that, thisElem, thatElem))

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
    val unzipped = new View.Unzip(this)
    (fromIterable(unzipped.first), fromIterable(unzipped.second))
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
    val unzipped = new View.Unzip3(this)
    (fromIterable(unzipped.first), fromIterable(unzipped.second), fromIterable(unzipped.third))
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
    *  @return  an iterator over all the inits of this $coll
    *  @example  `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
    */
  def inits: Iterator[C] = iterateUntilEmpty(_.init)

  // A helper for tails and inits.
  private[this] def iterateUntilEmpty(f: Iterable[A] => Iterable[A]): Iterator[C] = {
    val it = Iterator.iterate(toIterable)(f).takeWhile(x => !x.isEmpty)
    (it ++ Iterator(Iterable.empty)).map(fromSpecificIterable)
  }
}

object IterableOps {

  /** A trait that contains just the `map`, `flatMap`, `foreach` and `withFilter` methods
    * of trait `Iterable`.
    *
    * @tparam A Element type (e.g. `Int`)
    * @tparam CC Collection type constructor (e.g. `List`)
    *
    * @define coll collection
    */
  class WithFilter[+A, +CC[_]](
    self: IterableOps[A, CC, _],
    p: A => Boolean
  ) extends collection.WithFilter[A, CC] {

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

object Iterable extends IterableFactory.Delegate[Iterable](immutable.Iterable) {
  implicit def toLazyZipOps[A, CC[X] <: Iterable[X]](that: CC[A]): LazyZipOps[A, CC[A]] = new LazyZipOps(that)
}

/** Explicit instantiation of the `Iterable` trait to reduce class file size in subclasses. */
abstract class AbstractIterable[+A] extends Iterable[A]
