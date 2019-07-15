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

import generic._
import mutable.Builder
import scala.annotation.{migration, tailrec}
import scala.annotation.unchecked.{uncheckedVariance => uV}
import parallel.ParIterable
import scala.collection.immutable.{::, List, Nil}
import scala.language.higherKinds

/** A template trait for traversable collections of type `Traversable[A]`.
 *
 *  $traversableInfo
 *  @define mutability
 *  @define traversableInfo
 *  This is a base trait of all kinds of $mutability Scala collections. It
 *  implements the behavior common to all collections, in terms of a method
 *  `foreach` with signature:
 * {{{
 *     def foreach[U](f: Elem => U): Unit
 * }}}
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
 *  [[scala.collection.immutable.Stream]].
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
 *  is ordered because its `foreach` method visits elements in the
 *  order they were inserted into the `HashMap`.
 *
 *  @author Martin Odersky
 *  @since   2.8
 *  @tparam A    the element type of the collection
 *  @tparam Repr the type of the actual collection containing the elements.
 *
 *  @define Coll Traversable
 *  @define coll traversable collection
 */
trait TraversableLike[+A, +Repr] extends Any
                                    with HasNewBuilder[A, Repr]
                                    with FilterMonadic[A, Repr]
                                    with TraversableOnce[A]
                                    with GenTraversableLike[A, Repr]
                                    with Parallelizable[A, ParIterable[A]]
{
  self =>

  import Traversable.breaks._

  /** The type implementing this traversable */
  protected[this] type Self = Repr

  /** The collection of type $coll underlying this `TraversableLike` object.
   *  By default this is implemented as the `TraversableLike` object itself,
   *  but this can be overridden.
   */
  def repr: Repr = this.asInstanceOf[Repr]

  final def isTraversableAgain: Boolean = true

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

  protected[this] def parCombiner = ParIterable.newCombiner[A]

  /** Applies a function `f` to all elements of this $coll.
   *
   *  @param  f   the function that is applied for its side-effect to every element.
   *              The result of function `f` is discarded.
   *
   *  @tparam  U  the type parameter describing the result of function `f`.
   *              This result will always be ignored. Typically `U` is `Unit`,
   *              but this is not necessary.
   *
   *  @usecase def foreach(f: A => Unit): Unit
   *    @inheritdoc
   *
   *    Note: this method underlies the implementation of most other bulk operations.
   *    It's important to implement this method in an efficient way.
   *
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

  def hasDefiniteSize = true

  def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.seq.size)
    b ++= thisCollection
    b ++= that.seq
    b.result
  }

  /** As with `++`, returns a new collection containing the elements from the left operand followed by the
   *  elements from the right operand.
   *
   *  It differs from `++` in that the right operand determines the type of
   *  the resulting collection rather than the left one.
   *  Mnemonic: the COLon is on the side of the new COLlection type.
   *
   *  @param that   the traversable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   *
   *  @usecase def ++:[B](that: TraversableOnce[B]): $Coll[B]
   *    @inheritdoc
   *
   *    Example:
   *    {{{
   *      scala> val x = List(1)
   *      x: List[Int] = List(1)
   *
   *      scala> val y = LinkedList(2)
   *      y: scala.collection.mutable.LinkedList[Int] = LinkedList(2)
   *
   *      scala> val z = x ++: y
   *      z: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *    }}}
   *
   *    @return       a new $coll which contains all elements of this $coll
   *                  followed by all elements of `that`.
   */
  def ++:[B >: A, That](that: TraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    if (that.isInstanceOf[IndexedSeqLike[_, _]]) b.sizeHint(this, that.size)
    b ++= that
    b ++= thisCollection
    b.result
  }

  /** As with `++`, returns a new collection containing the elements from the
   *  left operand followed by the elements from the right operand.
   *
   *  It differs from `++` in that the right operand determines the type of
   *  the resulting collection rather than the left one.
   *  Mnemonic: the COLon is on the side of the new COLlection type.
   *
   *  Example:
   *  {{{
   *     scala> val x = List(1)
   *     x: List[Int] = List(1)
   *
   *     scala> val y = LinkedList(2)
   *     y: scala.collection.mutable.LinkedList[Int] = LinkedList(2)
   *
   *     scala> val z = x ++: y
   *     z: scala.collection.mutable.LinkedList[Int] = LinkedList(1, 2)
   *  }}}
   *
   * This overload exists because: for the implementation of `++:` we should
   *  reuse that of `++` because many collections override it with more
   *  efficient versions.
   *
   *  Since `TraversableOnce` has no `++` method, we have to implement that
   *  directly, but `Traversable` and down can use the overload.
   *
   *  @param that   the traversable to append.
   *  @tparam B     the element type of the returned collection.
   *  @tparam That  $thatinfo
   *  @param bf     $bfinfo
   *  @return       a new collection of type `That` which contains all elements
   *                of this $coll followed by all elements of `that`.
   */
  def ++:[B >: A, That](that: Traversable[B])(implicit bf: CanBuildFrom[Repr, B, That]): That =
    (that ++ seq)(breakOut)

  def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    def builder = { // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
      val b = bf(repr)
      b.sizeHint(this)
      b
    }
    val b = builder
    for (x <- this) b += f(x)
    b.result
  }

  def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    def builder = bf(repr) // extracted to keep method size under 35 bytes, so that it can be JIT-inlined
    val b = builder
    for (x <- this) b ++= f(x).seq
    b.result
  }

  private[scala] def filterImpl(p: A => Boolean, isFlipped: Boolean): Repr = {
    this match {
      case as: List[A] =>
        filterImplList(as, p, isFlipped).asInstanceOf[Repr]
      case _ =>
        val b = newBuilder
        for (x <- this)
          if (p(x) != isFlipped) b += x

        b.result
    }
  }

  private[this] def filterImplList[A](self: List[A], p: A => Boolean, isFlipped: Boolean): List[A] = {

    // everything seen so far so far is not included
    @tailrec def noneIn(l: List[A]): List[A] = {
      if (l.isEmpty)
        Nil
      else {
        val h = l.head
        val t = l.tail
        if (p(h) != isFlipped)
          allIn(l, t)
        else
          noneIn(t)
      }
    }

    // everything from 'start' is included, if everything from this point is in we can return the origin
    // start otherwise if we discover an element that is out we must create a new partial list.
    @tailrec def allIn(start: List[A], remaining: List[A]): List[A] = {
      if (remaining.isEmpty)
        start
      else {
        val x = remaining.head
        if (p(x) != isFlipped)
          allIn(start, remaining.tail)
        else
          partialFill(start, remaining)
      }
    }

    // we have seen elements that should be included then one that should be excluded, start building
    def partialFill(origStart: List[A], firstMiss: List[A]): List[A] = {
      val newHead = new ::(origStart.head, Nil)
      var toProcess = origStart.tail
      var currentLast = newHead

      // we know that all elements are :: until at least firstMiss.tail
      while (!(toProcess eq firstMiss)) {
        val newElem = new ::(toProcess.head, Nil)
        currentLast.tl = newElem
        currentLast = newElem
        toProcess = toProcess.tail
      }

      // at this point newHead points to a list which is a duplicate of all the 'in' elements up to the first miss.
      // currentLast is the last element in that list.

      // now we are going to try and share as much of the tail as we can, only moving elements across when we have to.
      var next = firstMiss.tail
      var nextToCopy = next // the next element we would need to copy to our list if we cant share.
      while (!next.isEmpty) {
        // generally recommended is next.isNonEmpty but this incurs an extra method call.
        val head: A = next.head
        if (p(head) != isFlipped) {
          next = next.tail
        } else {
          // its not a match - do we have outstanding elements?
          while (!(nextToCopy eq next)) {
            val newElem = new ::(nextToCopy.head, Nil)
            currentLast.tl = newElem
            currentLast = newElem
            nextToCopy = nextToCopy.tail
          }
          nextToCopy = next.tail
          next = next.tail
        }
      }

      // we have remaining elements - they are unchanged attach them to the end
      if (!nextToCopy.isEmpty)
        currentLast.tl = nextToCopy

      newHead
    }

    val result = noneIn(self)
    result
  }

  /** Selects all elements of this $coll which satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filter(p: A => Boolean): Repr = filterImpl(p, isFlipped = false)

  /** Selects all elements of this $coll which do not satisfy a predicate.
   *
   *  @param p     the predicate used to test elements.
   *  @return      a new $coll consisting of all elements of this $coll that do not satisfy the given
   *               predicate `p`. The order of the elements is preserved.
   */
  def filterNot(p: A => Boolean): Repr = filterImpl(p, isFlipped = true)

  def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    foreach(pf.runWith(b += _))
    b.result
  }

  /** Builds a new collection by applying an option-valued function to all
   *  elements of this $coll on which the function is defined.
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
   *    @inheritdoc
   *
   *    @param pf     the partial function which filters and maps the $coll.
   *    @return       a new $coll resulting from applying the given option-valued function
   *                  `f` to each element and collecting all defined results.
   *                  The order of the elements is preserved.
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

  def groupBy[K](f: A => K): immutable.Map[K, Repr] = {
    val m = mutable.Map.empty[K, Builder[A, Repr]]
    for (elem <- this) {
      val key = f(elem)
      val bldr = m.getOrElseUpdate(key, newBuilder)
      bldr += elem
    }
    val b = immutable.Map.newBuilder[K, Repr]
    for ((k, v) <- m)
      b += ((k, v.result))

    b.result
  }

  def forall(p: A => Boolean): Boolean = {
    var result = true
    breakable {
      for (x <- this)
        if (!p(x)) { result = false; break }
    }
    result
  }

  /** Tests whether a predicate holds for at least one element of this $coll.
   *
   *  $mayNotTerminateInf
   *
   *  @param   p     the predicate used to test elements.
   *  @return        `false` if this $coll is empty, otherwise `true` if the given predicate `p`
    *                holds for some of the elements of this $coll, otherwise `false`
   */
  def exists(p: A => Boolean): Boolean = {
    var result = false
    breakable {
      for (x <- this)
        if (p(x)) { result = true; break }
    }
    result
  }

  def find(p: A => Boolean): Option[A] = {
    var result: Option[A] = None
    breakable {
      for (x <- this)
        if (p(x)) { result = Some(x); break }
    }
    result
  }

  def scan[B >: A, That](z: B)(op: (B, B) => B)(implicit cbf: CanBuildFrom[Repr, B, That]): That = scanLeft(z)(op)

  def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    val b = bf(repr)
    b.sizeHint(this, 1)
    var acc = z
    b += acc
    for (x <- this) { acc = op(acc, x); b += acc }
    b.result
  }

  @migration("The behavior of `scanRight` has changed. The previous behavior can be reproduced with scanRight.reverse.", "2.9.0")
  def scanRight[B, That](z: B)(op: (A, B) => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
    var scanned = List(z)
    var acc = z
    for (x <- reversed) {
      acc = op(x, acc)
      scanned ::= acc
    }
    val b = bf(repr)
    for (elem <- scanned) b += elem
    b.result
  }

  /** Selects the first element of this $coll.
   *  $orderDependent
   *  @return  the first element of this $coll.
   *  @throws NoSuchElementException if the $coll is empty.
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
   *  @return  the first element of this $coll if it is nonempty,
   *           `None` if it is empty.
   */
  def headOption: Option[A] = if (isEmpty) None else Some(head)

  /** Selects all elements except the first.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the first one.
   *  @throws java.lang.UnsupportedOperationException if the $coll is empty.
   */
  override def tail: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    drop(1)
  }

  /** Selects the last element.
    * $orderDependent
    * @return The last element of this $coll.
    * @throws NoSuchElementException If the $coll is empty.
    */
  def last: A = {
    var lst = head
    for (x <- this)
      lst = x
    lst
  }

  /** Optionally selects the last element.
   *  $orderDependent
   *  @return  the last element of this $coll$ if it is nonempty,
   *           `None` if it is empty.
   */
  def lastOption: Option[A] = if (isEmpty) None else Some(last)

  /** Selects all elements except the last.
   *  $orderDependent
   *  @return  a $coll consisting of all elements of this $coll
   *           except the last one.
   *  @throws UnsupportedOperationException if the $coll is empty.
   */
  def init: Repr = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    var lst = head
    var follow = false
    val b = newBuilder
    b.sizeHint(this, -1)
    for (x <- this) {
      if (follow) b += lst
      else follow = true
      lst = x
    }
    b.result
  }

  def take(n: Int): Repr = slice(0, n)

  def drop(n: Int): Repr =
    if (n <= 0) {
      val b = newBuilder
      b.sizeHint(this)
      (b ++= thisCollection).result
    }
    else sliceWithKnownDelta(n, Int.MaxValue, -n)

  def slice(from: Int, until: Int): Repr =
    sliceWithKnownBound(scala.math.max(from, 0), until)

  // Precondition: from >= 0, until > 0, builder already configured for building.
  private[this] def sliceInternal(from: Int, until: Int, b: Builder[A, Repr]): Repr = {
    var i = 0
    breakable {
      for (x <- this) {
        if (i >= from) b += x
        i += 1
        if (i >= until) break
      }
    }
    b.result
  }
  // Precondition: from >= 0
  private[scala] def sliceWithKnownDelta(from: Int, until: Int, delta: Int): Repr = {
    val b = newBuilder
    if (until <= from) b.result
    else {
      b.sizeHint(this, delta)
      sliceInternal(from, until, b)
    }
  }
  // Precondition: from >= 0
  private[scala] def sliceWithKnownBound(from: Int, until: Int): Repr = {
    val b = newBuilder
    if (until <= from) b.result
    else {
      b.sizeHintBounded(until - from, this)
      sliceInternal(from, until, b)
    }
  }

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

  def dropWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    var go = false
    for (x <- this) {
      if (!go && !p(x)) go = true
      if (go) b += x
    }
    b.result
  }

  def span(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    var toLeft = true
    for (x <- this) {
      toLeft = toLeft && p(x)
      (if (toLeft) l else r) += x
    }
    (l.result, r.result)
  }

  def splitAt(n: Int): (Repr, Repr) = {
    val l, r = newBuilder
    l.sizeHintBounded(n, this)
    if (n >= 0) r.sizeHint(this, -n)
    var i = 0
    for (x <- this) {
      (if (i < n) l else r) += x
      i += 1
    }
    (l.result, r.result)
  }

  /** Iterates over the tails of this $coll. The first value will be this
   *  $coll and the final one will be an empty $coll, with the intervening
   *  values the results of successive applications of `tail`.
   *
   *  @return   an iterator over all the tails of this $coll
   *  @example  `List(1,2,3).tails = Iterator(List(1,2,3), List(2,3), List(3), Nil)`
   */
  def tails: Iterator[Repr] = iterateUntilEmpty(_.tail)

  /** Iterates over the inits of this $coll. The first value will be this
   *  $coll and the final one will be an empty $coll, with the intervening
   *  values the results of successive applications of `init`.
   *
   *  @return  an iterator over all the inits of this $coll
   *  @example  `List(1,2,3).inits = Iterator(List(1,2,3), List(1,2), List(1), Nil)`
   */
  def inits: Iterator[Repr] = iterateUntilEmpty(_.init)

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

  @deprecatedOverriding("Enforce contract of toTraversable that if it is Traversable it returns itself.", "2.11.0")
  def toTraversable: Traversable[A] = thisCollection

  def toIterator: Iterator[A] = toStream.iterator
  def toStream: Stream[A] = toBuffer.toStream
  // Override to provide size hint.
  override def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, A, Col[A @uV]]): Col[A @uV] = {
    val b = cbf()
    b.sizeHint(this)
    b ++= thisCollection
    b.result
  }

  /** Converts this $coll to a string.
   *
   *  @return   a string representation of this collection. By default this
   *            string consists of the `stringPrefix` of this $coll, followed
   *            by all elements separated by commas and enclosed in parentheses.
   */
  override def toString = mkString(stringPrefix + "(", ", ", ")")

  /** Defines the prefix of this object's `toString` representation.
   *
   *  @return  a string representation which starts the result of `toString`
   *           applied to this $coll. By default the string prefix is the
   *           simple name of the collection class $coll.
   */
  def stringPrefix: String = {
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
    val fqn = repr.getClass.getName
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

  /** Creates a non-strict view of this $coll.
   *
   *  @return a non-strict view of this $coll.
   */
  def view = new TraversableView[A, Repr] {
    protected lazy val underlying = self.repr
    override def foreach[U](f: A => U) = self foreach f
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
  def withFilter(p: A => Boolean): FilterMonadic[A, Repr] = new WithFilter(p)

  /** A class supporting filtered operations. Instances of this class are
   *  returned by method `withFilter`.
   */
  class WithFilter(p: A => Boolean) extends FilterMonadic[A, Repr] {

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy predicate `p`.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying
     *                the given function `f` to each element of the outer $coll
     *                that satisfies predicate `p` and collecting the results.
     *
     *  @usecase def map[B](f: A => B): $Coll[B]
     *    @inheritdoc
     *
     *    @return       a new $coll resulting from applying the given function
     *                  `f` to each element of the outer $coll that satisfies
     *                  predicate `p` and collecting the results.
     */
    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b += f(x)
      b.result
    }

    /** Builds a new collection by applying a function to all elements of the
     *  outer $coll containing this `WithFilter` instance that satisfy
     *  predicate `p` and concatenating the results.
     *
     *  @param f      the function to apply to each element.
     *  @tparam B     the element type of the returned collection.
     *  @tparam That  $thatinfo
     *  @param bf     $bfinfo
     *  @return       a new collection of type `That` resulting from applying
     *                the given collection-valued function `f` to each element
     *                of the outer $coll that satisfies predicate `p` and
     *                concatenating the results.
     *
     *  @usecase def flatMap[B](f: A => TraversableOnce[B]): $Coll[B]
     *    @inheritdoc
     *
     *    The type of the resulting collection will be guided by the static type
     *    of the outer $coll.
     *
     *    @return       a new $coll resulting from applying the given
     *                  collection-valued function `f` to each element of the
     *                  outer $coll that satisfies predicate `p` and concatenating
     *                  the results.
     */
    def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, B, That]): That = {
      val b = bf(repr)
      for (x <- self)
        if (p(x)) b ++= f(x).seq
      b.result
    }

    /** Applies a function `f` to all elements of the outer $coll containing
     *  this `WithFilter` instance that satisfy predicate `p`.
     *
     *  @param  f   the function that is applied for its side-effect to every element.
     *              The result of function `f` is discarded.
     *
     *  @tparam  U  the type parameter describing the result of function `f`.
     *              This result will always be ignored. Typically `U` is `Unit`,
     *              but this is not necessary.
     *
     *  @usecase def foreach(f: A => Unit): Unit
     *    @inheritdoc
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
     *             satisfy the predicate `q` in addition to the predicate `p`.
     */
    def withFilter(q: A => Boolean): WithFilter =
      new WithFilter(x => p(x) && q(x))
  }

  // A helper for tails and inits.
  private def iterateUntilEmpty(f: Traversable[A @uV] => Traversable[A @uV]): Iterator[Repr] = {
    val it = Iterator.iterate(thisCollection)(f) takeWhile (x => !x.isEmpty)
    it ++ Iterator(Nil) map (x => (newBuilder ++= x).result)
  }
}
