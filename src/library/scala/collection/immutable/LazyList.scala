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
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.collection.generic.SerializeEnd
import scala.collection.mutable.{ArrayBuffer, Builder, ReusableBuilder, StringBuilder}
import scala.language.implicitConversions
import scala.runtime.Statics

/**  This class implements an immutable linked list. We call it "lazy"
  *  because it computes its elements only when they are needed.
  *
  *  Elements are memoized; that is, the value of each element is computed at most once.
  *
  *  Elements are computed in-order and are never skipped. In other words,
  *  accessing the tail causes the head to be computed first.
  *
  *  How lazy is a `LazyList`? When you have a value of type `LazyList`, you
  *  don't know yet whether the list is empty or not. If you learn that it is non-empty,
  *  then you also know that the head has been computed. But the tail is itself
  *  a `LazyList`, whose emptiness-or-not might remain undetermined.
  *
  *  A `LazyList` may be infinite. For example, `LazyList.from(0)` contains
  *  all of the natural numbers 0, 1, 2, and so on. For infinite sequences,
  *  some methods (such as `count`, `sum`, `max` or `min`) will not terminate.
  *
  *  Here is an example:
  *
  *  {{{
  *  import scala.math.BigInt
  *  object Main extends App {
  *    val fibs: LazyList[BigInt] =
  *      BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map{ n => n._1 + n._2 }
  *    fibs.take(5).foreach(println)
  *  }
  *
  *  // prints
  *  //
  *  // 0
  *  // 1
  *  // 1
  *  // 2
  *  // 3
  *  }}}
  *
  *  To illustrate, let's add some output to the definition `fibs`, so we
  *  see what's going on.
  *
  *  {{{
  *  import scala.math.BigInt
  *  object Main extends App {
  *    val fibs: LazyList[BigInt] =
  *      BigInt(0) #:: BigInt(1) #::
  *        fibs.zip(fibs.tail).map{ n =>
  *          println(s"Adding \${n._1} and \${n._2}")
  *          n._1 + n._2
  *        }
  *    fibs.take(5).foreach(println)
  *    fibs.take(6).foreach(println)
  *  }
  *
  *  // prints
  *  //
  *  // 0
  *  // 1
  *  // Adding 0 and 1
  *  // 1
  *  // Adding 1 and 1
  *  // 2
  *  // Adding 1 and 2
  *  // 3
  *
  *  // And then prints
  *  //
  *  // 0
  *  // 1
  *  // 1
  *  // 2
  *  // 3
  *  // Adding 2 and 3
  *  // 5
  *  }}}
  *
  *  Note that the definition of `fibs` uses `val` not `def`.  The memoization of the
  *  `LazyList` requires us to have somewhere to store the information and a `val`
  *  allows us to do that.
  *
  *  Further remarks about the semantics of `LazyList`:
  *
  *  - Though the `LazyList` changes as it is accessed, this does not
  *  contradict its immutability.  Once the values are memoized they do
  *  not change. Values that have yet to be memoized still "exist", they
  *  simply haven't been computed yet.
  *
  *  - One must be cautious of memoization; it can eat up memory if you're not
  *  careful.  That's because memoization of the `LazyList` creates a structure much like
  *  [[scala.collection.immutable.List]].  As long as something is holding on to
  *  the head, the head holds on to the tail, and so on recursively.
  *  If, on the other hand, there is nothing holding on to the head (e.g. if we used
  *  `def` to define the `LazyList`) then once it is no longer being used directly,
  *  it disappears.
  *
  *  - Note that some operations, including [[drop]], [[dropWhile]],
  *  [[flatMap]] or [[collect]] may process a large number of intermediate
  *  elements before returning.
  *
  *  Here's another example.  Let's start with the natural numbers and iterate
  *  over them.
  *
  *  {{{
  *  // We'll start with a silly iteration
  *  def loop(s: String, i: Int, iter: Iterator[Int]): Unit = {
  *    // Stop after 200,000
  *    if (i < 200001) {
  *      if (i % 50000 == 0) println(s + i)
  *      loop(s, iter.next(), iter)
  *    }
  *  }
  *
  *  // Our first LazyList definition will be a val definition
  *  val lazylist1: LazyList[Int] = {
  *    def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)
  *    loop(0)
  *  }
  *
  *  // Because lazylist1 is a val, everything that the iterator produces is held
  *  // by virtue of the fact that the head of the LazyList is held in lazylist1
  *  val it1 = lazylist1.iterator
  *  loop("Iterator1: ", it1.next(), it1)
  *
  *  // We can redefine this LazyList such that all we have is the Iterator left
  *  // and allow the LazyList to be garbage collected as required.  Using a def
  *  // to provide the LazyList ensures that no val is holding onto the head as
  *  // is the case with lazylist1
  *  def lazylist2: LazyList[Int] = {
  *    def loop(v: Int): LazyList[Int] = v #:: loop(v + 1)
  *    loop(0)
  *  }
  *  val it2 = lazylist2.iterator
  *  loop("Iterator2: ", it2.next(), it2)
  *
  *  // And, of course, we don't actually need a LazyList at all for such a simple
  *  // problem.  There's no reason to use a LazyList if you don't actually need
  *  // one.
  *  val it3 = new Iterator[Int] {
  *    var i = -1
  *    def hasNext = true
  *    def next(): Int = { i += 1; i }
  *  }
  *  loop("Iterator3: ", it3.next(), it3)
  *  }}}
  *
  *  - In the `fibs` example earlier, the fact that `tail` works at all is of interest.
  *  `fibs` has an initial `(0, 1, LazyList(...))`, so `tail` is deterministic.
  *  If we defined `fibs` such that only `0` were concretely known, then the act
  *  of determining `tail` would require the evaluation of `tail`, so the
  *  computation would be unable to progress, as in this code:
  *  {{{
  *  // The first time we try to access the tail we're going to need more
  *  // information which will require us to recurse, which will require us to
  *  // recurse, which...
  *  lazy val sov: LazyList[Vector[Int]] = Vector(0) #:: sov.zip(sov.tail).map { n => n._1 ++ n._2 }
  *  }}}
  *
  *  The definition of `fibs` above creates a larger number of objects than
  *  necessary depending on how you might want to implement it.  The following
  *  implementation provides a more "cost effective" implementation due to the
  *  fact that it has a more direct route to the numbers themselves:
  *
  *  {{{
  *  lazy val fib: LazyList[Int] = {
  *    def loop(h: Int, n: Int): LazyList[Int] = h #:: loop(n, h + n)
  *    loop(1, 1)
  *  }
  *  }}}
  *
  *  The head, the tail and whether the list is empty or not can be initially unknown.
  *  Once any of those are evaluated, they are all known, though if the tail is
  *  built with `#::` or `#:::`, it's content still isn't evaluated. Instead, evaluating
  *  the tails content is deferred until the tails empty status, head or tail is
  *  evaluated.
  *
  *  Delaying the evaluation of whether a LazyList is empty or not until it's needed
  *  allows LazyList to not eagerly evaluate any elements on a call to `filter`.
  *
  *  Only when it's further evaluated (which may be never!) any of the elements gets
  *  forced.
  *
  *  for example:
  *
  *  {{{
  *  def tailWithSideEffect: LazyList[Nothing] = {
  *    println("getting empty LazyList")
  *    LazyList.empty
  *  }
  *
  *  val emptyTail = tailWithSideEffect // prints "getting empty LazyList"
  *
  *  val suspended = 1 #:: tailWithSideEffect // doesn't print anything
  *  val tail = suspended.tail // although the tail is evaluated, *still* nothing is yet printed
  *  val filtered = tail.filter(_ => false) // still nothing is printed
  *  filtered.isEmpty // prints "getting empty LazyList"
  *  }}}
  *
  *  @tparam A    the type of the elements contained in this lazy list.
  *
  *  @see [[https://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lazylists "Scala's Collection Library overview"]]
  *  section on `LazyLists` for more information.
  *  @define Coll `LazyList`
  *  @define coll lazy list
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define appendStackSafety Note: Repeated chaining of calls to append methods (`appended`,
  *                            `appendedAll`, `lazyAppendedAll`) without forcing any of the
  *                            intermediate resulting lazy lists may overflow the stack when
  *                            the final result is forced.
  *  @define preservesLaziness This method preserves laziness; elements are only evaluated
  *                            individually as needed.
  *  @define initiallyLazy This method does not evaluate anything until an operation is performed
  *                        on the result (e.g. calling `head` or `tail`, or checking if it is empty).
  *  @define evaluatesAllElements This method evaluates all elements of the collection.
  */
@SerialVersionUID(3L)
final class LazyList[+A] private(private[this] var lazyState: () => LazyList.State[A])
  extends AbstractSeq[A]
    with LinearSeq[A]
    with LinearSeqOps[A, LazyList, LazyList[A]]
    with IterableFactoryDefaults[A, LazyList]
    with Serializable {
  import LazyList._

  @volatile private[this] var stateEvaluated: Boolean = false
  @inline private def stateDefined: Boolean = stateEvaluated
  private[this] var midEvaluation = false

  private lazy val state: State[A] = {
    // if it's already mid-evaluation, we're stuck in an infinite
    // self-referential loop (also it's empty)
    if (midEvaluation) {
      throw new RuntimeException("self-referential LazyList or a derivation thereof has no more elements")
    }
    midEvaluation = true
    val res = try lazyState() finally midEvaluation = false
    // if we set it to `true` before evaluating, we may infinite loop
    // if something expects `state` to already be evaluated
    stateEvaluated = true
    lazyState = null // allow GC
    res
  }

  override def iterableFactory: SeqFactory[LazyList] = LazyList

  override def isEmpty: Boolean = state eq State.Empty

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def knownSize: Int = if (knownIsEmpty) 0 else -1

  override def head: A = state.head

  override def tail: LazyList[A] = state.tail

  @inline private[this] def knownIsEmpty: Boolean = stateEvaluated && (isEmpty: @inline)
  @inline private def knownNonEmpty: Boolean = stateEvaluated && !(isEmpty: @inline)

  /** Evaluates all undefined elements of the lazy list.
    *
    * This method detects cycles in lazy lists, and terminates after all
    * elements of the cycle are evaluated. For example:
    *
    * {{{
    * val ring: LazyList[Int] = 1 #:: 2 #:: 3 #:: ring
    * ring.force
    * ring.toString
    *
    * // prints
    * //
    * // LazyList(1, 2, 3, ...)
    * }}}
    *
    * This method will *not* terminate for non-cyclic infinite-sized collections.
    *
    * @return this
    */
  def force: this.type = {
    // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
    var these, those: LazyList[A] = this
    if (!these.isEmpty) {
      these = these.tail
    }
    while (those ne these) {
      if (these.isEmpty) return this
      these = these.tail
      if (these.isEmpty) return this
      these = these.tail
      if (these eq those) return this
      those = those.tail
    }
    this
  }

  /** @inheritdoc
    *
    * The iterator returned by this method preserves laziness; elements are
    * only evaluated individually as needed.
    */
  override def iterator: Iterator[A] =
    if (knownIsEmpty) Iterator.empty
    else new LazyIterator(this)

  /** Apply the given function `f` to each element of this linear sequence
    * (while respecting the order of the elements).
    *
    *  @param f The treatment to apply to each element.
    *  @note  Overridden here as final to trigger tail-call optimization, which
    *  replaces 'this' with 'tail' at each iteration. This is absolutely
    *  necessary for allowing the GC to collect the underlying LazyList as elements
    *  are consumed.
    *  @note  This function will force the realization of the entire LazyList
    *  unless the `f` throws an exception.
    */
  @tailrec
  override def foreach[U](f: A => U): Unit = {
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  /** LazyList specialization of foldLeft which allows GC to collect along the
    * way.
    *
    * @tparam B The type of value being accumulated.
    * @param z The initial value seeded into the function `op`.
    * @param op The operation to perform on successive elements of the `LazyList`.
    * @return The accumulated value from successive applications of `op`.
    */
  @tailrec
  override def foldLeft[B](z: B)(op: (B, A) => B): B =
    if (isEmpty) z
    else tail.foldLeft(op(z, head))(op)

  // State.Empty doesn't use the SerializationProxy
  protected[this] def writeReplace(): AnyRef =
    if (knownNonEmpty) new LazyList.SerializationProxy[A](this) else this

  override protected[this] def className = "LazyList"

  /** The lazy list resulting from the concatenation of this lazy list with the argument lazy list.
    *
    * $preservesLaziness
    *
    * $appendStackSafety
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    */
  def lazyAppendedAll[B >: A](suffix: => collection.IterableOnce[B]): LazyList[B] =
    newLL {
      if (isEmpty) suffix match {
        case lazyList: LazyList[B]       => lazyList.state // don't recompute the LazyList
        case coll if coll.knownSize == 0 => State.Empty
        case coll                        => stateFromIterator(coll.iterator)
      }
      else sCons(head, tail lazyAppendedAll suffix)
    }

  /** @inheritdoc
    *
    * $preservesLaziness
    *
    * $appendStackSafety
    */
  override def appendedAll[B >: A](suffix: IterableOnce[B]): LazyList[B] =
    if (knownIsEmpty) LazyList.from(suffix)
    else lazyAppendedAll(suffix)

  /** @inheritdoc
    *
    * $preservesLaziness
    *
    * $appendStackSafety
    */
  override def appended[B >: A](elem: B): LazyList[B] =
    if (knownIsEmpty) newLL(sCons(elem, LazyList.empty))
    else lazyAppendedAll(Iterator.single(elem))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def scanLeft[B](z: B)(op: (B, A) => B): LazyList[B] =
    if (knownIsEmpty) newLL(sCons(z, LazyList.empty))
    else newLL(scanLeftState(z)(op))

  private def scanLeftState[B](z: B)(op: (B, A) => B): State[B] =
    sCons(
      z,
      newLL {
        if (isEmpty) State.Empty
        else tail.scanLeftState(op(z, head))(op)
      }
    )

  /** LazyList specialization of reduceLeft which allows GC to collect
    *  along the way.
    *
    * @tparam B The type of value being accumulated.
    * @param f The operation to perform on successive elements of the `LazyList`.
    * @return The accumulated value from successive applications of `f`.
    */
  override def reduceLeft[B >: A](f: (B, A) => B): B = {
    if (this.isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else {
      var reducedRes: B = this.head
      var left: LazyList[A] = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def partition(p: A => Boolean): (LazyList[A], LazyList[A]) = (filter(p), filterNot(p))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def partitionMap[A1, A2](f: A => Either[A1, A2]): (LazyList[A1], LazyList[A2]) = {
    val (left, right) = map(f).partition(_.isLeft)
    (left.map(_.asInstanceOf[Left[A1, _]].value), right.map(_.asInstanceOf[Right[_, A2]].value))
  }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def filter(pred: A => Boolean): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else LazyList.filterImpl(this, pred, isFlipped = false)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def filterNot(pred: A => Boolean): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else LazyList.filterImpl(this, pred, isFlipped = true)

  /** A `collection.WithFilter` which allows GC of the head of lazy list during processing.
    *
    * This method is not particularly useful for a lazy list, as [[filter]] already preserves
    * laziness.
    *
    * The `collection.WithFilter` returned by this method preserves laziness; elements are
    * only evaluated individually as needed.
    */
  override def withFilter(p: A => Boolean): collection.WithFilter[A, LazyList] =
    new LazyList.WithFilter(coll, p)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def prepended[B >: A](elem: B): LazyList[B] = newLL(sCons(elem, this))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def prependedAll[B >: A](prefix: collection.IterableOnce[B]): LazyList[B] =
    if (knownIsEmpty) LazyList.from(prefix)
    else if (prefix.knownSize == 0) this
    else newLL(stateFromIteratorConcatSuffix(prefix.iterator)(state))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def map[B](f: A => B): LazyList[B] =
    if (knownIsEmpty) LazyList.empty
    else (mapImpl(f): @inline)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def tapEach[U](f: A => U): LazyList[A] = map { a => f(a); a }

  private def mapImpl[B](f: A => B): LazyList[B] =
    newLL {
      if (isEmpty) State.Empty
      else sCons(f(head), tail.mapImpl(f))
    }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def collect[B](pf: PartialFunction[A, B]): LazyList[B] =
    if (knownIsEmpty) LazyList.empty
    else LazyList.collectImpl(this, pf)

  /** @inheritdoc
    *
    * This method does not evaluate any elements further than
    * the first element for which the partial function is defined.
    */
  @tailrec
  override def collectFirst[B](pf: PartialFunction[A, B]): Option[B] =
    if (isEmpty) None
    else {
      val res = pf.applyOrElse(head, LazyList.anyToMarker.asInstanceOf[A => B])
      if (res.asInstanceOf[AnyRef] eq Statics.pfMarker) tail.collectFirst(pf)
      else Some(res)
    }

  /** @inheritdoc
    *
    * This method does not evaluate any elements further than
    * the first element matching the predicate.
    */
  @tailrec
  override def find(p: A => Boolean): Option[A] =
    if (isEmpty) None
    else {
      val elem = head
      if (p(elem)) Some(elem)
      else tail.find(p)
    }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  // optimisations are not for speed, but for functionality
  // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
  override def flatMap[B](f: A => IterableOnce[B]): LazyList[B] =
    if (knownIsEmpty) LazyList.empty
    else LazyList.flatMapImpl(this, f)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def flatten[B](implicit asIterable: A => IterableOnce[B]): LazyList[B] = flatMap(asIterable)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def zip[B](that: collection.IterableOnce[B]): LazyList[(A, B)] =
    if (this.knownIsEmpty || that.knownSize == 0) LazyList.empty
    else newLL(zipState(that.iterator))

  private def zipState[B](it: Iterator[B]): State[(A, B)] =
    if (this.isEmpty || !it.hasNext) State.Empty
    else sCons((head, it.next()), newLL { tail zipState it })

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def zipWithIndex: LazyList[(A, Int)] = this zip LazyList.from(0)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def zipAll[A1 >: A, B](that: collection.Iterable[B], thisElem: A1, thatElem: B): LazyList[(A1, B)] = {
    if (this.knownIsEmpty) {
      if (that.knownSize == 0) LazyList.empty
      else LazyList.continually(thisElem) zip that
    } else {
      if (that.knownSize == 0) zip(LazyList.continually(thatElem))
      else newLL(zipAllState(that.iterator, thisElem, thatElem))
    }
  }

  private def zipAllState[A1 >: A, B](it: Iterator[B], thisElem: A1, thatElem: B): State[(A1, B)] = {
    if (it.hasNext) {
      if (this.isEmpty) sCons((thisElem, it.next()), newLL { LazyList.continually(thisElem) zipState it })
      else sCons((this.head, it.next()), newLL { this.tail.zipAllState(it, thisElem, thatElem) })
    } else {
      if (this.isEmpty) State.Empty
      else sCons((this.head, thatElem), this.tail zip LazyList.continually(thatElem))
    }
  }

  /** @inheritdoc
    *
    * This method is not particularly useful for a lazy list, as [[zip]] already preserves
    * laziness.
    *
    * The `collection.LazyZip2` returned by this method preserves laziness; elements are
    * only evaluated individually as needed.
    */
  // just in case it can be meaningfully overridden at some point
  override def lazyZip[B](that: collection.Iterable[B]): LazyZip2[A, B, LazyList.this.type] =
    super.lazyZip(that)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def unzip[A1, A2](implicit asPair: A => (A1, A2)): (LazyList[A1], LazyList[A2]) =
    (map(asPair(_)._1), map(asPair(_)._2))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def unzip3[A1, A2, A3](implicit asTriple: A => (A1, A2, A3)): (LazyList[A1], LazyList[A2], LazyList[A3]) =
    (map(asTriple(_)._1), map(asTriple(_)._2), map(asTriple(_)._3))

  /** @inheritdoc
    *
    * $initiallyLazy
    * Additionally, it preserves laziness for all except the first `n` elements.
    */
  override def drop(n: Int): LazyList[A] =
    if (n <= 0) this
    else if (knownIsEmpty) LazyList.empty
    else LazyList.dropImpl(this, n)

  /** @inheritdoc
    *
    * $initiallyLazy
    * Additionally, it preserves laziness for all elements after the predicate returns `false`.
    */
  override def dropWhile(p: A => Boolean): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else LazyList.dropWhileImpl(this, p)

  /** @inheritdoc
    *
    * $initiallyLazy
    */
  override def dropRight(n: Int): LazyList[A] = {
    if (n <= 0) this
    else if (knownIsEmpty) LazyList.empty
    else newLL {
      var scout = this
      var remaining = n
      // advance scout n elements ahead (or until empty)
      while (remaining > 0 && !scout.isEmpty) {
        remaining -= 1
        scout = scout.tail
      }
      dropRightState(scout)
    }
  }

  private def dropRightState(scout: LazyList[_]): State[A] =
    if (scout.isEmpty) State.Empty
    else sCons(head, newLL(tail.dropRightState(scout.tail)))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def take(n: Int): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else (takeImpl(n): @inline)

  private def takeImpl(n: Int): LazyList[A] = {
    if (n <= 0) LazyList.empty
    else newLL {
      if (isEmpty) State.Empty
      else sCons(head, tail.takeImpl(n - 1))
    }
  }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def takeWhile(p: A => Boolean): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else (takeWhileImpl(p): @inline)

  private def takeWhileImpl(p: A => Boolean): LazyList[A] =
    newLL {
      if (isEmpty || !p(head)) State.Empty
      else sCons(head, tail.takeWhileImpl(p))
    }

  /** @inheritdoc
    *
    * $initiallyLazy
    */
  override def takeRight(n: Int): LazyList[A] =
    if (n <= 0 || knownIsEmpty) LazyList.empty
    else LazyList.takeRightImpl(this, n)

  /** @inheritdoc
    *
    * $initiallyLazy
    * Additionally, it preserves laziness for all but the first `from` elements.
    */
  override def slice(from: Int, until: Int): LazyList[A] = take(until).drop(from)

  /** @inheritdoc
    *
    * $evaluatesAllElements
    */
  override def reverse: LazyList[A] = reverseOnto(LazyList.empty)

  // need contravariant type B to make the compiler happy - still returns LazyList[A]
  @tailrec
  private def reverseOnto[B >: A](tl: LazyList[B]): LazyList[B] =
    if (isEmpty) tl
    else tail.reverseOnto(newLL(sCons(head, tl)))

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def diff[B >: A](that: collection.Seq[B]): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else super.diff(that)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def intersect[B >: A](that: collection.Seq[B]): LazyList[A] =
    if (knownIsEmpty) LazyList.empty
    else super.intersect(that)

  @tailrec
  private def lengthGt(len: Int): Boolean =
    if (len < 0) true
    else if (isEmpty) false
    else tail.lengthGt(len - 1)

  /** @inheritdoc
    *
    * The iterator returned by this method mostly preserves laziness;
    * a single element ahead of the iterator is evaluated.
    */
  override def grouped(size: Int): Iterator[LazyList[A]] = {
    require(size > 0, "size must be positive, but was " + size)
    slidingImpl(size = size, step = size)
  }

  /** @inheritdoc
    *
    * The iterator returned by this method mostly preserves laziness;
    * `size - step max 1` elements ahead of the iterator are evaluated.
    */
  override def sliding(size: Int, step: Int): Iterator[LazyList[A]] = {
    require(size > 0 && step > 0, s"size=$size and step=$step, but both must be positive")
    slidingImpl(size = size, step = step)
  }

  @inline private def slidingImpl(size: Int, step: Int): Iterator[LazyList[A]] =
    if (knownIsEmpty) Iterator.empty
    else new SlidingIterator[A](this, size = size, step = step)

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def padTo[B >: A](len: Int, elem: B): LazyList[B] = {
    if (len <= 0) this
    else newLL {
      if (isEmpty) LazyList.fill(len)(elem).state
      else sCons(head, tail.padTo(len - 1, elem))
    }
  }

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def patch[B >: A](from: Int, other: IterableOnce[B], replaced: Int): LazyList[B] =
    if (knownIsEmpty) LazyList from other
    else patchImpl(from, other, replaced)

  private def patchImpl[B >: A](from: Int, other: IterableOnce[B], replaced: Int): LazyList[B] =
    newLL {
      if (from <= 0) stateFromIteratorConcatSuffix(other.iterator)(LazyList.dropImpl(this, replaced).state)
      else if (isEmpty) stateFromIterator(other.iterator)
      else sCons(head, tail.patchImpl(from - 1, other, replaced))
    }

  /** @inheritdoc
    *
    * $evaluatesAllElements
    */
  // overridden just in case a lazy implementation is developed at some point
  override def transpose[B](implicit asIterable: A => collection.Iterable[B]): LazyList[LazyList[B]] = super.transpose

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  override def updated[B >: A](index: Int, elem: B): LazyList[B] =
    if (index < 0) throw new IndexOutOfBoundsException(s"$index")
    else updatedImpl(index, elem, index)

  private def updatedImpl[B >: A](index: Int, elem: B, startIndex: Int): LazyList[B] = {
    newLL {
      if (index <= 0) sCons(elem, tail)
      else if (tail.isEmpty) throw new IndexOutOfBoundsException(startIndex.toString)
      else sCons(head, tail.updatedImpl(index - 1, elem, startIndex))
    }
  }

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
    *  The written text begins with the string `start` and ends with the string `end`.
    *  Inside, the string representations (w.r.t. the method `toString`)
    *  of all elements of this $coll are separated by the string `sep`.
    *
    * An undefined state is represented with `"&lt;not computed&gt;"` and cycles are represented with `"&lt;cycle&gt;"`.
    *
    * $evaluatesAllElements
    *
    *  @param sb    the string builder to which elements are appended.
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      the string builder `b` to which elements were appended.
    */
  override def addString(sb: StringBuilder, start: String, sep: String, end: String): sb.type = {
    force
    addStringNoForce(sb.underlying, start, sep, end)
    sb
  }

  private[this] def addStringNoForce(b: JStringBuilder, start: String, sep: String, end: String): JStringBuilder = {
    b.append(start)
    if (!stateDefined) b.append("<not computed>")
    else if (!isEmpty) {
      b.append(head)
      var cursor = this
      @inline def appendCursorElement(): Unit = b.append(sep).append(cursor.head)
      var scout = tail
      @inline def scoutNonEmpty: Boolean = scout.stateDefined && !scout.isEmpty
      if ((cursor ne scout) && (!scout.stateDefined || (cursor.state ne scout.state))) {
        cursor = scout
        if (scoutNonEmpty) {
          scout = scout.tail
          // Use 2x 1x iterator trick for cycle detection; slow iterator can add strings
          while ((cursor ne scout) && scoutNonEmpty && (cursor.state ne scout.state)) {
            appendCursorElement()
            cursor = cursor.tail
            scout = scout.tail
            if (scoutNonEmpty) scout = scout.tail
          }
        }
      }
      if (!scoutNonEmpty) {  // Not a cycle, scout hit an end
        while (cursor ne scout) {
          appendCursorElement()
          cursor = cursor.tail
        }
        // if cursor (eq scout) has state defined, it is empty; else unknown state
        if (!cursor.stateDefined) b.append(sep).append("<not computed>")
      } else {
        @inline def same(a: LazyList[A], b: LazyList[A]): Boolean = (a eq b) || (a.state eq b.state)
        // Cycle.
        // If we have a prefix of length P followed by a cycle of length C,
        // the scout will be at position (P%C) in the cycle when the cursor
        // enters it at P.  They'll then collide when the scout advances another
        // C - (P%C) ahead of the cursor.
        // If we run the scout P farther, then it will be at the start of
        // the cycle: (C - (P%C) + (P%C)) == C == 0.  So if another runner
        // starts at the beginning of the prefix, they'll collide exactly at
        // the start of the loop.
        var runner = this
        var k = 0
        while (!same(runner, scout)) {
          runner = runner.tail
          scout = scout.tail
          k += 1
        }
        // Now runner and scout are at the beginning of the cycle.  Advance
        // cursor, adding to string, until it hits; then we'll have covered
        // everything once.  If cursor is already at beginning, we'd better
        // advance one first unless runner didn't go anywhere (in which case
        // we've already looped once).
        if (same(cursor, scout) && (k > 0)) {
          appendCursorElement()
          cursor = cursor.tail
        }
        while (!same(cursor, scout)) {
          appendCursorElement()
          cursor = cursor.tail
        }
        b.append(sep).append("<cycle>")
      }
    }
    b.append(end)
  }

  /** $preservesLaziness
    *
    * @return a string representation of this collection. An undefined state is
    *         represented with `"&lt;not computed&gt;"` and cycles are represented with `"&lt;cycle&gt;"`
    *
    *         Examples:
    *
    *           - `"LazyList(4, &lt;not computed&gt;)"`, a non-empty lazy list ;
    *           - `"LazyList(1, 2, 3, &lt;not computed&gt;)"`, a lazy list with at least three elements ;
    *           - `"LazyList(1, 2, 3, &lt;cycle&gt;)"`, an infinite lazy list that contains
    *             a cycle at the fourth element.
    */
  override def toString(): String = addStringNoForce(new JStringBuilder(className), "(", ", ", ")").toString

  /** @inheritdoc
    *
    * $preservesLaziness
    */
  @deprecated("Check .knownSize instead of .hasDefiniteSize for more actionable information (see scaladoc for details)", "2.13.0")
  override def hasDefiniteSize: Boolean = {
    if (!stateDefined) false
    else if (isEmpty) true
    else {
      // Two-iterator trick (2x & 1x speed) for cycle detection.
      var those = this
      var these = tail
      while (those ne these) {
        if (!these.stateDefined) return false
        else if (these.isEmpty) return true
        these = these.tail
        if (!these.stateDefined) return false
        else if (these.isEmpty) return true
        these = these.tail
        if (those eq these) return false
        those = those.tail
      }
      false  // Cycle detected
    }
  }
}

/**
  * $factoryInfo
  * @define coll lazy list
  * @define Coll `LazyList`
  */
@SerialVersionUID(3L)
object LazyList extends SeqFactory[LazyList] {
  // Eagerly evaluate cached empty instance
  private[this] val _empty = newLL(State.Empty).force

  private sealed trait State[+A] extends Serializable {
    def head: A
    def tail: LazyList[A]
  }

  private object State {
    @SerialVersionUID(3L)
    object Empty extends State[Nothing] {
      def head: Nothing = throw new NoSuchElementException("head of empty lazy list")
      def tail: LazyList[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")
    }

    @SerialVersionUID(3L)
    final class Cons[A](val head: A, val tail: LazyList[A]) extends State[A]
  }

  /** Creates a new LazyList. */
  @inline private def newLL[A](state: => State[A]): LazyList[A] = new LazyList[A](() => state)

  /** Creates a new State.Cons. */
  @inline private def sCons[A](hd: A, tl: LazyList[A]): State[A] = new State.Cons[A](hd, tl)

  private val anyToMarker: Any => Any = _ => Statics.pfMarker

  /* All of the following `<op>Impl` methods are carefully written so as not to
   * leak the beginning of the `LazyList`. They copy the initial `LazyList` (`ll`) into
   * `var rest`, which gets closed over as a `scala.runtime.ObjectRef`, thus not permanently
   * leaking the head of the `LazyList`. Additionally, the methods are written so that, should
   * an exception be thrown by the evaluation of the `LazyList` or any supplied function, they
   * can continue their execution where they left off.
   */

  private def filterImpl[A](ll: LazyList[A], p: A => Boolean, isFlipped: Boolean): LazyList[A] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef = ll                         // val restRef = new ObjectRef(ll)
    newLL {
      var elem: A = null.asInstanceOf[A]
      var found   = false
      var rest    = restRef                  // var rest = restRef.elem
      while (!found && !rest.isEmpty) {
        elem    = rest.head
        found   = p(elem) != isFlipped
        rest    = rest.tail
        restRef = rest                       // restRef.elem = rest
      }
      if (found) sCons(elem, filterImpl(rest, p, isFlipped)) else State.Empty
    }
  }

  private def collectImpl[A, B](ll: LazyList[A], pf: PartialFunction[A, B]): LazyList[B] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef = ll                                  // val restRef = new ObjectRef(ll)
    newLL {
      val marker = Statics.pfMarker
      val toMarker = anyToMarker.asInstanceOf[A => B] // safe because Function1 is erased

      var res: B = marker.asInstanceOf[B]             // safe because B is unbounded
      var rest   = restRef                            // var rest = restRef.elem
      while((res.asInstanceOf[AnyRef] eq marker) && !rest.isEmpty) {
        res     = pf.applyOrElse(rest.head, toMarker)
        rest    = rest.tail
        restRef = rest                                // restRef.elem = rest
      }
      if (res.asInstanceOf[AnyRef] eq marker) State.Empty
      else sCons(res, collectImpl(rest, pf))
    }
  }

  private def flatMapImpl[A, B](ll: LazyList[A], f: A => IterableOnce[B]): LazyList[B] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef = ll                          // val restRef = new ObjectRef(ll)
    newLL {
      var it: Iterator[B] = null
      var itHasNext       = false
      var rest            = restRef           // var rest = restRef.elem
      while (!itHasNext && !rest.isEmpty) {
        it        = f(rest.head).iterator
        itHasNext = it.hasNext
        if (!itHasNext) {                     // wait to advance `rest` because `it.next()` can throw
          rest    = rest.tail
          restRef = rest                      // restRef.elem = rest
        }
      }
      if (itHasNext) {
        val head = it.next()
        rest     = rest.tail
        restRef  = rest                       // restRef.elem = rest
        sCons(head, newLL(stateFromIteratorConcatSuffix(it)(flatMapImpl(rest, f).state)))
      } else State.Empty
    }
  }

  private def dropImpl[A](ll: LazyList[A], n: Int): LazyList[A] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef = ll                     // val restRef = new ObjectRef(ll)
    var iRef    = n                      // val iRef    = new IntRef(n)
    newLL {
      var rest = restRef                 // var rest = restRef.elem
      var i    = iRef                    // var i    = iRef.elem
      while (i > 0 && !rest.isEmpty) {
        rest    = rest.tail
        restRef = rest                   // restRef.elem = rest
        i      -= 1
        iRef    = i                      // iRef.elem    = i
      }
      rest.state
    }
  }

  private def dropWhileImpl[A](ll: LazyList[A], p: A => Boolean): LazyList[A] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef = ll                            // val restRef = new ObjectRef(ll)
    newLL {
      var rest = restRef                        // var rest = restRef.elem
      while (!rest.isEmpty && p(rest.head)) {
        rest    = rest.tail
        restRef = rest                          // restRef.elem = rest
      }
      rest.state
    }
  }

  private def takeRightImpl[A](ll: LazyList[A], n: Int): LazyList[A] = {
    // DO NOT REFERENCE `ll` ANYWHERE ELSE, OR IT WILL LEAK THE HEAD
    var restRef      = ll                         // val restRef      = new ObjectRef(ll)
    var scoutRef     = ll                         // val scoutRef     = new ObjectRef(ll)
    var remainingRef = n                          // val remainingRef = new IntRef(n)
    newLL {
      var scout     = scoutRef                    // var scout     = scoutRef.elem
      var remaining = remainingRef                // var remaining = remainingRef.elem
      // advance `scout` `n` elements ahead (or until empty)
      while (remaining > 0 && !scout.isEmpty) {
        scout        = scout.tail
        scoutRef     = scout                      // scoutRef.elem     = scout
        remaining   -= 1
        remainingRef = remaining                  // remainingRef.elem = remaining
      }
      var rest = restRef                          // var rest = restRef.elem
      // advance `rest` and `scout` in tandem until `scout` reaches the end
      while(!scout.isEmpty) {
        scout    = scout.tail
        scoutRef = scout                          // scoutRef.elem = scout
        rest     = rest.tail                      // can't throw an exception as `scout` has already evaluated its tail
        restRef  = rest                           // restRef.elem  = rest
      }
      // `rest` is the last `n` elements (or all of them)
      rest.state
    }
  }

  /** An alternative way of building and matching lazy lists using LazyList.cons(hd, tl).
    */
  object cons {
    /** A lazy list consisting of a given first element and remaining elements
      *  @param hd   The first element of the result lazy list
      *  @param tl   The remaining elements of the result lazy list
      */
    def apply[A](hd: => A, tl: => LazyList[A]): LazyList[A] = newLL(sCons(hd, newLL(tl.state)))

    /** Maps a lazy list to its head and tail */
    def unapply[A](xs: LazyList[A]): Option[(A, LazyList[A])] = #::.unapply(xs)
  }

  implicit def toDeferrer[A](l: => LazyList[A]): Deferrer[A] = new Deferrer[A](() => l)

  final class Deferrer[A] private[LazyList] (private val l: () => LazyList[A]) extends AnyVal {
    /** Construct a LazyList consisting of a given first element followed by elements
      *  from another LazyList.
      */
    def #:: [B >: A](elem: => B): LazyList[B] = newLL(sCons(elem, newLL(l().state)))
    /** Construct a LazyList consisting of the concatenation of the given LazyList and
      *  another LazyList.
      */
    def #:::[B >: A](prefix: LazyList[B]): LazyList[B] = prefix lazyAppendedAll l()
  }

  object #:: {
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
      if (!s.isEmpty) Some((s.head, s.tail)) else None
  }

  def from[A](coll: collection.IterableOnce[A]): LazyList[A] = coll match {
    case lazyList: LazyList[A]    => lazyList
    case _ if coll.knownSize == 0 => empty[A]
    case _                        => newLL(stateFromIterator(coll.iterator))
  }

  def empty[A]: LazyList[A] = _empty

  /** Creates a State from an Iterator, with another State appended after the Iterator
    * is empty.
    */
  private def stateFromIteratorConcatSuffix[A](it: Iterator[A])(suffix: => State[A]): State[A] =
    if (it.hasNext) sCons(it.next(), newLL(stateFromIteratorConcatSuffix(it)(suffix)))
    else suffix

  /** Creates a State from an IterableOnce. */
  private def stateFromIterator[A](it: Iterator[A]): State[A] =
    if (it.hasNext) sCons(it.next(), newLL(stateFromIterator(it)))
    else State.Empty

  override def concat[A](xss: collection.Iterable[A]*): LazyList[A] =
    if (xss.knownSize == 0) empty
    else newLL(concatIterator(xss.iterator))

  private def concatIterator[A](it: Iterator[collection.Iterable[A]]): State[A] =
    if (!it.hasNext) State.Empty
    else stateFromIteratorConcatSuffix(it.next().iterator)(concatIterator(it))

  /** An infinite LazyList that repeatedly applies a given function to a start value.
    *
    *  @param start the start value of the LazyList
    *  @param f     the function that's repeatedly applied
    *  @return      the LazyList returning the infinite sequence of values `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: => A)(f: A => A): LazyList[A] =
    newLL {
      val head = start
      sCons(head, iterate(f(head))(f))
    }

  /**
    * Create an infinite LazyList starting at `start` and incrementing by
    * step `step`.
    *
    * @param start the start value of the LazyList
    * @param step the increment value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int, step: Int): LazyList[Int] =
    newLL(sCons(start, from(start + step, step)))

  /**
    * Create an infinite LazyList starting at `start` and incrementing by `1`.
    *
    * @param start the start value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int): LazyList[Int] = from(start, 1)

  /**
    * Create an infinite LazyList containing the given element expression (which
    * is computed for each occurrence).
    *
    * @param elem the element composing the resulting LazyList
    * @return the LazyList containing an infinite number of elem
    */
  def continually[A](elem: => A): LazyList[A] = newLL(sCons(elem, continually(elem)))

  override def fill[A](n: Int)(elem: => A): LazyList[A] =
    if (n > 0) newLL(sCons(elem, fill(n - 1)(elem))) else empty

  override def tabulate[A](n: Int)(f: Int => A): LazyList[A] = {
    def at(index: Int): LazyList[A] =
      if (index < n) newLL(sCons(f(index), at(index + 1))) else empty

    at(0)
  }

  // significantly simpler than the iterator returned by Iterator.unfold
  override def unfold[A, S](init: S)(f: S => Option[(A, S)]): LazyList[A] =
    newLL {
      f(init) match {
        case Some((elem, state)) => sCons(elem, unfold(state)(f))
        case None                => State.Empty
      }
    }

  /** The builder returned by this method only evaluates elements
    * of collections added to it as needed.
    *
    * @tparam A the type of the ${coll}’s elements
    * @return A builder for $Coll objects.
    */
  def newBuilder[A]: Builder[A, LazyList[A]] = new LazyBuilder[A]

  private class LazyIterator[+A](private[this] var lazyList: LazyList[A]) extends AbstractIterator[A] {
    override def hasNext: Boolean = !lazyList.isEmpty

    override def next(): A =
      if (lazyList.isEmpty) Iterator.empty.next()
      else {
        val res = lazyList.head
        lazyList = lazyList.tail
        res
      }
  }

  private class SlidingIterator[A](private[this] var lazyList: LazyList[A], size: Int, step: Int)
    extends AbstractIterator[LazyList[A]] {
    private val minLen = size - step max 0
    private var first = true

    def hasNext: Boolean =
      if (first) !lazyList.isEmpty
      else lazyList.lengthGt(minLen)

    def next(): LazyList[A] = {
      if (!hasNext) Iterator.empty.next()
      else {
        first = false
        val list = lazyList
        lazyList = list.drop(step)
        list.take(size)
      }
    }
  }

  private final class WithFilter[A] private[LazyList](lazyList: LazyList[A], p: A => Boolean)
    extends collection.WithFilter[A, LazyList] {
    private[this] val filtered = lazyList.filter(p)
    def map[B](f: A => B): LazyList[B] = filtered.map(f)
    def flatMap[B](f: A => IterableOnce[B]): LazyList[B] = filtered.flatMap(f)
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def withFilter(q: A => Boolean): collection.WithFilter[A, LazyList] = new WithFilter(filtered, q)
  }

  private final class LazyBuilder[A] extends ReusableBuilder[A, LazyList[A]] {
    import LazyBuilder._

    private[this] var next: DeferredState[A] = _
    private[this] var list: LazyList[A] = _

    clear()

    override def clear(): Unit = {
      val deferred = new DeferredState[A]
      list = newLL(deferred.eval())
      next = deferred
    }

    override def result(): LazyList[A] = {
      next init State.Empty
      list
    }

    override def addOne(elem: A): this.type = {
      val deferred = new DeferredState[A]
      next init sCons(elem, newLL(deferred.eval()))
      next = deferred
      this
    }

    // lazy implementation which doesn't evaluate the collection being added
    override def addAll(xs: IterableOnce[A]): this.type = {
      if (xs.knownSize != 0) {
        val deferred = new DeferredState[A]
        next init stateFromIteratorConcatSuffix(xs.iterator)(deferred.eval())
        next = deferred
      }
      this
    }
  }

  private object LazyBuilder {
    final class DeferredState[A] {
      private[this] var _state: () => State[A] = _

      def eval(): State[A] = {
        val state = _state
        if (state == null) throw new IllegalStateException("uninitialized")
        state()
      }

      // racy
      def init(state: => State[A]): Unit = {
        if (_state != null) throw new IllegalStateException("already initialized")
        _state = () => state
      }
    }
  }

  /** This serialization proxy is used for LazyLists which start with a sequence of evaluated cons cells.
    * The forced sequence is serialized in a compact, sequential format, followed by the unevaluated tail, which uses
    * standard Java serialization to store the complete structure of unevaluated thunks. This allows the serialization
    * of long evaluated lazy lists without exhausting the stack through recursive serialization of cons cells.
    */
  @SerialVersionUID(3L)
  final class SerializationProxy[A](@transient protected var coll: LazyList[A]) extends Serializable {

    private[this] def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      var these = coll
      while(these.knownNonEmpty) {
        out.writeObject(these.head)
        these = these.tail
      }
      out.writeObject(SerializeEnd)
      out.writeObject(these)
    }

    private[this] def readObject(in: ObjectInputStream): Unit = {
      in.defaultReadObject()
      val init = new ArrayBuffer[A]
      var initRead = false
      while (!initRead) in.readObject match {
        case SerializeEnd => initRead = true
        case a => init += a.asInstanceOf[A]
      }
      val tail = in.readObject().asInstanceOf[LazyList[A]]
      coll = init ++: tail
    }

    private[this] def readResolve(): Any = coll
  }
}
