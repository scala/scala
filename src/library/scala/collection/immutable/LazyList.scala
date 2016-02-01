/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import generic._
import mutable.{Builder, StringBuilder, LazyBuilder}
import scala.annotation.tailrec
import LazyList.cons
import scala.language.implicitConversions

/** The class `LazyList` implements lazy lists where elements
 *  are only evaluated when they are needed. Here is an example:
 *
 *  {{{
 *  import scala.math.BigInt
 *  object Main extends App {
 *
 *    val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
 *
 *    fibs take 5 foreach println
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
 *  The `LazyList` class also employs memoization such that previously computed
 *  values are converted from `LazyList` elements to concrete values of type `A`.
 *  To illustrate, we will alter body of the `fibs` value above and take some
 *  more values:
 *
 *  {{{
 *  import scala.math.BigInt
 *  object Main extends App {
 *
 *    val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(
 *      fibs.tail).map(n => {
 *        println("Adding %d and %d".format(n._1, n._2))
 *        n._1 + n._2
 *      })
 *
 *    fibs take 5 foreach println
 *    fibs take 6 foreach println
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
 *  There are a number of subtle points to the above example.
 *
 *  - The definition of `fibs` is a `val` not a method.  The memoization of the
 *  `LazyList` requires us to have somewhere to store the information and a `val`
 *  allows us to do that.
 *
 *  - While the `LazyList` is actually being modified during access, this does not
 *  change the notion of its immutability.  Once the values are memoized they do
 *  not change and values that have yet to be memoized still "exist", they
 *  simply haven't been realized yet.
 *
 *  - One must be cautious of memoization; you can very quickly eat up large
 *  amounts of memory if you're not careful.  The reason for this is that the
 *  memoization of the `LazyList` creates a structure much like
 *  [[scala.collection.immutable.List]].  So long as something is holding on to
 *  the head, the head holds on to the tail, and so it continues recursively.
 *  If, on the other hand, there is nothing holding on to the head (e.g. we used
 *  `def` to define the `LazyList`) then once it is no longer being used directly,
 *  it disappears.
 *
 *  - Note that some operations, including [[drop]], [[dropWhile]],
 *  [[flatMap]] or [[collect]] may process a large number of intermediate
 *  elements before returning.  These necessarily hold onto the head, since
 *  they are methods on `LazyList`, and a lazy list holds its own head.  For
 *  computations of this sort where memoization is not desired, use
 *  `Iterator` when possible.
 *
 *  {{{
 *  // For example, let's build the natural numbers and do some silly iteration
 *  // over them.
 *
 *  // We'll start with a silly iteration
 *  def loop(s: String, i: Int, iter: Iterator[Int]): Unit = {
 *    // Stop after 200,000
 *    if (i < 200001) {
 *      if (i % 50000 == 0) println(s + i)
 *      loop(s, iter.next, iter)
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
 *  loop("Iterator1: ", it1.next, it1)
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
 *  loop("Iterator2: ", it2.next, it2)
 *
 *  // And, of course, we don't actually need a LazyList at all for such a simple
 *  // problem.  There's no reason to use a LazyList if you don't actually need
 *  // one.
 *  val it3 = new Iterator[Int] {
 *    var i = -1
 *    def hasNext = true
 *    def next(): Int = { i += 1; i }
 *  }
 *  loop("Iterator3: ", it3.next, it3)
 *  }}}
 *
 *  - The fact that `tail` works at all is of interest.  In the definition of
 *  `fibs` we have an initial `(0, 1, LazyList(...))` so `tail` is deterministic.
 *  If we defined `fibs` such that only `0` were concretely known then the act
 *  of determining `tail` would require the evaluation of `tail` which would
 *  cause an infinite recursion and stack overflow.  If we define a definition
 *  where the tail is not initially computable then we're going to have an
 *  infinite recursion:
 *  {{{
 *  // The first time we try to access the tail we're going to need more
 *  // information which will require us to recurse, which will require us to
 *  // recurse, which...
 *  val sov: LazyList[Vector[Int]] = Vector(0) #:: sov.zip(sov.tail).map { n => n._1 ++ n._2 }
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
 *  Note that `mkString` forces evaluation of a `LazyList`, but `addString` does
 *  not.  In both cases, a `LazyList` that is or ends in a cycle
 *  (e.g. `lazy val s: LazyList[Int] = 0 #:: s`) will convert additional trips
 *  through the cycle to `...`.  Additionally, `addString` will display an
 *  un-memoized tail as `?`.
 *
 *  @tparam A    the type of the elements contained in this lazy list.
 *
 *  @author Martin Odersky, Matthias Zenger
 *  @version 1.1 08/08/03
 *  @since   2.8
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lazy-lists "Scala's Collection Library overview"]]
 *  section on `LazyLists` for more information.

 *  @define naturalsEx def naturalsFrom(i: Int): LazyList[Int] = i #:: naturalsFrom(i + 1)
 *  @define Coll `LazyList`
 *  @define coll lazy list
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define willTerminateInf Note: lazily evaluated; will terminate for infinite-sized collections.
 */
@deprecatedInheritance("This class will be sealed.", "2.11.0")
abstract class LazyList[+A] extends AbstractSeq[A]
                             with LinearSeq[A]
                             with GenericTraversableTemplate[A, LazyList]
                             with LinearSeqOptimized[A, LazyList[A]]
                             with Serializable { self =>

  override def companion: GenericCompanion[LazyList] = LazyList

  /** Indicates whether or not the `LazyList` is empty.
   *
   * @return `true` if the `LazyList` is empty and `false` otherwise.
   */
  def isEmpty: Boolean

  /** Gives constant time access to the first element of this `LazyList`.  Using
   * the `fibs` example from earlier:
   *
   * {{{
   * println(fibs head)
   * // prints
   * // 0
   * }}}
   *
   *  @return The first element of the `LazyList`.
   *  @throws java.util.NoSuchElementException if the lazy list is empty.
   */
  def head: A

  /** A lazy list consisting of the remaining elements of this lazy list after the
   *  first one.
   *
   *  Note that this method does not force evaluation of the `LazyList` but merely
   *  returns the lazy result.
   *
   *  @return The tail of the `LazyList`.
   *  @throws UnsupportedOperationException if the lazy list is empty.
   */
  def tail: LazyList[A]

  /** Is the tail of this lazy list defined? */
  protected def tailDefined: Boolean

  // Implementation of abstract method in Traversable

  // New methods in LazyList

  /** The lazy list resulting from the concatenation of this lazy list with the argument lazy list.
   *  @param rest   The lazy list that gets appended to this lazy list
   *  @return       The lazy list containing elements of this lazy list and the traversable object.
   */
  def append[B >: A](rest: => TraversableOnce[B]): LazyList[B] =
    if (isEmpty) rest.toLazyList else cons(head, tail append rest)

  /** Forces evaluation of the whole lazy list and returns it.
   *
   * @note Often we use `LazyList`s to represent an infinite set or series.  If
   * that's the case for your particular `LazyList` then this function will never
   * return and will probably crash the VM with an `OutOfMemory` exception.
   * This function will not hang on a finite cycle, however.
   *
   *  @return The fully realized `LazyList`.
   */
  def force: LazyList[A] = {
    // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
    var these, those = this
    if (!these.isEmpty) these = these.tail
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

  /** Prints elements of this lazy list one by one, separated by commas. */
  def print() { print(", ") }

  /** Prints elements of this lazy list one by one, separated by `sep`.
   *  @param sep   The separator string printed between consecutive elements.
   */
  def print(sep: String) {
    def loop(these: LazyList[A], start: String) {
      Console.print(start)
      if (these.isEmpty) Console.print("empty")
      else {
        Console.print(these.head)
        loop(these.tail, sep)
      }
    }
    loop(this, "")
  }

  /** Returns the length of this `LazyList`.
   *
   * @note In order to compute the length of the `LazyList`, it must first be
   * fully realized, which could cause the complete evaluation of an infinite
   * series, assuming that's what your `LazyList` represents.
   *
   * @return The length of this `LazyList`.
   */
  override def length: Int = {
    var len = 0
    var left = this
    while (!left.isEmpty) {
      len += 1
      left = left.tail
    }
    len
  }

  // It's an imperfect world, but at least we can bottle up the
  // imperfection in a capsule.
  @inline private def asThat[That](x: AnyRef): That     = x.asInstanceOf[That]
  @inline private def asLazyList[B](x: AnyRef): LazyList[B] = x.asInstanceOf[LazyList[B]]
  @inline private def isLazyListBuilder[B, That](bf: CanBuildFrom[LazyList[A], B, That]) =
    bf(repr).isInstanceOf[LazyList.LazyListBuilder[_]]

  // Overridden methods from Traversable

  override def toLazyList: LazyList[A] = this

  override def hasDefiniteSize: Boolean = isEmpty || {
    if (!tailDefined) false
    else {
      // Two-iterator trick (2x & 1x speed) for cycle detection.
      var those = this
      var these = tail
      while (those ne these) {
        if (these.isEmpty) return true
        if (!these.tailDefined) return false
        these = these.tail
        if (these.isEmpty) return true
        if (!these.tailDefined) return false
        these = these.tail
        if (those eq these) return false
        those = those.tail
      }
      false  // Cycle detected
    }
  }

  /** Create a new lazy list which contains all elements of this lazy list followed by
   * all elements of Traversable `that`.
   *
   * @note It's subtle why this works. We know that if the target type of the
   * [[scala.collection.mutable.Builder]] `That` is either a `LazyList`, or one of
   * its supertypes, or undefined, then `LazyListBuilder` will be chosen for the
   * implicit.  We recognize that fact and optimize to get more laziness.
   *
   * @note This method doesn't cause the `LazyList` to be fully realized but it
   * should be noted that using the `++` operator from another collection type
   * could cause infinite realization of a `LazyList`.  For example, referring to
   * the definition of `fibs` in the preamble, the following would never return:
   * `List(BigInt(12)) ++ fibs`.
   *
   * @tparam B The element type of the returned collection.'''That'''
   * @param that The [[scala.collection.GenTraversableOnce]] the be concatenated
   * to this `LazyList`.
   * @return A new collection containing the result of concatenating `this` with
   * `that`.
   */
  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
    // we assume there is no other builder factory on lazy lists and therefore know that That = LazyList[A]
    if (isLazyListBuilder(bf)) asThat(
      if (isEmpty) that.toLazyList
      else cons(head, asLazyList[A](tail ++ that))
    )
    else super.++(that)(bf)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
    if (isLazyListBuilder(bf)) asThat(cons(elem, this))
    else super.+:(elem)(bf)

  /**
   * Create a new lazy list which contains all intermediate results of applying the
   * operator to subsequent elements left to right.  `scanLeft` is analogous to
   * `foldLeft`.
   *
   * @note This works because the target type of the
   * [[scala.collection.mutable.Builder]] `That` is a `LazyList`.
   *
   * @param z The initial value for the scan.
   * @param op A function that will apply operations to successive values in the
   * `LazyList` against previous accumulated results.
   * @return A new collection containing the modifications from the application
   * of `op`.
   */
  override final def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
    if (isLazyListBuilder(bf)) asThat(
      if (isEmpty) LazyList(z)
      else cons(z, asLazyList[B](tail.scanLeft(op(z, head))(op)))
    )
    else super.scanLeft(z)(op)(bf)

  /** Returns the lazy list resulting from applying the given function `f` to each
   * element of this lazy list.  This returns a lazy `LazyList` such that it does not
   * need to be fully realized.
   *
   * @example {{{
   * $naturalsEx
   * naturalsFrom(1).map(_ + 10) take 5 mkString(", ")
   * // produces: "11, 12, 13, 14, 15"
   * }}}
   *
   * @tparam B The element type of the returned collection '''That'''.
   * @param f function to apply to each element.
   * @return  `f(a,,0,,), ..., f(a,,n,,)` if this sequence is `a,,0,,, ..., a,,n,,`.
   */
  override final def map[B, That](f: A => B)(implicit bf: CanBuildFrom[LazyList[A], B, That]): That = {
    if (isLazyListBuilder(bf)) asThat(
      if (isEmpty) LazyList.Empty
      else cons(f(head), asLazyList[B](tail map f))
    )
    else super.map(f)(bf)
  }

  override final def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[LazyList[A], B, That]): That = {
    if (!isLazyListBuilder(bf)) super.collect(pf)(bf)
    else {
      // this implementation avoids:
      // 1) stackoverflows (could be achieved with tailrec, too)
      // 2) out of memory errors for big lazy lists (`this` reference can be eliminated from the stack)
      var rest: LazyList[A] = this

      // Avoids calling both `pf.isDefined` and `pf.apply`.
      var newHead: B = null.asInstanceOf[B]
      val runWith = pf.runWith((b: B) => newHead = b)

      while (rest.nonEmpty && !runWith(rest.head)) rest = rest.tail

      //  without the call to the companion object, a thunk is created for the tail of the new lazy list,
      //  and the closure of the thunk will reference `this`
      if (rest.isEmpty) LazyList.Empty.asInstanceOf[That]
      else LazyList.collectedTail(newHead, rest, pf, bf).asInstanceOf[That]
    }
  }

  /** Applies the given function `f` to each element of this lazy list, then
   * concatenates the results.  As with `map` this function does not need to
   * realize the entire `LazyList` but continues to keep it as a lazy `LazyList`.
   *
   * @example {{{
   * // Let's create a LazyList of Vectors, each of which contains the
   * // collection of Fibonacci numbers up to the current value.  We
   * // can then 'flatMap' that LazyList.
   *
   * val fibVec: LazyList[Vector[Int]] = Vector(0) #:: Vector(0, 1) #:: fibVec.zip(fibVec.tail).map(n => {
   *   n._2 ++ Vector(n._1.last + n._2.last)
   * })
   *
   * fibVec take 5 foreach println
   * // prints
   * // Vector(0)
   * // Vector(0, 1)
   * // Vector(0, 1, 1)
   * // Vector(0, 1, 1, 2)
   * // Vector(0, 1, 1, 2, 3)
   *
   * // If we now want to `flatMap` across that lazy list by adding 10
   * // we can see what the series turns into:
   *
   * fibVec.flatMap(_.map(_ + 10)) take 15 mkString(", ")
   * // produces: 10, 10, 11, 10, 11, 11, 10, 11, 11, 12, 10, 11, 11, 12, 13
   * }}}
   *
   * ''Note:''  Currently `flatMap` will evaluate as much of the LazyList as needed
   * until it finds a non-empty element for the head, which is non-lazy.
   *
   * @tparam B The element type of the returned collection '''That'''.
   * @param f  the function to apply on each element.
   * @return  `f(a,,0,,) ::: ... ::: f(a,,n,,)` if
   *           this lazy list is `[a,,0,,, ..., a,,n,,]`.
   */
  override final def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
    // we assume there is no other builder factory on lazy lists and therefore know that That = LazyList[B]
    // optimisations are not for speed, but for functionality
    // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
    if (isLazyListBuilder(bf)) asThat(
      if (isEmpty) LazyList.Empty
      else {
        // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
        var nonEmptyPrefix = this
        var prefix = f(nonEmptyPrefix.head).toLazyList
        while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
          nonEmptyPrefix = nonEmptyPrefix.tail
          if(!nonEmptyPrefix.isEmpty)
            prefix = f(nonEmptyPrefix.head).toLazyList
        }

        if (nonEmptyPrefix.isEmpty) LazyList.empty
        else prefix append asLazyList[B](nonEmptyPrefix.tail flatMap f)
      }
    )
    else super.flatMap(f)(bf)

  override private[scala] def filterImpl(p: A => Boolean, isFlipped: Boolean): LazyList[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest = this
    while (!rest.isEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) LazyList.filteredTail(rest, p, isFlipped)
    else LazyList.Empty
  }

  /** A FilterMonadic which allows GC of the head of lazy list during processing */
  @noinline // Workaround SI-9137, see https://github.com/scala/scala/pull/4284#issuecomment-73180791
  override final def withFilter(p: A => Boolean): FilterMonadic[A, LazyList[A]] = new LazyList.LazyListWithFilter(this, p)

  /** A lazier Iterator than LinearSeqLike's. */
  override def iterator: Iterator[A] = new LazyListIterator(self)

  /** Apply the given function `f` to each element of this linear sequence
   * (while respecting the order of the elements).
   *
   *  @param f The treatment to apply to each element.
   *  @note  Overridden here as final to trigger tail-call optimization, which
   *  replaces 'this' with 'tail' at each iteration. This is absolutely
   *  necessary for allowing the GC to collect the underlying lazy list as elements
   *  are consumed.
   *  @note  This function will force the realization of the entire lazy list
   *  unless the `f` throws an exception.
   */
  @tailrec
  override final def foreach[U](f: A => U) {
    if (!this.isEmpty) {
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
  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  /** LazyList specialization of reduceLeft which allows GC to collect
   *  along the way.
   *
   * @tparam B The type of value being accumulated.
   * @param f The operation to perform on successive elements of the `LazyList`.
   * @return The accumulated value from successive applications of `f`.
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

  /** Returns all the elements of this lazy list that satisfy the predicate `p`
   * returning of [[scala.Tuple2]] of `LazyList`s obeying the partition predicate
   * `p`. The order of the elements is preserved.
   *
   * @param p the predicate used to filter the lazy list.
   * @return the elements of this lazy list satisfying `p`.
   *
   * @example {{{
   * $naturalsEx
   * val parts = naturalsFrom(1) partition { _ % 2 == 0 }
   * parts._1 take 10 mkString ", "
   * // produces: "2, 4, 6, 8, 10, 12, 14, 16, 18, 20"
   * parts._2 take 10 mkString ", "
   * // produces: "1, 3, 5, 7, 9, 11, 13, 15, 17, 19"
   * }}}
   *
   */
  override def partition(p: A => Boolean): (LazyList[A], LazyList[A]) = (filter(p(_)), filterNot(p(_)))

  /** Returns a lazy list formed from this lazy list and the specified lazy list `that`
   * by associating each element of the former with the element at the same
   * position in the latter.
   *
   * If one of the two lazy lists is longer than the other, its remaining elements
   * are ignored.
   *
   * The return type of this function may not be obvious.  The lazy aspect of
   * the returned value is different than that of `partition`.  In `partition`
   * we get back a [[scala.Tuple2]] of two lazy `LazyList`s whereas here we get
   * back a single lazy `LazyList` of [[scala.Tuple2]]s where the
   * [[scala.Tuple2]]'s type signature is `(A1, B)`.
   *
   * @tparam A1 The type of the first parameter of the zipped tuple
   * @tparam B The type of the second parameter of the zipped tuple
   * @tparam That The type of the returned `LazyList`.
   * @return `LazyList({a,,0,,,b,,0,,}, ...,
   *         {a,,min(m,n),,,b,,min(m,n),,)}` when
   *         `LazyList(a,,0,,, ..., a,,m,,)
   *         zip LazyList(b,,0,,, ..., b,,n,,)` is invoked.
   *
   * @example {{{
   * $naturalsEx
   * naturalsFrom(1) zip naturalsFrom(2) take 5 foreach println
   * // prints
   * // (1,2)
   * // (2,3)
   * // (3,4)
   * // (4,5)
   * // (5,6)
   * }}}
   */
  override final def zip[A1 >: A, B, That](that: scala.collection.GenIterable[B])(implicit bf: CanBuildFrom[LazyList[A], (A1, B), That]): That =
    // we assume there is no other builder factory on lazy lists and therefore know that That = LazyList[(A1, B)]
    if (isLazyListBuilder(bf)) asThat(
      if (this.isEmpty || that.isEmpty) LazyList.Empty
      else cons((this.head, that.head), asLazyList[(A1, B)](this.tail zip that.tail))
    )
    else super.zip(that)(bf)

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to `s
   * zip s.indices`.
   *
   * This method is much like `zip` in that it returns a single lazy `LazyList` of
   * [[scala.Tuple2]].
   *
   * @tparam A1 The type of the first element of the [[scala.Tuple2]] in the
   * resulting lazy list.
   * @tparam That The type of the resulting `LazyList`.
   * @return `LazyList({a,,0,,,0}, ..., {a,,n,,,n)}`
   *
   * @example {{{
   * $naturalsEx
   * (naturalsFrom(1) zipWithIndex) take 5 foreach println
   * // prints
   * // (1,0)
   * // (2,1)
   * // (3,2)
   * // (4,3)
   * // (5,4)
   * }}}
   */
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[LazyList[A], (A1, Int), That]): That =
    this.zip[A1, Int, That](LazyList.from(0))

  /** Write all defined elements of this iterable into given string builder.
   *  The written text begins with the string `start` and is finished by the string
   *  `end`. Inside, the string representations of defined elements (w.r.t.
   *  the method `toString()`) are separated by the string `sep`. The method will
   *  not force evaluation of undefined elements. A tail of such elements will be
   * represented by a `"?"` instead.  A cyclic lazy list is represented by a `"..."`
   * at the point where the cycle repeats.
   *
   * @param b The [[collection.mutable.StringBuilder]] factory to which we need
   * to add the string elements.
   * @param start The prefix of the resulting string (e.g. "LazyList(")
   * @param sep The separator between elements of the resulting string (e.g. ",")
   * @param end The end of the resulting string (e.g. ")")
   * @return The original [[collection.mutable.StringBuilder]] containing the
   * resulting string.
   */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b append start
    if (!isEmpty) {
      b append head
      var cursor = this
      var n = 1
      if (cursor.tailDefined) {  // If tailDefined, also !isEmpty
        var scout = tail
        if (scout.isEmpty) {
          // Single element.  Bail out early.
          b append end
          return b
        }
        if (cursor ne scout) {
          cursor = scout
          if (scout.tailDefined) {
            scout = scout.tail
            // Use 2x 1x iterator trick for cycle detection; slow iterator can add strings
            while ((cursor ne scout) && scout.tailDefined) {
              b append sep append cursor.head
              n += 1
              cursor = cursor.tail
              scout = scout.tail
              if (scout.tailDefined) scout = scout.tail
            }
          }
        }
        if (!scout.tailDefined) {  // Not a cycle, scout hit an end
          while (cursor ne scout) {
            b append sep append cursor.head
            n += 1
            cursor = cursor.tail
          }
          if (cursor.nonEmpty) {
            b append sep append cursor.head
          }
        }
        else {
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
          while (runner ne scout) {
            runner = runner.tail
            scout = scout.tail
            k += 1
          }
          // Now runner and scout are at the beginning of the cycle.  Advance
          // cursor, adding to string, until it hits; then we'll have covered
          // everything once.  If cursor is already at beginning, we'd better
          // advance one first unless runner didn't go anywhere (in which case
          // we've already looped once).
          if ((cursor eq scout) && (k > 0)) {
            b append sep append cursor.head
            n += 1
            cursor = cursor.tail
          }
          while (cursor ne scout) {
            b append sep append cursor.head
            n += 1
            cursor = cursor.tail
          }
          // Subtract prefix length from total length for cycle reporting.
          // (Not currently used, but probably a good idea for the future.)
          n -= k
        }
      }
      if (!cursor.isEmpty) {
        // Either undefined or cyclic; we can check with tailDefined
        if (!cursor.tailDefined) b append sep append "?"
        else b append sep append "..."
      }
    }
    b append end
    b
  }

  override def mkString(sep: String): String = mkString("", sep, "")
  override def mkString: String = mkString("")
  override def mkString(start: String, sep: String, end: String): String = {
    this.force
    super.mkString(start, sep, end)
  }
  override def toString = super.mkString(stringPrefix + "(", ", ", ")")

  override def splitAt(n: Int): (LazyList[A], LazyList[A]) = (take(n), drop(n))

  /** Returns the `n` first elements of this `LazyList` as another `LazyList`, or
   * else the whole `LazyList`, if it has less than `n` elements.
   *
   * The result of `take` is, again, a `LazyList` meaning that it also does not
   * make any needless evaluations of the `LazyList` itself, delaying that until
   * the usage of the resulting `LazyList`.
   *
   * @param n the number of elements to take.
   * @return the `n` first elements of this lazy list.
   *
   * @example {{{
   * $naturalsEx
   * scala> naturalsFrom(5) take 5
   * res1: scala.collection.immutable.LazyList[Int] = LazyList(5, ?)
   *
   * scala> naturalsFrom(5) take 5 mkString ", "
   * // produces: "5, 6, 7, 8, 9"
   * }}}
   */
  override def take(n: Int): LazyList[A] = (
    // Note that the n == 1 condition appears redundant but is not.
    // It prevents "tail" from being referenced (and its head being evaluated)
    // when obtaining the last element of the result. Such are the challenges
    // of working with a lazy-but-not-really sequence.
    if (n <= 0 || isEmpty) LazyList.empty
    else if (n == 1) cons(head, LazyList.empty)
    else cons(head, tail take n-1)
  )

  @tailrec final override def drop(n: Int): LazyList[A] =
    if (n <= 0 || isEmpty) this
    else tail drop n-1

  /** A sublazy list starting at index `from` and extending up to (but not including)
   *  index `until`.  This returns a `LazyList` that is lazily evaluated.
   *
   * @param from    The index of the first element of the returned subsequence
   * @param until   The index of the element following the returned subsequence
   * @return A new string containing the elements requested from `start` until
   * `end`.
   *
   * @example {{{
   * naturalsFrom(0) slice(50, 60) mkString ", "
   * // produces: "50, 51, 52, 53, 54, 55, 56, 57, 58, 59"
   * }}}
   */
  override def slice(from: Int, until: Int): LazyList[A] = {
    val lo = from max 0
    if (until <= lo || isEmpty) LazyList.empty
    else this drop lo take (until - lo)
  }

  /** The lazy list without its last element.
   *
   * @return A new `LazyList` containing everything but the last element.  If your
   * `LazyList` represents an infinite series, this method will not return.
   *
   *  @throws UnsupportedOperationException if the lazy list is empty.
   */
  override def init: LazyList[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) LazyList.Empty
    else cons(head, tail.init)

  /** Returns the rightmost `n` elements from this iterable.
   *
   * @note Take serious caution here.  If the `LazyList` represents an infinite
   * series then this function ''will not return''.  The right most elements of
   * an infinite series takes an infinite amount of time to produce.
   *
   *  @param n the number of elements to take
   *  @return The last `n` elements from this `LazyList`.
   */
  override def takeRight(n: Int): LazyList[A] = {
    var these: LazyList[A] = this
    var lead = this drop n
    while (!lead.isEmpty) {
      these = these.tail
      lead = lead.tail
    }
    these
  }

  /**
   * @inheritdoc
   * $willTerminateInf
   */
  override def dropRight(n: Int): LazyList[A] = {
    // We make dropRight work for possibly infinite lazy lists by carrying
    // a buffer of the dropped size. As long as the buffer is full and the
    // rest is non-empty, we can feed elements off the buffer head.  When
    // the rest becomes empty, the full buffer is the dropped elements.
    def advance(stub0: List[A], stub1: List[A], rest: LazyList[A]): LazyList[A] = {
      if (rest.isEmpty) LazyList.empty
      else if (stub0.isEmpty) advance(stub1.reverse, Nil, rest)
      else cons(stub0.head, advance(stub0.tail, rest.head :: stub1, rest.tail))
    }
    if (n <= 0) this
    else advance((this take n).toList, Nil, this drop n)
  }

  /** Returns the longest prefix of this `LazyList` whose elements satisfy the
   * predicate `p`.
   *
   * @param p the test predicate.
   * @return A new `LazyList` representing the values that satisfy the predicate
   * `p`.
   *
   * @example {{{
   + naturalsFrom(0) takeWhile { _ < 5 } mkString ", "
   * produces: "0, 1, 2, 3, 4"
   * }}}
   */
  override def takeWhile(p: A => Boolean): LazyList[A] =
    if (!isEmpty && p(head)) cons(head, tail takeWhile p)
    else LazyList.Empty

  /** Returns the a `LazyList` representing the longest suffix of this iterable
   * whose first element does not satisfy the predicate `p`.
   *
   * @note This method realizes the entire `LazyList` beyond the truth value of
   * the predicate `p`.
   *
   * @param p the test predicate.
   * @return A new `LazyList` representing the results of applying `p` to the
   * original `LazyList`.
   *
   * @example {{{
   * // Assume we have a LazyList that takes the first 20 natural numbers
   * def naturalsLt50(i: Int): LazyList[Int] = i #:: { if (i < 20) naturalsLt50(i * + 1) else LazyList.Empty }
   * naturalsLt50(0) dropWhile { _ < 10 }
   * // produces: "10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20"
   * }}}
   */
  override def dropWhile(p: A => Boolean): LazyList[A] = {
    var these: LazyList[A] = this
    while (!these.isEmpty && p(these.head)) these = these.tail
    these
  }

  /** Builds a new lazy list from this lazy list in which any duplicates (as
   * determined by `==`) have been removed. Among duplicate elements, only the
   * first one is retained in the resulting `LazyList`.
   *
   * @return A new `LazyList` representing the result of applying distinctness to
   * the original `LazyList`.
   * @example {{{
   * // Creates a LazyList where every element is duplicated
   * def naturalsFrom(i: Int): LazyList[Int] = i #:: { i #:: naturalsFrom(i + 1) }
   * naturalsFrom(1) take 6 mkString ", "
   * // produces: "1, 1, 2, 2, 3, 3"
   * (naturalsFrom(1) distinct) take 6 mkString ", "
   * // produces: "1, 2, 3, 4, 5, 6"
   * }}}
   */
  override def distinct: LazyList[A] = {
    // This should use max memory proportional to N, whereas
    // recursively calling distinct on the tail is N^2.
    def loop(seen: Set[A], rest: LazyList[A]): LazyList[A] = {
      if (rest.isEmpty) rest
      else if (seen(rest.head)) loop(seen, rest.tail)
      else cons(rest.head, loop(seen + rest.head, rest.tail))
    }
    loop(Set(), this)
  }

  /** Returns a new sequence of given length containing the elements of this
   * sequence followed by zero or more occurrences of given elements.
   *
   * @tparam B The type of the value to pad with.
   * @tparam That The type contained within the resulting `LazyList`.
   * @param len The number of elements to pad into the `LazyList`.
   * @param elem The value of the type `B` to use for padding.
   * @return A new `LazyList` representing the collection with values padding off
   * to the end. If your `LazyList` represents an infinite series, this method will
   * not return.
   * @example {{{
   * def naturalsFrom(i: Int): LazyList[Int] = i #:: { if (i < 5) naturalsFrom(i + 1) else LazyList.Empty }
   * naturalsFrom(1) padTo(10, 0) foreach println
   * // prints
   * // 1
   * // 2
   * // 3
   * // 4
   * // 5
   * // 0
   * // 0
   * // 0
   * // 0
   * // 0
   * }}}
   */
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[LazyList[A], B, That]): That = {
    def loop(len: Int, these: LazyList[A]): LazyList[B] =
      if (these.isEmpty) LazyList.fill(len)(elem)
      else cons(these.head, loop(len - 1, these.tail))

    if (isLazyListBuilder(bf)) asThat(loop(len, this))
    else super.padTo(len, elem)(bf)
  }

  /** A list consisting of all elements of this list in reverse order.
   *
   * @note This function must realize the entire `LazyList` in order to perform
   * this operation so if your `LazyList` represents an infinite sequence then
   * this function will never return.
   *
   * @return A new `LazyList` containing the representing of the original `LazyList`
   * in reverse order.
   *
   * @example {{{
   * def naturalsFrom(i: Int): LazyList[Int] = i #:: { if (i < 5) naturalsFrom(i + 1) else LazyList.Empty }
   * (naturalsFrom(1) reverse) foreach println
   * // prints
   * // 5
   * // 4
   * // 3
   * // 2
   * // 1
   * }}}
   */
  override def reverse: LazyList[A] = {
    var result: LazyList[A] = LazyList.Empty
    var these = this
    while (!these.isEmpty) {
      val r = LazyList.consWrapper(result).#::(these.head)
      r.tail // force it!
      result = r
      these = these.tail
    }
    result
  }

  /** Evaluates and concatenates all elements within the `LazyList` into a new
   * flattened `LazyList`.
   *
   * @tparam B The type of the elements of the resulting `LazyList`.
   * @return A new `LazyList` of type `B` of the flattened elements of `this`
   * `LazyList`.
   * @example {{{
   * val sov: LazyList[Vector[Int]] = Vector(0) #:: Vector(0, 0) #:: sov.zip(sov.tail).map { n => n._1 ++ n._2 }
   * sov.flatten take 10 mkString ", "
   * // produces: "0, 0, 0, 0, 0, 0, 0, 0, 0, 0"
   * }}}
   */
  override def flatten[B](implicit asTraversable: A => /*<:<!!!*/ GenTraversableOnce[B]): LazyList[B] = {
    var st: LazyList[A] = this
    while (st.nonEmpty) {
      val h = asTraversable(st.head)
      if (h.isEmpty) {
        st = st.tail
      } else {
        return h.toLazyList #::: st.tail.flatten
      }
    }
    LazyList.empty
  }

  override def view = new LazyListView[A, LazyList[A]] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  /** Defines the prefix of this object's `toString` representation as `LazyList`.
   */
  override def stringPrefix = "LazyList"

}

/** A specialized, extra-lazy implementation of a lazy list iterator, so it can
 *  iterate as lazily as it traverses the tail.
 */
final class LazyListIterator[+A] private() extends AbstractIterator[A] with Iterator[A] {
  def this(self: LazyList[A]) {
    this()
    these = new LazyCell(self)
  }

  // A call-by-need cell.
  class LazyCell(st: => LazyList[A]) {
    lazy val v = st
  }

  private var these: LazyCell = _

  def hasNext: Boolean = these.v.nonEmpty
  def next(): A =
    if (isEmpty) Iterator.empty.next()
    else {
      val cur    = these.v
      val result = cur.head
      these = new LazyCell(cur.tail)
      result
    }
  override def toLazyList = {
    val result = these.v
    these = new LazyCell(LazyList.empty)
    result
  }
  override def toList   = toLazyList.toList
}

/**
 * The object `LazyList` provides helper functions to manipulate lazy lists.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 * @since   2.8
 */
object LazyList extends SeqFactory[LazyList] {

  /** The factory for lazy lists.
   *  @note Methods such as map/flatMap will not invoke the `Builder` factory,
   *        but will return a new lazy list directly, to preserve laziness.
   *        The new lazy list is then cast to the factory's result type.
   *        This means that every CanBuildFrom that takes a
   *        LazyList as its From type parameter must yield a lazy list as its result parameter.
   *        If that assumption is broken, cast errors might result.
   */
  class LazyListCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, LazyList[A]] = new LazyListCanBuildFrom[A]

  /** Creates a new builder for a lazy list */
  def newBuilder[A]: Builder[A, LazyList[A]] = new LazyListBuilder[A]

  /** A builder for lazy lists
   *  @note This builder is lazy only in the sense that it does not go downs the spine
   *        of traversables that are added as a whole. If more laziness can be achieved,
   *        this builder should be bypassed.
   */
  class LazyListBuilder[A] extends LazyBuilder[A, LazyList[A]] {
    def result: LazyList[A] = parts.toLazyList flatMap (_.toLazyList)
  }

  object Empty extends LazyList[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("head of empty lazy list")
    override def tail = throw new UnsupportedOperationException("tail of empty lazy list")
    def tailDefined = false
  }

  /** The empty lazy list */
  override def empty[A]: LazyList[A] = Empty

  /** A lazy list consisting of given elements */
  override def apply[A](xs: A*): LazyList[A] = xs.toLazyList

  /** A wrapper class that adds `#::` for cons and `#:::` for concat as operations
   *  to lazy lists.
   */
  class ConsWrapper[A](tl: => LazyList[A]) {
    /** Construct a lazy list consisting of a given first element followed by elements
     *  from a lazily evaluated LazyList.
     */
    def #::(hd: A): LazyList[A] = cons(hd, tl)
    /** Construct a lazy list consisting of the concatenation of the given lazy list and
     *  a lazily evaluated LazyList.
     */
    def #:::(prefix: LazyList[A]): LazyList[A] = prefix append tl
  }

  /** A wrapper method that adds `#::` for cons and `#:::` for concat as operations
   *  to lazy lists.
   */
  implicit def consWrapper[A](lazyList: => LazyList[A]): ConsWrapper[A] =
    new ConsWrapper[A](lazyList)

  /** An extractor that allows to pattern match lazy lists with `#::`.
   */
  object #:: {
    def unapply[A](xs: LazyList[A]): Option[(A, LazyList[A])] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

  /** An alternative way of building and matching LazyLists using LazyList.cons(hd, tl).
   */
  object cons {

    /** A lazy list consisting of a given first element and remaining elements
     *  @param hd   The first element of the result lazy list
     *  @param tl   The remaining elements of the result lazy list
     */
    def apply[A](hd: A, tl: => LazyList[A]) = new Cons(hd, tl)

    /** Maps a lazy list to its head and tail */
    def unapply[A](xs: LazyList[A]): Option[(A, LazyList[A])] = #::.unapply(xs)
  }

  /** A lazy cons cell, from which lazy lists are built. */
  @SerialVersionUID(-602202424901551803L)
  final class Cons[+A](hd: A, tl: => LazyList[A]) extends LazyList[A] {
    override def isEmpty = false
    override def head = hd
    @volatile private[this] var tlVal: LazyList[A] = _
    @volatile private[this] var tlGen = tl _
    def tailDefined: Boolean = tlGen eq null
    override def tail: LazyList[A] = {
      if (!tailDefined)
        synchronized {
          if (!tailDefined) {
            tlVal = tlGen()
            tlGen = null
          }
        }

      tlVal
    }
  }

  /** An infinite lazy list that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the lazy list
   *  @param f     the function that's repeatedly applied
   *  @return      the lazy list returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A)(f: A => A): LazyList[A] = cons(start, iterate(f(start))(f))

  override def iterate[A](start: A, len: Int)(f: A => A): LazyList[A] =
    iterate(start)(f) take len

  /**
   * Create an infinite lazy list starting at `start` and incrementing by
   * step `step`.
   *
   * @param start the start value of the lazy list
   * @param step the increment value of the lazy list
   * @return the lazy list starting at value `start`.
   */
  def from(start: Int, step: Int): LazyList[Int] =
    cons(start, from(start+step, step))

  /**
   * Create an infinite lazy list starting at `start` and incrementing by `1`.
   *
   * @param start the start value of the lazy list
   * @return the lazy list starting at value `start`.
   */
  def from(start: Int): LazyList[Int] = from(start, 1)

  /**
   * Create an infinite lazy list containing the given element expression (which
   * is computed for each occurrence).
   *
   * @param elem the element composing the resulting lazy list
   * @return the lazy list containing an infinite number of elem
   */
  def continually[A](elem: => A): LazyList[A] = cons(elem, continually(elem))

  override def fill[A](n: Int)(elem: => A): LazyList[A] =
    if (n <= 0) Empty else cons(elem, fill(n-1)(elem))

  override def tabulate[A](n: Int)(f: Int => A): LazyList[A] = {
    def loop(i: Int): LazyList[A] =
      if (i >= n) Empty else cons(f(i), loop(i+1))
    loop(0)
  }

  override def range[T: Integral](start: T, end: T, step: T): LazyList[T] = {
    val num = implicitly[Integral[T]]
    import num._

    if (if (step < zero) start <= end else end <= start) Empty
    else cons(start, range(start + step, end, step))
  }

  private[immutable] def filteredTail[A](lazyList: LazyList[A], p: A => Boolean, isFlipped: Boolean) = {
    cons(lazyList.head, lazyList.tail.filterImpl(p, isFlipped))
  }

  private[immutable] def collectedTail[A, B, That](head: B, lazyList: LazyList[A], pf: PartialFunction[A, B], bf: CanBuildFrom[LazyList[A], B, That]) = {
    cons(head, lazyList.tail.collect(pf)(bf).asInstanceOf[LazyList[B]])
  }

  /** An implementation of `FilterMonadic` allowing GC of the filtered-out elements of
    * the `LazyList` as it is processed.
    *
    * Because this is not an inner class of `LazyList` with a reference to the original
    * head, it is now possible for GC to collect any leading and filtered-out elements
    * which do not satisfy the filter, while the tail is still processing (see SI-8990).
    */
  private[immutable] final class LazyListWithFilter[A](sl: => LazyList[A], p: A => Boolean) extends FilterMonadic[A, LazyList[A]] {
    private var s = sl                                              // set to null to allow GC after filtered
    private lazy val filtered = { val f = s filter p; s = null; f } // don't set to null if throw during filter

    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
      filtered map f

    def flatMap[B, That](f: A => scala.collection.GenTraversableOnce[B])(implicit bf: CanBuildFrom[LazyList[A], B, That]): That =
      filtered flatMap f

    def foreach[U](f: A => U): Unit =
      filtered foreach f

    def withFilter(q: A => Boolean): FilterMonadic[A, LazyList[A]] =
      new LazyListWithFilter[A](filtered, q)
  }

}
