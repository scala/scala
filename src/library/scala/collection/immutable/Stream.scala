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
import Stream.cons
import scala.language.implicitConversions

/** The class `Stream` implements lazy lists where elements
 *  are only evaluated when they are needed. Here is an example:
 *
 *  {{{
 *  import scala.math.BigInt
 *  object Main extends App {
 *
 *    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
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
 *  The `Stream` class also employs memoization such that previously computed
 *  values are converted from `Stream` elements to concrete values of type `A`.
 *  To illustrate, we will alter body of the `fibs` value above and take some
 *  more values:
 *
 *  {{{
 *  import scala.math.BigInt
 *  object Main extends App {
 *
 *    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(
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
 *  `Stream` requires us to have somewhere to store the information and a `val`
 *  allows us to do that.
 *
 *  - While the `Stream` is actually being modified during access, this does not
 *  change the notion of its immutability.  Once the values are memoized they do
 *  not change and values that have yet to be memoized still "exist", they
 *  simply haven't been realized yet.
 *
 *  - One must be cautious of memoization; you can very quickly eat up large
 *  amounts of memory if you're not careful.  The reason for this is that the
 *  memoization of the `Stream` creates a structure much like
 *  [[scala.collection.immutable.List]].  So long as something is holding on to
 *  the head, the head holds on to the tail, and so it continues recursively.
 *  If, on the other hand, there is nothing holding on to the head (e.g. we used
 *  `def` to define the `Stream`) then once it is no longer being used directly,
 *  it disappears.
 *
 *  - Note that some operations, including [[drop]], [[dropWhile]],
 *  [[flatMap]] or [[collect]] may process a large number of intermediate
 *  elements before returning.  These necessarily hold onto the head, since
 *  they are methods on `Stream`, and a stream holds its own head.  For
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
 *  // Our first Stream definition will be a val definition
 *  val stream1: Stream[Int] = {
 *    def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
 *    loop(0)
 *  }
 *
 *  // Because stream1 is a val, everything that the iterator produces is held
 *  // by virtue of the fact that the head of the Stream is held in stream1
 *  val it1 = stream1.iterator
 *  loop("Iterator1: ", it1.next, it1)
 *
 *  // We can redefine this Stream such that all we have is the Iterator left
 *  // and allow the Stream to be garbage collected as required.  Using a def
 *  // to provide the Stream ensures that no val is holding onto the head as
 *  // is the case with stream1
 *  def stream2: Stream[Int] = {
 *    def loop(v: Int): Stream[Int] = v #:: loop(v + 1)
 *    loop(0)
 *  }
 *  val it2 = stream2.iterator
 *  loop("Iterator2: ", it2.next, it2)
 *
 *  // And, of course, we don't actually need a Stream at all for such a simple
 *  // problem.  There's no reason to use a Stream if you don't actually need
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
 *  `fibs` we have an initial `(0, 1, Stream(...))` so `tail` is deterministic.
 *  If we defined `fibs` such that only `0` were concretely known then the act
 *  of determining `tail` would require the evaluation of `tail` which would
 *  cause an infinite recursion and stack overflow.  If we define a definition
 *  where the tail is not initially computable then we're going to have an
 *  infinite recursion:
 *  {{{
 *  // The first time we try to access the tail we're going to need more
 *  // information which will require us to recurse, which will require us to
 *  // recurse, which...
 *  lazy val sov: Stream[Vector[Int]] = Vector(0) #:: sov.zip(sov.tail).map { n => n._1 ++ n._2 }
 *  }}}
 *
 *  The definition of `fibs` above creates a larger number of objects than
 *  necessary depending on how you might want to implement it.  The following
 *  implementation provides a more "cost effective" implementation due to the
 *  fact that it has a more direct route to the numbers themselves:
 *
 *  {{{
 *  lazy val fib: Stream[Int] = {
 *    def loop(h: Int, n: Int): Stream[Int] = h #:: loop(n, h + n)
 *    loop(1, 1)
 *  }
 *  }}}
 *
 *  Note that `mkString` forces evaluation of a `Stream`, but `addString` does
 *  not.  In both cases, a `Stream` that is or ends in a cycle
 *  (e.g. `lazy val s: Stream[Int] = 0 #:: s`) will convert additional trips
 *  through the cycle to `...`.  Additionally, `addString` will display an
 *  un-memoized tail as `?`.
 *
 *  @tparam A    the type of the elements contained in this stream.
 *
 *  @author Martin Odersky, Matthias Zenger
 *  @version 1.1 08/08/03
 *  @since   2.8
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#streams "Scala's Collection Library overview"]]
 *  section on `Streams` for more information.

 *  @define naturalsEx def naturalsFrom(i: Int): Stream[Int] = i #:: naturalsFrom(i + 1)
 *  @define Coll `Stream`
 *  @define coll stream
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define willTerminateInf Note: lazily evaluated; will terminate for infinite-sized collections.
 */
sealed abstract class Stream[+A] extends AbstractSeq[A]
                             with LinearSeq[A]
                             with GenericTraversableTemplate[A, Stream]
                             with LinearSeqOptimized[A, Stream[A]]
                             with Serializable { self =>

  override def companion: GenericCompanion[Stream] = Stream

  /** Indicates whether or not the `Stream` is empty.
   *
   * @return `true` if the `Stream` is empty and `false` otherwise.
   */
  def isEmpty: Boolean

  /** Gives constant time access to the first element of this `Stream`.  Using
   * the `fibs` example from earlier:
   *
   * {{{
   * println(fibs head)
   * // prints
   * // 0
   * }}}
   *
   *  @return The first element of the `Stream`.
   *  @throws java.util.NoSuchElementException if the stream is empty.
   */
  def head: A

  /** A stream consisting of the remaining elements of this stream after the
   *  first one.
   *
   *  Note that this method does not force evaluation of the `Stream` but merely
   *  returns the lazy result.
   *
   *  @return The tail of the `Stream`.
   *  @throws UnsupportedOperationException if the stream is empty.
   */
  def tail: Stream[A]

  /** Is the tail of this stream defined? */
  protected def tailDefined: Boolean

  // Implementation of abstract method in Traversable

  // New methods in Stream

  /** The stream resulting from the concatenation of this stream with the argument stream.
   *  @param rest   The stream that gets appended to this stream
   *  @return       The stream containing elements of this stream and the traversable object.
   */
  def append[B >: A](rest: => TraversableOnce[B]): Stream[B] =
    if (isEmpty) rest.toStream else cons(head, tail append rest)

  /** Forces evaluation of the whole stream and returns it.
   *
   * @note Often we use `Stream`s to represent an infinite set or series.  If
   * that's the case for your particular `Stream` then this function will never
   * return and will probably crash the VM with an `OutOfMemory` exception.
   * This function will not hang on a finite cycle, however.
   *
   *  @return The fully realized `Stream`.
   */
  def force: Stream[A] = {
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

  /** Prints elements of this stream one by one, separated by commas. */
  def print() { print(", ") }

  /** Prints elements of this stream one by one, separated by `sep`.
   *  @param sep   The separator string printed between consecutive elements.
   */
  def print(sep: String) {
    def loop(these: Stream[A], start: String) {
      Console.print(start)
      if (these.isEmpty) Console.print("empty")
      else {
        Console.print(these.head)
        loop(these.tail, sep)
      }
    }
    loop(this, "")
  }

  /** Returns the length of this `Stream`.
   *
   * @note In order to compute the length of the `Stream`, it must first be
   * fully realized, which could cause the complete evaluation of an infinite
   * series, assuming that's what your `Stream` represents.
   *
   * @return The length of this `Stream`.
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
  @inline private def asStream[B](x: AnyRef): Stream[B] = x.asInstanceOf[Stream[B]]
  @inline private def isStreamBuilder[B, That](bf: CanBuildFrom[Stream[A], B, That]) =
    bf(repr).isInstanceOf[Stream.StreamBuilder[_]]

  // Overridden methods from Traversable

  override def toStream: Stream[A] = this

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

  /** Create a new stream which contains all elements of this stream followed by
   * all elements of Traversable `that`.
   *
   * @note It's subtle why this works. We know that if the target type of the
   * [[scala.collection.mutable.Builder]] `That` is either a `Stream`, or one of
   * its supertypes, or undefined, then `StreamBuilder` will be chosen for the
   * implicit.  We recognize that fact and optimize to get more laziness.
   *
   * @note This method doesn't cause the `Stream` to be fully realized but it
   * should be noted that using the `++` operator from another collection type
   * could cause infinite realization of a `Stream`.  For example, referring to
   * the definition of `fibs` in the preamble, the following would never return:
   * `List(BigInt(12)) ++ fibs`.
   *
   * @tparam B The element type of the returned collection.'''That'''
   * @param that The [[scala.collection.GenTraversableOnce]] to be concatenated
   * to this `Stream`.
   * @return A new collection containing the result of concatenating `this` with
   * `that`.
   */
  override def ++[B >: A, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[A]
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) that.toStream
      else cons(head, asStream[A](tail ++ that))
    )
    else super.++(that)(bf)

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(cons(elem, this))
    else super.+:(elem)(bf)

  /**
   * Create a new stream which contains all intermediate results of applying the
   * operator to subsequent elements left to right.  `scanLeft` is analogous to
   * `foldLeft`.
   *
   * @note This works because the target type of the
   * [[scala.collection.mutable.Builder]] `That` is a `Stream`.
   *
   * @param z The initial value for the scan.
   * @param op A function that will apply operations to successive values in the
   * `Stream` against previous accumulated results.
   * @return A new collection containing the modifications from the application
   * of `op`.
   */
  override final def scanLeft[B, That](z: B)(op: (B, A) => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream(z)
      else cons(z, asStream[B](tail.scanLeft(op(z, head))(op)))
    )
    else super.scanLeft(z)(op)(bf)

  /** Returns the stream resulting from applying the given function `f` to each
   * element of this stream.  This returns a lazy `Stream` such that it does not
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
  override final def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream.Empty
      else cons(f(head), asStream[B](tail map f))
    )
    else super.map(f)(bf)
  }

  override final def collect[B, That](pf: PartialFunction[A, B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    if (!isStreamBuilder(bf)) super.collect(pf)(bf)
    else {
      // this implementation avoids:
      // 1) stackoverflows (could be achieved with tailrec, too)
      // 2) out of memory errors for big streams (`this` reference can be eliminated from the stack)
      var rest: Stream[A] = this

      // Avoids calling both `pf.isDefined` and `pf.apply`.
      var newHead: B = null.asInstanceOf[B]
      val runWith = pf.runWith((b: B) => newHead = b)

      while (rest.nonEmpty && !runWith(rest.head)) rest = rest.tail

      //  without the call to the companion object, a thunk is created for the tail of the new stream,
      //  and the closure of the thunk will reference `this`
      if (rest.isEmpty) Stream.Empty.asInstanceOf[That]
      else Stream.collectedTail(newHead, rest, pf, bf).asInstanceOf[That]
    }
  }

  /** Applies the given function `f` to each element of this stream, then
   * concatenates the results.  As with `map` this function does not need to
   * realize the entire `Stream` but continues to keep it as a lazy `Stream`.
   *
   * @example {{{
   * // Let's create a Stream of Vectors, each of which contains the
   * // collection of Fibonacci numbers up to the current value.  We
   * // can then 'flatMap' that Stream.
   *
   * val fibVec: Stream[Vector[Int]] = Vector(0) #:: Vector(0, 1) #:: fibVec.zip(fibVec.tail).map(n => {
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
   * // If we now want to `flatMap` across that stream by adding 10
   * // we can see what the series turns into:
   *
   * fibVec.flatMap(_.map(_ + 10)) take 15 mkString(", ")
   * // produces: 10, 10, 11, 10, 11, 11, 10, 11, 11, 12, 10, 11, 11, 12, 13
   * }}}
   *
   * ''Note:''  Currently `flatMap` will evaluate as much of the Stream as needed
   * until it finds a non-empty element for the head, which is non-lazy.
   *
   * @tparam B The element type of the returned collection '''That'''.
   * @param f  the function to apply on each element.
   * @return  `f(a,,0,,) ::: ... ::: f(a,,n,,)` if
   *           this stream is `[a,,0,,, ..., a,,n,,]`.
   */
  override final def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[B]
    // optimisations are not for speed, but for functionality
    // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
    if (isStreamBuilder(bf)) asThat(
      if (isEmpty) Stream.Empty
      else {
        // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
        var nonEmptyPrefix = this
        var prefix = f(nonEmptyPrefix.head).toStream
        while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
          nonEmptyPrefix = nonEmptyPrefix.tail
          if(!nonEmptyPrefix.isEmpty)
            prefix = f(nonEmptyPrefix.head).toStream
        }

        if (nonEmptyPrefix.isEmpty) Stream.empty
        else prefix append asStream[B](nonEmptyPrefix.tail flatMap f)
      }
    )
    else super.flatMap(f)(bf)

  override private[scala] def filterImpl(p: A => Boolean, isFlipped: Boolean): Stream[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest = this
    while (!rest.isEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) Stream.filteredTail(rest, p, isFlipped)
    else Stream.Empty
  }

  /** A FilterMonadic which allows GC of the head of stream during processing */
  @noinline // Workaround scala/bug#9137, see https://github.com/scala/scala/pull/4284#issuecomment-73180791
  override final def withFilter(p: A => Boolean): FilterMonadic[A, Stream[A]] = new Stream.StreamWithFilter(this, p)

  /** A lazier Iterator than LinearSeqLike's. */
  override def iterator: Iterator[A] = new StreamIterator(self)

  /** Apply the given function `f` to each element of this linear sequence
   * (while respecting the order of the elements).
   *
   *  @param f The treatment to apply to each element.
   *  @note  Overridden here as final to trigger tail-call optimization, which
   *  replaces 'this' with 'tail' at each iteration. This is absolutely
   *  necessary for allowing the GC to collect the underlying stream as elements
   *  are consumed.
   *  @note  This function will force the realization of the entire stream
   *  unless the `f` throws an exception.
   */
  @tailrec
  override final def foreach[U](f: A => U) {
    if (!this.isEmpty) {
      f(head)
      tail.foreach(f)
    }
  }

  /** Stream specialization of foldLeft which allows GC to collect along the
   * way.
   *
   * @tparam B The type of value being accumulated.
   * @param z The initial value seeded into the function `op`.
   * @param op The operation to perform on successive elements of the `Stream`.
   * @return The accumulated value from successive applications of `op`.
   */
  @tailrec
  override final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (this.isEmpty) z
    else tail.foldLeft(op(z, head))(op)
  }

  /** Stream specialization of reduceLeft which allows GC to collect
   *  along the way.
   *
   * @tparam B The type of value being accumulated.
   * @param f The operation to perform on successive elements of the `Stream`.
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

  /** Returns all the elements of this stream that satisfy the predicate `p`
   * returning of [[scala.Tuple2]] of `Stream`s obeying the partition predicate
   * `p`. The order of the elements is preserved.
   *
   * @param p the predicate used to filter the stream.
   * @return the elements of this stream satisfying `p`.
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
  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), filterNot(p(_)))

  /** Returns a stream formed from this stream and the specified stream `that`
   * by associating each element of the former with the element at the same
   * position in the latter.
   *
   * If one of the two streams is longer than the other, its remaining elements
   * are ignored.
   *
   * The return type of this function may not be obvious.  The lazy aspect of
   * the returned value is different than that of `partition`.  In `partition`
   * we get back a [[scala.Tuple2]] of two lazy `Stream`s whereas here we get
   * back a single lazy `Stream` of [[scala.Tuple2]]s where the
   * [[scala.Tuple2]]'s type signature is `(A1, B)`.
   *
   * @tparam A1 The type of the first parameter of the zipped tuple
   * @tparam B The type of the second parameter of the zipped tuple
   * @tparam That The type of the returned `Stream`.
   * @return `Stream({a,,0,,,b,,0,,}, ...,
   *         {a,,min(m,n),,,b,,min(m,n),,)}` when
   *         `Stream(a,,0,,, ..., a,,m,,)
   *         zip Stream(b,,0,,, ..., b,,n,,)` is invoked.
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
  override final def zip[A1 >: A, B, That](that: scala.collection.GenIterable[B])(implicit bf: CanBuildFrom[Stream[A], (A1, B), That]): That =
    // we assume there is no other builder factory on streams and therefore know that That = Stream[(A1, B)]
    if (isStreamBuilder(bf)) asThat(
      if (this.isEmpty || that.isEmpty) Stream.Empty
      else cons((this.head, that.head), asStream[(A1, B)](this.tail zip that.tail))
    )
    else super.zip(that)(bf)

  /** Zips this iterable with its indices. `s.zipWithIndex` is equivalent to `s
   * zip s.indices`.
   *
   * This method is much like `zip` in that it returns a single lazy `Stream` of
   * [[scala.Tuple2]].
   *
   * @tparam A1 The type of the first element of the [[scala.Tuple2]] in the
   * resulting stream.
   * @tparam That The type of the resulting `Stream`.
   * @return `Stream({a,,0,,,0}, ..., {a,,n,,,n)}`
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
  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[Stream[A], (A1, Int), That]): That =
    this.zip[A1, Int, That](Stream.from(0))

  /** Write all defined elements of this iterable into given string builder.
   *  The written text begins with the string `start` and is finished by the string
   *  `end`. Inside, the string representations of defined elements (w.r.t.
   *  the method `toString()`) are separated by the string `sep`. The method will
   *  not force evaluation of undefined elements. A tail of such elements will be
   * represented by a `"?"` instead.  A cyclic stream is represented by a `"..."`
   * at the point where the cycle repeats.
   *
   * @param b The [[collection.mutable.StringBuilder]] factory to which we need
   * to add the string elements.
   * @param start The prefix of the resulting string (e.g. "Stream(")
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

  override def splitAt(n: Int): (Stream[A], Stream[A]) = (take(n), drop(n))

  /** Returns the `n` first elements of this `Stream` as another `Stream`, or
   * else the whole `Stream`, if it has less than `n` elements.
   *
   * The result of `take` is, again, a `Stream` meaning that it also does not
   * make any needless evaluations of the `Stream` itself, delaying that until
   * the usage of the resulting `Stream`.
   *
   * @param n the number of elements to take.
   * @return the `n` first elements of this stream.
   *
   * @example {{{
   * $naturalsEx
   * scala> naturalsFrom(5) take 5
   * res1: scala.collection.immutable.Stream[Int] = Stream(5, ?)
   *
   * scala> naturalsFrom(5) take 5 mkString ", "
   * // produces: "5, 6, 7, 8, 9"
   * }}}
   */
  override def take(n: Int): Stream[A] = (
    // Note that the n == 1 condition appears redundant but is not.
    // It prevents "tail" from being referenced (and its head being evaluated)
    // when obtaining the last element of the result. Such are the challenges
    // of working with a lazy-but-not-really sequence.
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) cons(head, Stream.empty)
    else cons(head, tail take n-1)
  )

  @tailrec final override def drop(n: Int): Stream[A] =
    if (n <= 0 || isEmpty) this
    else tail drop n-1

  /** A substream starting at index `from` and extending up to (but not including)
   *  index `until`.  This returns a `Stream` that is lazily evaluated.
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
  override def slice(from: Int, until: Int): Stream[A] = {
    val lo = from max 0
    if (until <= lo || isEmpty) Stream.empty
    else this drop lo take (until - lo)
  }

  /** The stream without its last element.
   *
   * @return A new `Stream` containing everything but the last element.  If your
   * `Stream` represents an infinite series, this method will not return.
   *
   *  @throws UnsupportedOperationException if the stream is empty.
   */
  override def init: Stream[A] =
    if (isEmpty) super.init
    else if (tail.isEmpty) Stream.Empty
    else cons(head, tail.init)

  /** Returns the rightmost `n` elements from this iterable.
   *
   * @note Take serious caution here.  If the `Stream` represents an infinite
   * series then this function ''will not return''.  The right most elements of
   * an infinite series takes an infinite amount of time to produce.
   *
   *  @param n the number of elements to take
   *  @return The last `n` elements from this `Stream`.
   */
  override def takeRight(n: Int): Stream[A] = {
    var these: Stream[A] = this
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
  override def dropRight(n: Int): Stream[A] = {
    // We make dropRight work for possibly infinite streams by carrying
    // a buffer of the dropped size. As long as the buffer is full and the
    // rest is non-empty, we can feed elements off the buffer head.  When
    // the rest becomes empty, the full buffer is the dropped elements.
    def advance(stub0: List[A], stub1: List[A], rest: Stream[A]): Stream[A] = {
      if (rest.isEmpty) Stream.empty
      else if (stub0.isEmpty) advance(stub1.reverse, Nil, rest)
      else cons(stub0.head, advance(stub0.tail, rest.head :: stub1, rest.tail))
    }
    if (n <= 0) this
    else advance((this take n).toList, Nil, this drop n)
  }

  /** Returns the longest prefix of this `Stream` whose elements satisfy the
   * predicate `p`.
   *
   * @param p the test predicate.
   * @return A new `Stream` representing the values that satisfy the predicate
   * `p`.
   *
   * @example {{{
   + naturalsFrom(0) takeWhile { _ < 5 } mkString ", "
   * produces: "0, 1, 2, 3, 4"
   * }}}
   */
  override def takeWhile(p: A => Boolean): Stream[A] =
    if (!isEmpty && p(head)) cons(head, tail takeWhile p)
    else Stream.Empty

  /** Returns the a `Stream` representing the longest suffix of this iterable
   * whose first element does not satisfy the predicate `p`.
   *
   * @note This method realizes the entire `Stream` beyond the truth value of
   * the predicate `p`.
   *
   * @param p the test predicate.
   * @return A new `Stream` representing the results of applying `p` to the
   * original `Stream`.
   *
   * @example {{{
   * // Assume we have a Stream that takes the first 20 natural numbers
   * def naturalsLt50(i: Int): Stream[Int] = i #:: { if (i < 20) naturalsLt50(i * + 1) else Stream.Empty }
   * naturalsLt50(0) dropWhile { _ < 10 }
   * // produces: "10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20"
   * }}}
   */
  override def dropWhile(p: A => Boolean): Stream[A] = {
    var these: Stream[A] = this
    while (!these.isEmpty && p(these.head)) these = these.tail
    these
  }

  /** Builds a new stream from this stream in which any duplicates (as
   * determined by `==`) have been removed. Among duplicate elements, only the
   * first one is retained in the resulting `Stream`.
   *
   * @return A new `Stream` representing the result of applying distinctness to
   * the original `Stream`.
   * @example {{{
   * // Creates a Stream where every element is duplicated
   * def naturalsFrom(i: Int): Stream[Int] = i #:: { i #:: naturalsFrom(i + 1) }
   * naturalsFrom(1) take 6 mkString ", "
   * // produces: "1, 1, 2, 2, 3, 3"
   * (naturalsFrom(1) distinct) take 6 mkString ", "
   * // produces: "1, 2, 3, 4, 5, 6"
   * }}}
   */
  override def distinct: Stream[A] = {
    // This should use max memory proportional to N, whereas
    // recursively calling distinct on the tail is N^2.
    def loop(seen: Set[A], rest: Stream[A]): Stream[A] = {
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
   * @tparam That The type contained within the resulting `Stream`.
   * @param len The number of elements to pad into the `Stream`.
   * @param elem The value of the type `B` to use for padding.
   * @return A new `Stream` representing the collection with values padding off
   * to the end. If your `Stream` represents an infinite series, this method will
   * not return.
   * @example {{{
   * def naturalsFrom(i: Int): Stream[Int] = i #:: { if (i < 5) naturalsFrom(i + 1) else Stream.Empty }
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
  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That = {
    def loop(len: Int, these: Stream[A]): Stream[B] =
      if (these.isEmpty) Stream.fill(len)(elem)
      else cons(these.head, loop(len - 1, these.tail))

    if (isStreamBuilder(bf)) asThat(loop(len, this))
    else super.padTo(len, elem)(bf)
  }

  /** A list consisting of all elements of this list in reverse order.
   *
   * @note This function must realize the entire `Stream` in order to perform
   * this operation so if your `Stream` represents an infinite sequence then
   * this function will never return.
   *
   * @return A new `Stream` containing the representing of the original `Stream`
   * in reverse order.
   *
   * @example {{{
   * def naturalsFrom(i: Int): Stream[Int] = i #:: { if (i < 5) naturalsFrom(i + 1) else Stream.Empty }
   * (naturalsFrom(1) reverse) foreach println
   * // prints
   * // 5
   * // 4
   * // 3
   * // 2
   * // 1
   * }}}
   */
  override def reverse: Stream[A] = {
    var result: Stream[A] = Stream.Empty
    var these = this
    while (!these.isEmpty) {
      val r = Stream.consWrapper(result).#::(these.head)
      r.tail // force it!
      result = r
      these = these.tail
    }
    result
  }

  /** Evaluates and concatenates all elements within the `Stream` into a new
   * flattened `Stream`.
   *
   * @tparam B The type of the elements of the resulting `Stream`.
   * @return A new `Stream` of type `B` of the flattened elements of `this`
   * `Stream`.
   * @example {{{
   * val sov: Stream[Vector[Int]] = Vector(0) #:: Vector(0, 0) #:: sov.zip(sov.tail).map { n => n._1 ++ n._2 }
   * sov.flatten take 10 mkString ", "
   * // produces: "0, 0, 0, 0, 0, 0, 0, 0, 0, 0"
   * }}}
   */
  override def flatten[B](implicit asTraversable: A => /*<:<!!!*/ GenTraversableOnce[B]): Stream[B] = {
    var st: Stream[A] = this
    while (st.nonEmpty) {
      val h = asTraversable(st.head)
      if (h.isEmpty) {
        st = st.tail
      } else {
        return h.toStream #::: st.tail.flatten
      }
    }
    Stream.empty
  }

  override def view = new StreamView[A, Stream[A]] {
    protected lazy val underlying = self.repr
    override def iterator = self.iterator
    override def length = self.length
    override def apply(idx: Int) = self.apply(idx)
  }

  /** Defines the prefix of this object's `toString` representation as `Stream`.
   */
  override def stringPrefix = "Stream"

  override def equals(that: Any): Boolean =
    if (this eq that.asInstanceOf[AnyRef]) true else super.equals(that)
}

/** A specialized, extra-lazy implementation of a stream iterator, so it can
 *  iterate as lazily as it traverses the tail.
 */
final class StreamIterator[+A] private() extends AbstractIterator[A] with Iterator[A] {
  def this(self: Stream[A]) {
    this()
    these = new LazyCell(self)
  }

  // A call-by-need cell.
  class LazyCell(st: => Stream[A]) {
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
  override def toStream = {
    val result = these.v
    these = new LazyCell(Stream.empty)
    result
  }
  override def toList   = toStream.toList
}

/**
 * The object `Stream` provides helper functions to manipulate streams.
 *
 * @author Martin Odersky, Matthias Zenger
 * @version 1.1 08/08/03
 * @since   2.8
 */
object Stream extends SeqFactory[Stream] {

  /** The factory for streams.
   *  @note Methods such as map/flatMap will not invoke the `Builder` factory,
   *        but will return a new stream directly, to preserve laziness.
   *        The new stream is then cast to the factory's result type.
   *        This means that every CanBuildFrom that takes a
   *        Stream as its From type parameter must yield a stream as its result parameter.
   *        If that assumption is broken, cast errors might result.
   */
  class StreamCanBuildFrom[A] extends GenericCanBuildFrom[A]

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Stream[A]] = new StreamCanBuildFrom[A]

  /** Creates a new builder for a stream */
  def newBuilder[A]: Builder[A, Stream[A]] = new StreamBuilder[A]

  /** A builder for streams
   *  @note This builder is lazy only in the sense that it does not go downs the spine
   *        of traversables that are added as a whole. If more laziness can be achieved,
   *        this builder should be bypassed.
   */
  class StreamBuilder[A] extends LazyBuilder[A, Stream[A]] {
    def result: Stream[A] = parts.toStream flatMap (_.toStream)
  }

  object Empty extends Stream[Nothing] {
    override def isEmpty = true
    override def head = throw new NoSuchElementException("head of empty stream")
    override def tail = throw new UnsupportedOperationException("tail of empty stream")
    def tailDefined = false
  }

  /** The empty stream */
  override def empty[A]: Stream[A] = Empty

  /** A stream consisting of given elements */
  override def apply[A](xs: A*): Stream[A] = xs.toStream

  /** A wrapper class that adds `#::` for cons and `#:::` for concat as operations
   *  to streams.
   */
  class ConsWrapper[A](tl: => Stream[A]) {
    /** Construct a stream consisting of a given first element followed by elements
     *  from a lazily evaluated Stream.
     */
    def #::[B >: A](hd: B): Stream[B] = cons(hd, tl)
    /** Construct a stream consisting of the concatenation of the given stream and
     *  a lazily evaluated Stream.
     */
    def #:::[B >: A](prefix: Stream[B]): Stream[B] = prefix append tl
  }

  /** A wrapper method that adds `#::` for cons and `#:::` for concat as operations
   *  to streams.
   */
  implicit def consWrapper[A](stream: => Stream[A]): ConsWrapper[A] =
    new ConsWrapper[A](stream)

  /** An extractor that allows to pattern match streams with `#::`.
   */
  object #:: {
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] =
      if (xs.isEmpty) None
      else Some((xs.head, xs.tail))
  }

  /** An alternative way of building and matching Streams using Stream.cons(hd, tl).
   */
  object cons {

    /** A stream consisting of a given first element and remaining elements
     *  @param hd   The first element of the result stream
     *  @param tl   The remaining elements of the result stream
     */
    def apply[A](hd: A, tl: => Stream[A]) = new Cons(hd, tl)

    /** Maps a stream to its head and tail */
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] = #::.unapply(xs)
  }

  /** A lazy cons cell, from which streams are built. */
  @SerialVersionUID(-602202424901551803L)
  final class Cons[+A](hd: A, tl: => Stream[A]) extends Stream[A] {
    override def isEmpty = false
    override def head = hd
    @volatile private[this] var tlVal: Stream[A] = _
    @volatile private[this] var tlGen = tl _
    def tailDefined: Boolean = tlGen eq null
    override def tail: Stream[A] = {
      if (!tailDefined)
        synchronized {
          if (!tailDefined) {
            tlVal = tlGen()
            tlGen = null
          }
        }

      tlVal
    }

    override /*LinearSeqOptimized*/
    def sameElements[B >: A](that: GenIterable[B]): Boolean = {
      @tailrec def consEq(a: Cons[_], b: Cons[_]): Boolean = {
        if (a.head != b.head) false
        else {
          a.tail match {
            case at: Cons[_] =>
              b.tail match {
                case bt: Cons[_] => (at eq bt) || consEq(at, bt)
                case _ => false
              }
            case _ => b.tail.isEmpty
          }
        }
      }
      that match {
        case that: Cons[_] => consEq(this, that)
        case _ =>             super.sameElements(that)
      }
    }
  }

  /** An infinite stream that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the stream
   *  @param f     the function that's repeatedly applied
   *  @return      the stream returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: A)(f: A => A): Stream[A] = cons(start, iterate(f(start))(f))

  override def iterate[A](start: A, len: Int)(f: A => A): Stream[A] =
    iterate(start)(f) take len

  /**
   * Create an infinite stream starting at `start` and incrementing by
   * step `step`.
   *
   * @param start the start value of the stream
   * @param step the increment value of the stream
   * @return the stream starting at value `start`.
   */
  def from(start: Int, step: Int): Stream[Int] =
    cons(start, from(start+step, step))

  /**
   * Create an infinite stream starting at `start` and incrementing by `1`.
   *
   * @param start the start value of the stream
   * @return the stream starting at value `start`.
   */
  def from(start: Int): Stream[Int] = from(start, 1)

  /**
   * Create an infinite stream containing the given element expression (which
   * is computed for each occurrence).
   *
   * @param elem the element composing the resulting stream
   * @return the stream containing an infinite number of elem
   */
  def continually[A](elem: => A): Stream[A] = cons(elem, continually(elem))

  override def fill[A](n: Int)(elem: => A): Stream[A] =
    if (n <= 0) Empty else cons(elem, fill(n-1)(elem))

  override def tabulate[A](n: Int)(f: Int => A): Stream[A] = {
    def loop(i: Int): Stream[A] =
      if (i >= n) Empty else cons(f(i), loop(i+1))
    loop(0)
  }

  override def range[T: Integral](start: T, end: T, step: T): Stream[T] = {
    val num = implicitly[Integral[T]]
    import num._

    if (if (step < zero) start <= end else end <= start) Empty
    else cons(start, range(start + step, end, step))
  }

  private[immutable] def filteredTail[A](stream: Stream[A], p: A => Boolean, isFlipped: Boolean) = {
    cons(stream.head, stream.tail.filterImpl(p, isFlipped))
  }

  private[immutable] def collectedTail[A, B, That](head: B, stream: Stream[A], pf: PartialFunction[A, B], bf: CanBuildFrom[Stream[A], B, That]) = {
    cons(head, stream.tail.collect(pf)(bf).asInstanceOf[Stream[B]])
  }

  /** An implementation of `FilterMonadic` allowing GC of the filtered-out elements of
    * the `Stream` as it is processed.
    *
    * Because this is not an inner class of `Stream` with a reference to the original
    * head, it is now possible for GC to collect any leading and filtered-out elements
    * which do not satisfy the filter, while the tail is still processing (see scala/bug#8990).
    */
  private[immutable] final class StreamWithFilter[A](sl: => Stream[A], p: A => Boolean) extends FilterMonadic[A, Stream[A]] {
    private var s = sl                                              // set to null to allow GC after filtered
    private lazy val filtered = { val f = s filter p; s = null; f } // don't set to null if throw during filter

    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
      filtered map f

    def flatMap[B, That](f: A => scala.collection.GenTraversableOnce[B])(implicit bf: CanBuildFrom[Stream[A], B, That]): That =
      filtered flatMap f

    def foreach[U](f: A => U): Unit =
      filtered foreach f

    def withFilter(q: A => Boolean): FilterMonadic[A, Stream[A]] =
      new StreamWithFilter[A](filtered, q)
  }

}
