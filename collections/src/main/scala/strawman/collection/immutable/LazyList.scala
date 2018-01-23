package strawman
package collection
package immutable

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, AnyRef, Boolean, Int, None, NoSuchElementException, noinline, Nothing, Option, PartialFunction, Some, StringContext, Unit, UnsupportedOperationException, deprecated}
import scala.Predef.String
import scala.annotation.tailrec

/**  The class `LazyList` implements lazy lists where elements
  *  are only evaluated when they are needed. Here is an example:
  *
  *  {{{
  *  import scala.math.BigInt
  *  object Main extends App {
  *
  *    lazy val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
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
  *    lazy val fibs: LazyList[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(
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
  *  they are methods on `LazyList`, and a lazy list holds its own head. For
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
  *  val it1 = lazylist1.iterator()
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
  *  val it2 = stream2.iterator()
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
  *  @tparam A    the type of the elements contained in this stream.
  *
  *  @author Martin Odersky, Matthias Zenger
  *  @version 1.1 08/08/03
  *  @since   2.8
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#streams "Scala's Collection Library overview"]]
  *  section on `Streams` for more information.

  *  @define Coll `LazyList`
  *  @define coll lazy list
  *  @define orderDependent
  *  @define orderDependentFold
  */
sealed abstract class LazyList[+A]
  extends LinearSeq[A]
     with LinearSeqOps[A, LazyList, LazyList[A]] {

  /** Force the evaluation of both the head and the tail of this `LazyList` */
  def force: LazyList.Evaluated[A]

  override def nonEmpty: Boolean = !isEmpty

  def iterableFactory: SeqFactory[LazyList] = LazyList

  protected[this] def fromSpecificIterable(coll: collection.Iterable[A]): LazyList[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, LazyList[A]] =
    IndexedSeq.newBuilder().mapResult(_.to(LazyList))

  /** The stream resulting from the concatenation of this stream with the argument stream.
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    */
  def lazyAppendAll[B >: A](suffix: => collection.IterableOnce[B]): LazyList[B] =
    if (isEmpty) LazyList.fromIterator(suffix.iterator()) else LazyList.cons(head, tail.lazyAppendAll(suffix))

  @deprecated("append has been renamed to lazyAppendAll", "2.13.0")
  def append[B >: A](rest: => collection.IterableOnce[B]): LazyList[B] = lazyAppendAll(rest)

  override def className = "LazyList"

  override def equals(that: Any): Boolean =
    if (this eq that.asInstanceOf[AnyRef]) true else super.equals(that)

  override def sameElements[B >: A](that: IterableOnce[B]): Boolean = {
    @tailrec def lazyListEq(a: LazyList[_], b: LazyList[_]): Boolean =
      if (a eq b) true else {
        (a.force, b.force) match {
          case (Some((ah, at)), Some((bh, bt))) => (ah == bh) && lazyListEq(at, bt)
          case (None, None) => true
          case _ => false
        }
      }
    that match {
      case that: LazyList[_] => lazyListEq(this, that)
      case _ => super.sameElements(that)
    }
  }

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
  override final def foreach[U](f: A => U): Unit = {
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

  override def scanLeft[B](z: B)(op: (B, A) => B): LazyList[B] =
    if (isEmpty) z +: LazyList.empty
    else LazyList.cons(z, tail.scanLeft(op(z, head))(op))

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

  override def partition(p: A => Boolean): (LazyList[A], LazyList[A]) = (filter(p(_)), filterNot(p(_)))

  override def filter(pred: A => Boolean): LazyList[A] = filterImpl(pred, isFlipped = false)

  override def filterNot(pred: A => Boolean): LazyList[A] = filterImpl(pred, isFlipped = true)

  private[immutable] def filterImpl(p: A => Boolean, isFlipped: Boolean): LazyList[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest = this
    while (rest.nonEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) LazyList.filteredTail(rest, p, isFlipped)
    else LazyList.Empty
  }

  /** A FilterMonadic which allows GC of the head of stream during processing */
  @noinline // Workaround scala/bug#9137, see https://github.com/scala/scala/pull/4284#issuecomment-73180791
  override final def withFilter(p: A => Boolean): collection.WithFilter[A, LazyList] =
    new LazyList.WithFilter(this, p)

  override final def prepended[B >: A](elem: B): LazyList[B] = LazyList.cons(elem, this)

  override final def map[B](f: A => B): LazyList[B] =
    if (isEmpty) LazyList.empty
    else LazyList.cons(f(head), tail.map(f))

  override final def collect[B](pf: PartialFunction[A, B]): LazyList[B] = {
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
    if (rest.isEmpty) LazyList.Empty
    else LazyList.collectedTail(newHead, rest, pf)
  }

  // optimisations are not for speed, but for functionality
  // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
  override final def flatMap[B](f: A => IterableOnce[B]): LazyList[B] =
    if (isEmpty) LazyList.Empty
    else {
      // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
      var nonEmptyPrefix = this
      var prefix = LazyList.fromIterator(f(nonEmptyPrefix.head).iterator())
      while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
        nonEmptyPrefix = nonEmptyPrefix.tail
        if(!nonEmptyPrefix.isEmpty)
          prefix = LazyList.fromIterator(f(nonEmptyPrefix.head).iterator())
      }

      if (nonEmptyPrefix.isEmpty) LazyList.empty
      else prefix.lazyAppendAll(nonEmptyPrefix.tail.flatMap(f))
    }

  override final def zip[B](that: collection.Iterable[B]): LazyList[(A, B)] =
    if (this.isEmpty || that.isEmpty) LazyList.empty
    else LazyList.cons((this.head, that.head), this.tail.zip(that.tail))

  override final def zipWithIndex: LazyList[(A, Int)] = this.zip(LazyList.from(0))

}

/**
  * $factoryInfo
  * @define coll lazy list
  * @define Coll `LazyList`
  */
object LazyList extends SeqFactory[LazyList] {

  type Evaluated[+A] = Option[(A, LazyList[A])]

  object Empty extends LazyList[Nothing] {
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException("head of empty lazy list")
    override def tail: LazyList[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")
    def force: Evaluated[Nothing] = None
    override def toString: String = "Empty"
  }

  final class Cons[A](hd: => A, tl: => LazyList[A]) extends LazyList[A] {
    private[this] var hdEvaluated: Boolean = false
    private[this] var tlEvaluated: Boolean = false
    override def isEmpty: Boolean = false
    override lazy val head: A = {
      hdEvaluated = true
      hd
    }
    override lazy val tail: LazyList[A] = {
      tlEvaluated = true
      tl
    }
    def force: Evaluated[A] = Some((head, tail))
    override def toString: String =
      if (hdEvaluated) s"$head #:: ${if (tlEvaluated) tail.toString else "?"}"
      else "LazyList(?)"
  }

  /** An alternative way of building and matching Streams using LazyList.cons(hd, tl).
    */
  object cons {
    /** A lazy list consisting of a given first element and remaining elements
      *  @param hd   The first element of the result lazy list
      *  @param tl   The remaining elements of the result lazy list
      */
    def apply[A](hd: => A, tl: => LazyList[A]): LazyList[A] = new Cons(hd, tl)

    /** Maps a lazy list to its head and tail */
    def unapply[A](xs: LazyList[A]): Option[(A, LazyList[A])] = #::.unapply(xs)
  }

  implicit class Deferrer[A](l: => LazyList[A]) {
    /** Construct a LazyList consisting of a given first element followed by elements
      *  from another LazyList.
      */
    def #:: [B >: A](elem: => B): LazyList[B] = cons(elem, l)
    /** Construct a LazyList consisting of the concatenation of the given LazyList and
      *  another LazyList.
      */
    def #:::[B >: A](prefix: LazyList[B]): LazyList[B] = prefix lazyAppendAll l
  }

  object #:: {
    def unapply[A](s: LazyList[A]): Evaluated[A] = s.force
  }

  def from[A](coll: collection.IterableOnce[A]): LazyList[A] = coll match {
    case coll: LazyList[A] => coll
    case _ => fromIterator(coll.iterator())
  }

  /**
    * @return A `LazyList[A]` that gets its elements from the given `Iterator`.
    *
    * @param it Source iterator
    * @tparam A type of elements
    */
  // Note that the resulting `LazyList` will be effectively iterable more than once because
  // `LazyList` memoizes its elements
  def fromIterator[A](it: Iterator[A]): LazyList[A] =
    if (it.hasNext) {
      // Be sure that `it.next()` is called even when the `head`
      // of our constructed lazy list is not evaluated (e.g. when one calls `drop`).
      lazy val evaluatedElem = it.next()
      new LazyList.Cons(evaluatedElem, { evaluatedElem; fromIterator(it) })
    } else LazyList.Empty

  def empty[A]: LazyList[A] = Empty

  /** An infinite LazyList that repeatedly applies a given function to a start value.
   *
   *  @param start the start value of the LazyList
   *  @param f     the function that's repeatedly applied
   *  @return      the LazyList returning the infinite sequence of values `start, f(start), f(f(start)), ...`
   */
  def iterate[A](start: => A)(f: A => A): LazyList[A] = cons(start, iterate(f(start))(f))

  /**
    * Create an infinite LazyList starting at `start` and incrementing by
    * step `step`.
    *
    * @param start the start value of the LazyList
    * @param step the increment value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int, step: Int): LazyList[Int] =
    cons(start, from(start + step, step))

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
  def continually[A](elem: => A): LazyList[A] = cons(elem, continually(elem))

  /**
    * @return a LazyList by using a function `f` producing elements of
    *         type `A` and updating an internal state `S`.
    * @param init State initial value
    * @param f    Computes the next element (or returns `None` to signal
    *             the end of the collection)
    * @tparam A   Type of the elements
    * @tparam S   Type of the internal state
    */
  def unfold[A, S](init: S)(f: S => Option[(A, S)]): LazyList[A] = {
    def loop(s: S): LazyList[A] = {
      f(s).fold(empty[A])(as => cons(as._1, loop(as._2)))
    }
    loop(init)
  }

  def newBuilder[A](): Builder[A, LazyList[A]] = ArrayBuffer.newBuilder[A]().mapResult(array => from(array))

  private[immutable] def filteredTail[A](lazyList: LazyList[A], p: A => Boolean, isFlipped: Boolean) = {
    cons(lazyList.head, lazyList.tail.filterImpl(p, isFlipped))
  }

  private[immutable] def collectedTail[A, B](head: B, stream: LazyList[A], pf: PartialFunction[A, B]) = {
    cons(head, stream.tail.collect(pf))
  }

  private[immutable] class WithFilter[A](l: LazyList[A], p: A => Boolean) extends collection.WithFilter[A, LazyList] {
    private[this] var s = l                                                // set to null to allow GC after filtered
    private[this] lazy val filtered = { val f = s.filter(p); s = null; f } // don't set to null if throw during filter
    def map[B](f: A => B): LazyList[B] = filtered.map(f)
    def flatMap[B](f: A => IterableOnce[B]): LazyList[B] = filtered.flatMap(f)
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter(filtered, q)
  }

}
