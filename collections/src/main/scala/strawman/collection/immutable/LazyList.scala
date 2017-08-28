package strawman
package collection
package immutable

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, AnyRef, Boolean, Int, None, NoSuchElementException, noinline, Nothing, Option, PartialFunction, Some, StringContext, Unit, UnsupportedOperationException}
import scala.Predef.String
import scala.annotation.tailrec

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

  override final def zip[B](xs: collection.Iterable[B]): LazyList[(A, B)] =
    if (this.isEmpty || xs.isEmpty) LazyList.empty
    else LazyList.cons((this.head, xs.head), this.tail.zip(xs.tail))

  override final def zipWithIndex: LazyList[(A, Int)] = this.zip(LazyList.from(0))

}

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
