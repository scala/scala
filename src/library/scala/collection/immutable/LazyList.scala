package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.{StringBuilder => JStringBuilder}

import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.SerializeEnd
import scala.collection.mutable.StringBuilder
import scala.language.higherKinds

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
  *  @tparam A    the type of the elements contained in this lazy list.
  *
  *  @author Martin Odersky, Matthias Zenger
  *  @since   2.8
  *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#streams "Scala's Collection Library overview"]]
  *  section on `Streams` for more information.

  *  @define Coll `LazyList`
  *  @define coll lazy list
  *  @define orderDependent
  *  @define orderDependentFold
  */
@SerialVersionUID(3L)
sealed abstract class LazyList[+A] extends AbstractSeq[A] with LinearSeq[A] with LazyListOps[A, LazyList, LazyList[A]] {
  override def iterableFactory: LazyListFactory[LazyList] = LazyList

  protected def cons[T](hd: => T, tl: => LazyList[T]): LazyList[T] = new LazyList.Cons(hd, tl)

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

  override protected[this] def writeReplace(): AnyRef =
    if(headDefined && tailDefined) new LazyListOps.SerializationProxy[A, LazyList](this) else this

  override protected[this] def className = "LazyList"
}

sealed private[immutable] trait LazyListOps[+A, +CC[+X] <: LinearSeq[X] with LazyListOps[X, CC, CC[X]], +C <: CC[A] with LazyListOps[A, CC, C]]
  extends LinearSeqOps[A, CC, C] {

  def iterableFactory: LazyListFactory[CC]

  def tail: C

  protected def cons[T](hd: => T, tl: => CC[T] @uncheckedVariance): CC[T]

  /** Forces evaluation of the whole `LazyList` and returns it.
    *
    * @note Often we use `LazyList`s to represent an infinite set or series.  If
    * that's the case for your particular `LazyList` then this function will never
    * return and will probably crash the VM with an `OutOfMemory` exception.
    * This function will not hang on a finite cycle, however.
    *
    *  @return The fully realized `LazyList`.
    */
  def force: this.type

  /** The stream resulting from the concatenation of this stream with the argument stream.
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    */
  def lazyAppendedAll[B >: A](suffix: => collection.IterableOnce[B]): CC[B] =
    if (isEmpty) iterableFactory.from(suffix) else cons[B](head, tail.lazyAppendedAll(suffix))

  override def equals(that: Any): Boolean =
    if (this eq that.asInstanceOf[AnyRef]) true else super.equals(that)

  override def scanLeft[B](z: B)(op: (B, A) => B): CC[B] =
    if (isEmpty) z +: iterableFactory.empty
    else cons(z, tail.scanLeft(op(z, head))(op))

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
      var left: CC[A] = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  override def partition(p: A => Boolean): (C, C) = (filter(p(_)), filterNot(p(_)))

  override def filter(pred: A => Boolean): C = filterImpl(pred, isFlipped = false)

  override def filterNot(pred: A => Boolean): C = filterImpl(pred, isFlipped = true)

  private[immutable] def filterImpl(p: A => Boolean, isFlipped: Boolean): C = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest: CC[A] = coll
    while (rest.nonEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    (if (rest.nonEmpty) iterableFactory.filteredTail(rest, p, isFlipped)
    else iterableFactory.empty).asInstanceOf[C]
  }

  /** A `collection.WithFilter` which allows GC of the head of stream during processing */
  @noinline // Workaround scala/bug#9137, see https://github.com/scala/scala/pull/4284#issuecomment-73180791
  override final def withFilter(p: A => Boolean): collection.WithFilter[A, CC] =
    iterableFactory.withFilter(coll, p)

  override final def prepended[B >: A](elem: B): CC[B] = cons(elem, coll)

  override final def map[B](f: A => B): CC[B] =
    if (isEmpty) iterableFactory.empty
    else cons(f(head), tail.map(f))

  override final def collect[B](pf: PartialFunction[A, B]): CC[B] = {
    // this implementation avoids:
    // 1) stackoverflows (could be achieved with tailrec, too)
    // 2) out of memory errors for big lazy lists (`this` reference can be eliminated from the stack)
    var rest: CC[A] = coll

    // Avoids calling both `pf.isDefined` and `pf.apply`.
    var newHead: B = null.asInstanceOf[B]
    val runWith = pf.runWith((b: B) => newHead = b)

    while (rest.nonEmpty && !runWith(rest.head)) rest = rest.tail

    //  without the call to the companion object, a thunk is created for the tail of the new lazy list,
    //  and the closure of the thunk will reference `this`
    if (rest.isEmpty) iterableFactory.empty
    else iterableFactory.collectedTail(newHead, rest, pf)
  }

  // optimisations are not for speed, but for functionality
  // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
  override final def flatMap[B](f: A => IterableOnce[B]): CC[B] =
    if (isEmpty) iterableFactory.empty
    else {
      // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
      var nonEmptyPrefix: CC[A] = coll
      var prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
        nonEmptyPrefix = nonEmptyPrefix.tail
        if(!nonEmptyPrefix.isEmpty)
          prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      }

      if (nonEmptyPrefix.isEmpty) iterableFactory.empty
      else prefix.lazyAppendedAll(nonEmptyPrefix.tail.flatMap(f))
    }

  override final def zip[B](that: collection.Iterable[B]): CC[(A, B)] =
    if (this.isEmpty || that.isEmpty) iterableFactory.empty
    else cons[(A, B)]((this.head, that.head), this.tail.zip(that.tail))

  override final def zipWithIndex: CC[(A, Int)] = this.zip(LazyList.from(0))

  protected def headDefined: Boolean
  protected def tailDefined: Boolean

  /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
    *  The written text begins with the string `start` and ends with the string `end`.
    *  Inside, the string representations (w.r.t. the method `toString`)
    *  of all elements of this $coll are separated by the string `sep`.
    *
    * Undefined elements are represented with `"_"`, an undefined tail is represented with `"?"`,
    * and cycles are represented with `"..."`.
    *
    *  @param  b    the string builder to which elements are appended.
    *  @param start the starting string.
    *  @param sep   the separator string.
    *  @param end   the ending string.
    *  @return      the string builder `b` to which elements were appended.
    */
  override def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    force
    addStringNoForce(sb.underlying, start, sep, end)
    sb
  }

  private[this] def addStringNoForce(b: JStringBuilder, start: String, sep: String, end: String): JStringBuilder = {
    b.append(start)
    if (nonEmpty) {
      if (headDefined) b.append(head) else b.append('_')
      var cursor = this
      def appendCursorElement() = {
        b.append(sep)
        if (cursor.headDefined) b.append(cursor.head) else b.append('_')
      }
      if (tailDefined) {  // If tailDefined, also !isEmpty
        var scout = tail
        if (cursor ne scout) {
          cursor = scout
          if (scout.tailDefined) {
            scout = scout.tail
            // Use 2x 1x iterator trick for cycle detection; slow iterator can add strings
            while ((cursor ne scout) && scout.tailDefined) {
              appendCursorElement()
              cursor = cursor.tail
              scout = scout.tail
              if (scout.tailDefined) scout = scout.tail
            }
          }
        }
        if (!scout.tailDefined) {  // Not a cycle, scout hit an end
          while (cursor ne scout) {
            appendCursorElement()
            cursor = cursor.tail
          }
          if (cursor.nonEmpty) {
            appendCursorElement()
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
            appendCursorElement()
            cursor = cursor.tail
          }
          while (cursor ne scout) {
            appendCursorElement()
            cursor = cursor.tail
          }
        }
      }
      if (cursor.nonEmpty) {
        // Either undefined or cyclic; we can check with tailDefined
        if (!cursor.tailDefined) b.append(sep).append('?')
        else b.append(sep).append("...")
      }
    }
    b.append(end)
  }

  protected[this] def className: String

  /**
    * @return a string representation of this collection. Undefined elements are
    *         represented with `"_"`, an undefined tail is represented with `"?"`,
    *         and cycles are represented with `"..."`
    *
    *         Examples:
    *
    *           - `"LazyList(_, ?)"`, a non-empty lazy list, whose head has not been
    *             evaluated ;
    *           - `"LazyList(_, 1, _, ?)"`, a lazy list with at least three elements,
    *             the second one has been evaluated ;
    *           - `"LazyList(1, 2, 3, ...)"`, an infinite lazy list that contains
    *             a cycle at the fourth element.
    */
  override def toString = addStringNoForce(new JStringBuilder(className), "(", ", ", ")").toString
}

private[immutable] object LazyListOps {

  /** This serialization proxy is used for LazyLists and Streams which start with a sequence of evaluated cons cells.
    * The forced sequence is serialized in a compact, sequential format, followed by the unevaluated tail, which uses
    * standard Java serialization to store the complete structure of unevaluated thunks. This allows the serialization
    * of long evaluated streams without exhausting the stack through recursive serialization of cons cells.
    */
  @SerialVersionUID(3L)
  class SerializationProxy[A, CC[+X] <: LinearSeq[X] with LazyListOps[X, CC, CC[X]]](@transient protected var coll: CC[A]) extends Serializable {

    private[this] def writeObject(out: ObjectOutputStream): Unit = {
      out.defaultWriteObject()
      var these = coll
      while(these.headDefined && these.tailDefined) {
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
      val tail = in.readObject().asInstanceOf[CC[A]]
      coll = (init ++: tail)
    }

    protected[this] def readResolve(): Any = coll
  }
}

sealed private[immutable] trait LazyListFactory[+CC[+X] <: LinearSeq[X] with LazyListOps[X, CC, CC[X]]] extends SeqFactory[CC] {

  protected def newCons[T](hd: => T, tl: => CC[T] @uncheckedVariance): CC[T]

  private[immutable] def withFilter[A](l: CC[A] @uncheckedVariance, p: A => Boolean): collection.WithFilter[A, CC] =
    new WithFilter[A](() => l, p)

  private[this] final class WithFilter[A](l: () => CC[A], p: A => Boolean) extends collection.WithFilter[A, CC] {
    private[this] var s = l                                                // set to null to allow GC after filtered
    private[this] lazy val filtered: CC[A] = { val f = s().filter(p); s = null; f } // don't set to null if throw during filter
    def map[B](f: A => B): CC[B] = filtered.map(f)
    def flatMap[B](f: A => IterableOnce[B]): CC[B] = filtered.flatMap(f)
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def withFilter(q: A => Boolean): collection.WithFilter[A, CC] = new WithFilter(() => filtered, q)
  }

  /** An infinite LazyList that repeatedly applies a given function to a start value.
    *
    *  @param start the start value of the LazyList
    *  @param f     the function that's repeatedly applied
    *  @return      the LazyList returning the infinite sequence of values `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: => A)(f: A => A): CC[A] = {
    lazy val head = start
    newCons(head, iterate(f(head))(f))
  }

  /**
    * Create an infinite LazyList starting at `start` and incrementing by
    * step `step`.
    *
    * @param start the start value of the LazyList
    * @param step the increment value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int, step: Int): CC[Int] =
    newCons(start, from(start + step, step))

  /**
    * Create an infinite LazyList starting at `start` and incrementing by `1`.
    *
    * @param start the start value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int): CC[Int] = from(start, 1)

  /**
    * Create an infinite LazyList containing the given element expression (which
    * is computed for each occurrence).
    *
    * @param elem the element composing the resulting LazyList
    * @return the LazyList containing an infinite number of elem
    */
  def continually[A](elem: => A): CC[A] = newCons(elem, continually(elem))

  def newBuilder[A]: Builder[A, CC[A]] = ArrayBuffer.newBuilder[A].mapResult(array => from(array))

  private[immutable] def filteredTail[A](lazyList: CC[A] @uncheckedVariance, p: A => Boolean, isFlipped: Boolean) = {
    newCons(lazyList.head, lazyList.tail.filterImpl(p, isFlipped))
  }

  private[immutable] def collectedTail[A, B](head: B, stream: CC[A] @uncheckedVariance, pf: PartialFunction[A, B]) = {
    newCons(head, stream.tail.collect(pf))
  }

}

/**
  * $factoryInfo
  * @define coll lazy list
  * @define Coll `LazyList`
  */
@SerialVersionUID(3L)
object LazyList extends LazyListFactory[LazyList] {

  protected def newCons[T](hd: => T, tl: => LazyList[T]): LazyList[T] = new LazyList.Cons(hd, tl)

  @SerialVersionUID(3L)
  object Empty extends LazyList[Nothing] {
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException("head of empty lazy list")
    override def tail: LazyList[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")
    def force: this.type = this
    override def knownSize: Int = 0
    override def iterator: Iterator[Nothing] = Iterator.empty
    protected def tailDefined: Boolean = false
    protected def headDefined: Boolean = false
  }

  @SerialVersionUID(3L)
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
    def force: this.type = {
      // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
      var these, those: LazyList[A] = this
      if (!these.isEmpty) {
        these.head
        these = these.tail
      }
      while (those ne these) {
        if (these.isEmpty) return this
        these.head
        these = these.tail
        if (these.isEmpty) return this
        these.head
        these = these.tail
        if (these eq those) return this
        those = those.tail
      }
      this
    }

    protected def tailDefined: Boolean = tlEvaluated
    protected def headDefined: Boolean = hdEvaluated
  }

  /** An alternative way of building and matching lazy lists using LazyList.cons(hd, tl).
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

  implicit def toDeferrer[A](l: => LazyList[A]): Deferrer[A] = new Deferrer[A](() => l)

  final class Deferrer[A] private[LazyList] (private val l: () => LazyList[A]) extends AnyVal {
    /** Construct a LazyList consisting of a given first element followed by elements
      *  from another LazyList.
      */
    def #:: [B >: A](elem: => B): LazyList[B] = newCons(elem, l())
    /** Construct a LazyList consisting of the concatenation of the given LazyList and
      *  another LazyList.
      */
    def #:::[B >: A](prefix: LazyList[B]): LazyList[B] = prefix lazyAppendedAll l()
  }

  object #:: {
    def unapply[A](s: LazyList[A]): Option[(A, LazyList[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  def from[A](coll: collection.IterableOnce[A]): LazyList[A] = coll match {
    case coll: LazyList[A] => coll
    case _ if coll.knownSize == 0 => empty[A]
    case _ => fromIterator(coll.iterator)
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

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}

@deprecated("Use LazyList (which has a lazy head and tail) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
sealed abstract class Stream[+A] extends AbstractSeq[A] with LinearSeq[A] with LazyListOps[A, Stream, Stream[A]] {
  override def iterableFactory: LazyListFactory[Stream] = Stream

  override protected[this] def className: String = "Stream"

  protected def cons[T](hd: => T, tl: => Stream[T]): Stream[T] = new Stream.Cons(hd, tl)

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

  override def take(n: Int): Stream[A] = {
    if (n <= 0 || isEmpty) Stream.empty
    else if (n == 1) new Stream.Cons(head, Stream.empty)
    else new Stream.Cons(head, tail.take(n - 1))
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

  @deprecated("The `append` operation has been renamed `lazyAppendedAll`", "2.13.0")
  @inline final def append[B >: A](suffix: IterableOnce[B]): Stream[B] = lazyAppendedAll(suffix)

  override protected[this] def writeReplace(): AnyRef =
    if(headDefined && tailDefined) new LazyListOps.SerializationProxy[A, Stream](this) else this

  /** Prints elements of this stream one by one, separated by commas. */
  @deprecated(message = """Use print(stream.force.mkString(", ")) instead""", since = "2.13.0")
  @inline def print(): Unit = Console.print(this.force.mkString(", "))

  /** Prints elements of this stream one by one, separated by `sep`.
    *  @param sep   The separator string printed between consecutive elements.
    */
  @deprecated(message = "Use print(stream.force.mkString(sep)) instead", since = "2.13.0")
  @inline def print(sep: String): Unit = Console.print(this.force.mkString(sep))

}

@deprecated("Use LazyList (which has a lazy head and tail) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
object Stream extends LazyListFactory[Stream] {

  protected def newCons[T](hd: => T, tl: => Stream[T]): Stream[T] = new Stream.Cons(hd, tl)

  //@SerialVersionUID(3L) //TODO Putting an annotation on Stream.empty causes a cyclic dependency in unpickling
  object Empty extends Stream[Nothing] {
    override def isEmpty: Boolean = true
    override def head: Nothing = throw new NoSuchElementException("head of empty stream")
    override def tail: Stream[Nothing] = throw new UnsupportedOperationException("tail of empty stream")
    /** Forces evaluation of the whole `Stream` and returns it.
      *
      * @note Often we use `Stream`s to represent an infinite set or series.  If
      * that's the case for your particular `Stream` then this function will never
      * return and will probably crash the VM with an `OutOfMemory` exception.
      * This function will not hang on a finite cycle, however.
      *
      *  @return The fully realized `Stream`.
      */
    def force: this.type = this
    override def knownSize: Int = 0
    override def iterator: Iterator[Nothing] = Iterator.empty
    protected def headDefined: Boolean = false
    protected def tailDefined: Boolean = false
  }

  @SerialVersionUID(3L)
  final class Cons[A](override val head: A, tl: => Stream[A]) extends Stream[A] {
    private[this] var tlEvaluated: Boolean = false
    override def isEmpty: Boolean = false
    override lazy val tail: Stream[A] = {
      tlEvaluated = true
      tl
    }
    protected def headDefined: Boolean = true
    protected def tailDefined: Boolean = tlEvaluated
    /** Forces evaluation of the whole `Stream` and returns it.
      *
      * @note Often we use `Stream`s to represent an infinite set or series.  If
      * that's the case for your particular `Stream` then this function will never
      * return and will probably crash the VM with an `OutOfMemory` exception.
      * This function will not hang on a finite cycle, however.
      *
      *  @return The fully realized `Stream`.
      */
    def force: this.type = {
      // Use standard 2x 1x iterator trick for cycle detection ("those" is slow one)
      var these, those: Stream[A] = this
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

  }

  /** An alternative way of building and matching Streams using Stream.cons(hd, tl).
    */
  object cons {
    /** A stream consisting of a given first element and remaining elements
      *  @param hd   The first element of the result stream
      *  @param tl   The remaining elements of the result stream
      */
    def apply[A](hd: A, tl: => Stream[A]): Stream[A] = new Cons(hd, tl)

    /** Maps a stream to its head and tail */
    def unapply[A](xs: Stream[A]): Option[(A, Stream[A])] = #::.unapply(xs)
  }

  implicit def toDeferrer[A](l: => Stream[A]): Deferrer[A] = new Deferrer[A](() => l)

  final class Deferrer[A] private[Stream] (private val l: () => Stream[A]) extends AnyVal {
    /** Construct a Stream consisting of a given first element followed by elements
      *  from another Stream.
      */
    def #:: [B >: A](elem: B): Stream[B] = new Cons(elem, l())
    /** Construct a Stream consisting of the concatenation of the given Stream and
      *  another Stream.
      */
    def #:::[B >: A](prefix: Stream[B]): Stream[B] = prefix lazyAppendedAll l()
  }

  object #:: {
    def unapply[A](s: Stream[A]): Option[(A, Stream[A])] =
      if (s.nonEmpty) Some((s.head, s.tail)) else None
  }

  def from[A](coll: collection.IterableOnce[A]): Stream[A] = coll match {
    case coll: Stream[A] => coll
    case _ => fromIterator(coll.iterator)
  }

  /**
    * @return A `Stream[A]` that gets its elements from the given `Iterator`.
    *
    * @param it Source iterator
    * @tparam A type of elements
    */
  // Note that the resulting `Stream` will be effectively iterable more than once because
  // `Stream` memoizes its elements
  def fromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) {
      new Stream.Cons(it.next(), fromIterator(it))
    } else Stream.Empty

  def empty[A]: Stream[A] = Empty

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}
