package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.SerializeEnd
import scala.collection.mutable.{ArrayBuffer, StringBuilder}

@deprecated("Use LazyList (which is fully lazy) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
sealed abstract class Stream[+A] extends AbstractSeq[A] with LinearSeq[A] with LinearSeqOps[A, Stream, Stream[A]] {
  def tail: Stream[A]

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

  override def iterableFactory: SeqFactory[Stream] = Stream

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
    if(headDefined && tailDefined) new Stream.SerializationProxy[A](this) else this

  /** Prints elements of this stream one by one, separated by commas. */
  @deprecated(message = """Use print(stream.force.mkString(", ")) instead""", since = "2.13.0")
  @inline def print(): Unit = Console.print(this.force.mkString(", "))

  /** Prints elements of this stream one by one, separated by `sep`.
    *  @param sep   The separator string printed between consecutive elements.
    */
  @deprecated(message = "Use print(stream.force.mkString(sep)) instead", since = "2.13.0")
  @inline def print(sep: String): Unit = Console.print(this.force.mkString(sep))

  /** The stream resulting from the concatenation of this stream with the argument stream.
    *
    * @param suffix The collection that gets appended to this lazy list
    * @return The lazy list containing elements of this lazy list and the iterable object.
    */
  def lazyAppendedAll[B >: A](suffix: => collection.IterableOnce[B]): Stream[B] =
    if (isEmpty) iterableFactory.from(suffix) else cons[B](head, tail.lazyAppendedAll(suffix))

  override def equals(that: Any): Boolean =
    if (this eq that.asInstanceOf[AnyRef]) true else super.equals(that)

  override def scanLeft[B](z: B)(op: (B, A) => B): Stream[B] =
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
      var left: Stream[A] = this.tail
      while (!left.isEmpty) {
        reducedRes = f(reducedRes, left.head)
        left = left.tail
      }
      reducedRes
    }
  }

  override def partition(p: A => Boolean): (Stream[A], Stream[A]) = (filter(p(_)), filterNot(p(_)))

  override def filter(pred: A => Boolean): Stream[A] = filterImpl(pred, isFlipped = false)

  override def filterNot(pred: A => Boolean): Stream[A] = filterImpl(pred, isFlipped = true)

  private[immutable] def filterImpl(p: A => Boolean, isFlipped: Boolean): Stream[A] = {
    // optimization: drop leading prefix of elems for which f returns false
    // var rest = this dropWhile (!p(_)) - forget DRY principle - GC can't collect otherwise
    var rest: Stream[A] = coll
    while (rest.nonEmpty && p(rest.head) == isFlipped) rest = rest.tail
    // private utility func to avoid `this` on stack (would be needed for the lazy arg)
    if (rest.nonEmpty) Stream.filteredTail(rest, p, isFlipped)
    else iterableFactory.empty
  }

  /** A `collection.WithFilter` which allows GC of the head of stream during processing */
  @noinline // Workaround scala/bug#9137, see https://github.com/scala/scala/pull/4284#issuecomment-73180791
  override final def withFilter(p: A => Boolean): collection.WithFilter[A, Stream] =
    Stream.withFilter(coll, p)

  override final def prepended[B >: A](elem: B): Stream[B] = cons(elem, coll)

  override final def map[B](f: A => B): Stream[B] =
    if (isEmpty) iterableFactory.empty
    else cons(f(head), tail.map(f))

  override final def collect[B](pf: PartialFunction[A, B]): Stream[B] = {
    // this implementation avoids:
    // 1) stackoverflows (could be achieved with tailrec, too)
    // 2) out of memory errors for big lazy lists (`this` reference can be eliminated from the stack)
    var rest: Stream[A] = coll

    // Avoids calling both `pf.isDefined` and `pf.apply`.
    var newHead: B = null.asInstanceOf[B]
    val runWith = pf.runWith((b: B) => newHead = b)

    while (rest.nonEmpty && !runWith(rest.head)) rest = rest.tail

    //  without the call to the companion object, a thunk is created for the tail of the new lazy list,
    //  and the closure of the thunk will reference `this`
    if (rest.isEmpty) iterableFactory.empty
    else Stream.collectedTail(newHead, rest, pf)
  }

  // optimisations are not for speed, but for functionality
  // see tickets #153, #498, #2147, and corresponding tests in run/ (as well as run/stream_flatmap_odds.scala)
  override final def flatMap[B](f: A => IterableOnce[B]): Stream[B] =
    if (isEmpty) iterableFactory.empty
    else {
      // establish !prefix.isEmpty || nonEmptyPrefix.isEmpty
      var nonEmptyPrefix: Stream[A] = coll
      var prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      while (!nonEmptyPrefix.isEmpty && prefix.isEmpty) {
        nonEmptyPrefix = nonEmptyPrefix.tail
        if(!nonEmptyPrefix.isEmpty)
          prefix = iterableFactory.from(f(nonEmptyPrefix.head))
      }

      if (nonEmptyPrefix.isEmpty) iterableFactory.empty
      else prefix.lazyAppendedAll(nonEmptyPrefix.tail.flatMap(f))
    }

  override final def zip[B](that: collection.IterableOnce[B]): Stream[(A, B)] =
    if (this.isEmpty || that.isEmpty) iterableFactory.empty
    else {
      val thatIterable = that match {
        case that: collection.Iterable[B] => that
        case _ => LazyList.from(that)
      }
      cons[(A, B)]((this.head, thatIterable.head), this.tail.zip(thatIterable.tail))
    }

  override final def zipWithIndex: Stream[(A, Int)] = this.zip(LazyList.from(0))

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

@deprecated("Use LazyList (which is fully lazy) instead of Stream (which has a lazy tail only)", "2.13.0")
@SerialVersionUID(3L)
object Stream extends SeqFactory[Stream] {

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

  override def newBuilder[A]: mutable.Builder[A, Stream[A]] = ArrayBuffer.newBuilder[A].mapResult(array => from(array))

  private[immutable] def withFilter[A](l: Stream[A] @uncheckedVariance, p: A => Boolean): collection.WithFilter[A, Stream] =
    new WithFilter[A](l, p)

  private[this] final class WithFilter[A](l: Stream[A] @uncheckedVariance, p: A => Boolean) extends collection.WithFilter[A, Stream] {
    private[this] var s = l                                                // set to null to allow GC after filtered
    private[this] lazy val filtered: Stream[A] = { val f = s.filter(p); s = null.asInstanceOf[Stream[A]]; f } // don't set to null if throw during filter
    def map[B](f: A => B): Stream[B] = filtered.map(f)
    def flatMap[B](f: A => IterableOnce[B]): Stream[B] = filtered.flatMap(f)
    def foreach[U](f: A => U): Unit = filtered.foreach(f)
    def withFilter(q: A => Boolean): collection.WithFilter[A, Stream] = new WithFilter(filtered, q)
  }

  /** An infinite LazyList that repeatedly applies a given function to a start value.
    *
    *  @param start the start value of the LazyList
    *  @param f     the function that's repeatedly applied
    *  @return      the LazyList returning the infinite sequence of values `start, f(start), f(f(start)), ...`
    */
  def iterate[A](start: => A)(f: A => A): Stream[A] = {
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
  def from(start: Int, step: Int): Stream[Int] =
    newCons(start, from(start + step, step))

  /**
    * Create an infinite LazyList starting at `start` and incrementing by `1`.
    *
    * @param start the start value of the LazyList
    * @return the LazyList starting at value `start`.
    */
  def from(start: Int): Stream[Int] = from(start, 1)

  /**
    * Create an infinite LazyList containing the given element expression (which
    * is computed for each occurrence).
    *
    * @param elem the element composing the resulting LazyList
    * @return the LazyList containing an infinite number of elem
    */
  def continually[A](elem: => A): Stream[A] = newCons(elem, continually(elem))


  private[Stream] def filteredTail[A](stream: Stream[A] @uncheckedVariance, p: A => Boolean, isFlipped: Boolean) = {
    newCons(stream.head, stream.tail.filterImpl(p, isFlipped))
  }

  private[Stream] def collectedTail[A, B](head: B, stream: Stream[A] @uncheckedVariance, pf: PartialFunction[A, B]) = {
    newCons(head, stream.tail.collect(pf))
  }

  /** This serialization proxy is used for LazyLists and Streams which start with a sequence of evaluated cons cells.
    * The forced sequence is serialized in a compact, sequential format, followed by the unevaluated tail, which uses
    * standard Java serialization to store the complete structure of unevaluated thunks. This allows the serialization
    * of long evaluated streams without exhausting the stack through recursive serialization of cons cells.
    */
  @SerialVersionUID(3L)
  class SerializationProxy[A](@transient protected var coll: Stream[A]) extends Serializable {

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
      val tail = in.readObject().asInstanceOf[Stream[A]]
      coll = (init ++: tail)
    }

    protected[this] def readResolve(): Any = coll
  }
}
