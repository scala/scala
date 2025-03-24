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

package scala.collection

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.immutable.LazyList

/** Views are collections whose transformation operations are non strict: the resulting elements
  * are evaluated only when the view is effectively traversed (e.g. using `foreach` or `foldLeft`),
  * or when the view is converted to a strict collection type (using the `to` operation).
  * @define coll view
  * @define Coll `View`
  */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] with IterableFactoryDefaults[A, View] with Serializable {

  override def view: View[A] = this

  override def iterableFactory: IterableFactory[View] = View

  override def empty: scala.collection.View[A] = iterableFactory.empty

  override def toString: String  = className + "(<not computed>)"

  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix: String = "View"

  @deprecated("Views no longer know about their underlying collection type; .force always returns an IndexedSeq", "2.13.0")
  @`inline` def force: IndexedSeq[A] = toIndexedSeq
}

/** This object reifies operations on views as case classes
  *
  * @define Coll View
  * @define coll view
  */
@SerialVersionUID(3L)
object View extends IterableFactory[View] {

  /**
    * @return A `View[A]` whose underlying iterator is provided by the `it` parameter-less function.
    *
    * @param it Function creating the iterator to be used by the view. This function must always return
    *           a fresh `Iterator`, otherwise the resulting view will be effectively iterable only once.
    *
    * @tparam A View element type
    */
  def fromIteratorProvider[A](it: () => Iterator[A]): View[A] = new AbstractView[A] {
    def iterator = it()
  }

  /**
    * @return A view iterating over the given `Iterable`
    *
    * @param it The `IterableOnce` to view. A proper `Iterable` is used directly. If it is really only
    *           `IterableOnce` it gets memoized on the first traversal.
    *
    * @tparam E View element type
    */
  def from[E](it: IterableOnce[E]): View[E] = it match {
    case it: View[E]     => it
    case it: Iterable[E] => View.fromIteratorProvider(() => it.iterator)
    case _               => LazyList.from(it).view
  }

  def empty[A]: View[A] = Empty

  def newBuilder[A]: Builder[A, View[A]] = ArrayBuffer.newBuilder[A].mapResult(from)

  override def apply[A](xs: A*): View[A] = new Elems(xs: _*)

  /** The empty view */
  @SerialVersionUID(3L)
  case object Empty extends AbstractView[Nothing] {
    def iterator = Iterator.empty
    override def knownSize = 0
    override def isEmpty: Boolean = true
  }

  /** A view with exactly one element */
  @SerialVersionUID(3L)
  class Single[A](a: A) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.single(a)
    override def knownSize: Int = 1
    override def isEmpty: Boolean = false
  }

  /** A view with given elements */
  @SerialVersionUID(3L)
  class Elems[A](xs: A*) extends AbstractView[A] {
    def iterator = xs.iterator
    override def knownSize = xs.knownSize
    override def isEmpty: Boolean = xs.isEmpty
  }

  /** A view containing the results of some element computation a number of times. */
  @SerialVersionUID(3L)
  class Fill[A](n: Int)(elem: => A) extends AbstractView[A] {
    def iterator = Iterator.fill(n)(elem)
    override def knownSize: Int = 0 max n
    override def isEmpty: Boolean = n <= 0
  }

  /** A view containing values of a given function over a range of integer values starting from 0. */
  @SerialVersionUID(3L)
  class Tabulate[A](n: Int)(f: Int => A) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.tabulate(n)(f)
    override def knownSize: Int = 0 max n
    override def isEmpty: Boolean = n <= 0
  }

  /** A view containing repeated applications of a function to a start value */
  @SerialVersionUID(3L)
  class Iterate[A](start: A, len: Int)(f: A => A) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.iterate(start)(f).take(len)
    override def knownSize: Int = 0 max len
    override def isEmpty: Boolean = len <= 0
  }

  /** A view that uses a function `f` to produce elements of type `A` and update
    * an internal state `S`.
    */
  @SerialVersionUID(3L)
  class Unfold[A, S](initial: S)(f: S => Option[(A, S)]) extends AbstractView[A] {
    def iterator: Iterator[A] = Iterator.unfold(initial)(f)
  }

  /** An `IterableOps` whose collection type and collection type constructor are unknown */
  type SomeIterableOps[A] = IterableOps[A, AnyConstr, _]

  /** A view that filters an underlying collection. */
  @SerialVersionUID(3L)
  class Filter[A](val underlying: SomeIterableOps[A], val p: A => Boolean, val isFlipped: Boolean) extends AbstractView[A] {
    def iterator = underlying.iterator.filterImpl(p, isFlipped)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  object Filter {
    def apply[A](underlying: Iterable[A], p: A => Boolean, isFlipped: Boolean): Filter[A] =
      underlying match {
        case filter: Filter[A] if filter.isFlipped == isFlipped => new Filter(filter.underlying, a => filter.p(a) && p(a), isFlipped)
        case _ => new Filter(underlying, p, isFlipped)
      }
  }

  /** A view that removes the duplicated elements as determined by the transformation function `f` */
  @SerialVersionUID(3L)
  class DistinctBy[A, B](underlying: SomeIterableOps[A], f: A => B) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.distinctBy(f)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class LeftPartitionMapped[A, A1, A2](underlying: SomeIterableOps[A], f: A => Either[A1, A2]) extends AbstractView[A1] {
    def iterator: AbstractIterator[A1] = new AbstractIterator[A1] {
      private[this] val self = underlying.iterator
      private[this] var hd: A1 = _
      private[this] var hdDefined: Boolean = false
      def hasNext = hdDefined || {
        @tailrec
        def findNext(): Boolean =
          if (self.hasNext) {
            f(self.next()) match {
              case Left(a1) => hd = a1; hdDefined = true; true
              case Right(_) => findNext()
            }
          } else false
        findNext()
      }
      def next() =
        if (hasNext) {
          hdDefined = false
          hd
        } else Iterator.empty.next()
    }
  }

  @SerialVersionUID(3L)
  class RightPartitionMapped[A, A1, A2](underlying: SomeIterableOps[A], f: A => Either[A1, A2]) extends AbstractView[A2] {
      def iterator: AbstractIterator[A2] = new AbstractIterator[A2] {
        private[this] val self = underlying.iterator
        private[this] var hd: A2 = _
        private[this] var hdDefined: Boolean = false
        def hasNext = hdDefined || {
          @tailrec
          def findNext(): Boolean =
            if (self.hasNext) {
              f(self.next()) match {
                case Left(_) => findNext()
                case Right(a2) => hd = a2; hdDefined = true; true
              }
            } else false
          findNext()
        }
        def next() =
          if (hasNext) {
            hdDefined = false
            hd
          } else Iterator.empty.next()
      }
  }

  /** A view that drops leading elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator = underlying.iterator.drop(n)
    protected val normN = n max 0
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) (size - normN) max 0 else -1
    }
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that drops trailing elements of the underlying collection. */
  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator = dropRightIterator(underlying.iterator, n)
    protected val normN = n max 0
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) (size - normN) max 0 else -1
    }
    override def isEmpty: Boolean =
      if(knownSize >= 0) knownSize == 0
      else iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class DropWhile[A](underlying: SomeIterableOps[A], p: A => Boolean) extends AbstractView[A] {
    def iterator = underlying.iterator.dropWhile(p)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that takes leading elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Take[+A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator = underlying.iterator.take(n)
    protected val normN = n max 0
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) size min normN else -1
    }
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that takes trailing elements of the underlying collection. */
  @SerialVersionUID(3L)
  class TakeRight[+A](underlying: SomeIterableOps[A], n: Int) extends AbstractView[A] {
    def iterator = takeRightIterator(underlying.iterator, n)
    protected val normN = n max 0
    override def knownSize = {
      val size = underlying.knownSize
      if (size >= 0) size min normN else -1
    }
    override def isEmpty: Boolean =
      if(knownSize >= 0) knownSize == 0
      else iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class TakeWhile[A](underlying: SomeIterableOps[A], p: A => Boolean) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.takeWhile(p)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class ScanLeft[+A, +B](underlying: SomeIterableOps[A], z: B, op: (B, A) => B) extends AbstractView[B] {
    def iterator: Iterator[B] = underlying.iterator.scanLeft(z)(op)
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that maps elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Map[+A, +B](underlying: SomeIterableOps[A], f: A => B) extends AbstractView[B] {
    def iterator = underlying.iterator.map(f)
    override def knownSize = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  /** A view that flatmaps elements of the underlying collection. */
  @SerialVersionUID(3L)
  class FlatMap[A, B](underlying: SomeIterableOps[A], f: A => IterableOnce[B]) extends AbstractView[B] {
    def iterator = underlying.iterator.flatMap(f)
    override def knownSize: Int = if (underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A view that collects elements of the underlying collection. */
  @SerialVersionUID(3L)
  class Collect[+A, B](underlying: SomeIterableOps[A], pf: PartialFunction[A, B]) extends AbstractView[B] {
    def iterator = underlying.iterator.collect(pf)
  }

  /** A view that concatenates elements of the prefix collection or iterator with the elements
   *  of the suffix collection or iterator.
   */
  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeIterableOps[A], suffix: SomeIterableOps[A]) extends AbstractView[A] {
    def iterator = prefix.iterator ++ suffix.iterator
    override def knownSize = {
      val prefixSize = prefix.knownSize
      if (prefixSize >= 0) {
        val suffixSize = suffix.knownSize
        if (suffixSize >= 0) prefixSize + suffixSize
        else -1
      }
      else -1
    }
    override def isEmpty: Boolean = prefix.isEmpty && suffix.isEmpty
  }

  /** A view that zips elements of the underlying collection with the elements
    *  of another collection.
    */
  @SerialVersionUID(3L)
  class Zip[A, B](underlying: SomeIterableOps[A], other: Iterable[B]) extends AbstractView[(A, B)] {
    def iterator = underlying.iterator.zip(other)
    override def knownSize = {
      val s1 = underlying.knownSize
      if (s1 == 0) 0 else {
        val s2 = other.knownSize
        if (s2 == 0) 0 else s1 min s2
      }
    }
    override def isEmpty: Boolean = underlying.isEmpty || other.isEmpty
  }

  /** A view that zips elements of the underlying collection with the elements
    *  of another collection. If one of the two collections is shorter than the other,
    *  placeholder elements are used to extend the shorter collection to the length of the longer.
    */
  @SerialVersionUID(3L)
  class ZipAll[A, B](underlying: SomeIterableOps[A], other: Iterable[B], thisElem: A, thatElem: B) extends AbstractView[(A, B)] {
    def iterator = underlying.iterator.zipAll(other, thisElem, thatElem)
    override def knownSize = {
      val s1 = underlying.knownSize
      if(s1 == -1) -1 else {
        val s2 = other.knownSize
        if(s2 == -1) -1 else s1 max s2
      }
    }
    override def isEmpty: Boolean = underlying.isEmpty && other.isEmpty
  }

  /** A view that appends an element to its elements */
  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeIterableOps[A], elem: A) extends AbstractView[A] {
    def iterator: Iterator[A] = new Concat(underlying, new View.Single(elem)).iterator
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    override def isEmpty: Boolean = false
  }

  /** A view that prepends an element to its elements */
  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeIterableOps[A]) extends AbstractView[A] {
    def iterator: Iterator[A] = new Concat(new View.Single(elem), underlying).iterator
    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size + 1 else -1
    }
    override def isEmpty: Boolean = false
  }

  @SerialVersionUID(3L)
  class Updated[A](underlying: SomeIterableOps[A], index: Int, elem: A) extends AbstractView[A] {
    def iterator: Iterator[A] = new AbstractIterator[A] {
      private[this] val it = underlying.iterator
      private[this] var i = 0
      def next(): A = {
        val value = if (i == index) { it.next(); elem } else it.next()
        i += 1
        value
      }
      def hasNext: Boolean =
        if(it.hasNext) true
        else if(index >= i) throw new IndexOutOfBoundsException(index.toString)
        else false
    }
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  private[collection] class Patched[A](underlying: SomeIterableOps[A], from: Int, other: IterableOnce[A], replaced: Int) extends AbstractView[A] {
    // we may be unable to traverse `other` more than once, so we need to cache it if that's the case
    private val _other: Iterable[A] = other match {
      case other: Iterable[A] => other
      case other              => LazyList.from(other)
    }

    def iterator: Iterator[A] = underlying.iterator.patch(from, _other.iterator, replaced)
    override def knownSize: Int = if (underlying.knownSize == 0 && _other.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = if (knownSize == 0) true else iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class ZipWithIndex[A](underlying: SomeIterableOps[A]) extends AbstractView[(A, Int)] {
    def iterator: Iterator[(A, Int)] = underlying.iterator.zipWithIndex
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class PadTo[A](underlying: SomeIterableOps[A], len: Int, elem: A) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.padTo(len, elem)

    override def knownSize: Int = {
      val size = underlying.knownSize
      if (size >= 0) size max len else -1
    }
    override def isEmpty: Boolean = underlying.isEmpty && len <= 0
  }

  private[collection] def takeRightIterator[A](it: Iterator[A], n: Int): Iterator[A] = {
    val k = it.knownSize
    if(k == 0 || n <= 0) Iterator.empty
    else if(n == Int.MaxValue) it
    else if(k > 0) it.drop((k-n) max 0)
    else new TakeRightIterator[A](it, n)
  }

  private final class TakeRightIterator[A](private[this] var underlying: Iterator[A], maxlen: Int) extends AbstractIterator[A] {
    private[this] var len: Int = -1
    private[this] var pos: Int = 0
    private[this] var buf: ArrayBuffer[AnyRef] = _
    def init(): Unit = if(buf eq null) {
      buf = new ArrayBuffer[AnyRef](maxlen min 256)
      len = 0
      while(underlying.hasNext) {
        val n = underlying.next().asInstanceOf[AnyRef]
        if(pos >= buf.length) buf.addOne(n)
        else buf(pos) = n
        pos += 1
        if(pos == maxlen) pos = 0
        len += 1
      }
      underlying = null
      if(len > maxlen) len = maxlen
      pos = pos - len
      if(pos < 0) pos += maxlen
    }
    override def knownSize = len
    def hasNext: Boolean = {
      init()
      len > 0
    }
    def next(): A = {
      init()
      if(len == 0) Iterator.empty.next()
      else {
        val x = buf(pos).asInstanceOf[A]
        pos += 1
        if(pos == maxlen) pos = 0
        len -= 1
        x
      }
    }
    override def drop(n: Int): Iterator[A] = {
      init()
      if (n > 0) {
        len = (len - n) max 0
        pos = (pos + n) % maxlen
      }
      this
    }
  }

  private[collection] def dropRightIterator[A](it: Iterator[A], n: Int): Iterator[A] = {
    if(n <= 0) it
    else {
      val k = it.knownSize
      if(k >= 0) it.take(k - n)
      else new DropRightIterator[A](it, n)
    }
  }

  private final class DropRightIterator[A](private[this] var underlying: Iterator[A], maxlen: Int) extends AbstractIterator[A] {
    private[this] var len: Int = -1 // known size or -1 if the end of `underlying` has not been seen yet
    private[this] var pos: Int = 0
    private[this] var buf: ArrayBuffer[AnyRef] = _
    def init(): Unit = if(buf eq null) {
      buf = new ArrayBuffer[AnyRef](maxlen min 256)
      while(pos < maxlen && underlying.hasNext) {
        buf.addOne(underlying.next().asInstanceOf[AnyRef])
        pos += 1
      }
      if(!underlying.hasNext) len = 0
      pos = 0
    }
    override def knownSize = len
    def hasNext: Boolean = {
      init()
      len != 0
    }
    def next(): A = {
      if(!hasNext) Iterator.empty.next()
      else {
        val x = buf(pos).asInstanceOf[A]
        if(len == -1) {
          buf(pos) = underlying.next().asInstanceOf[AnyRef]
          if(!underlying.hasNext) len = 0
        } else len -= 1
        pos += 1
        if(pos == maxlen) pos = 0
        x
      }
    }
  }
}

/** Explicit instantiation of the `View` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractView[+A] extends scala.collection.AbstractIterable[A] with View[A]
