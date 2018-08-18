package scala.collection

import java.io.{ObjectInputStream, ObjectOutputStream}

import scala.collection.mutable.{ArrayBuffer, Builder}
import scala.collection.immutable.LazyList


/** Views are collections whose transformation operations are non strict: the resulting elements
  * are evaluated only when the view is effectively traversed (e.g. using `foreach` or `foldLeft`),
  * or when the view is converted to a strict collection type (using the `to` operation).
  * @define coll view
  * @define Coll `View`
  */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] {

  override def view: View[A] = this

  override def iterableFactory: IterableFactory[View] = View

  override def toString: String  = stringPrefix + "(?)"

  override protected[this] def stringPrefix: String = "View"

  @deprecated("Views no longer know about their underlying collection type; .force always returns an IndexedSeq", "2.13.0")
  @`inline` def force: IndexedSeq[A] = toIndexedSeq

  override protected[this] def writeReplace(): AnyRef = this
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

  /** A class that partitions an underlying collection into two views */
  @SerialVersionUID(3L)
  class Partition[A](val underlying: SomeIterableOps[A], val p: A => Boolean) extends Serializable {

    /** The view consisting of all elements of the underlying collection
     *  that satisfy `p`.
     */
    val first = new Partitioned(this, true)

    /** The view consisting of all elements of the underlying collection
     *  that do not satisfy `p`.
     */
    val second = new Partitioned(this, false)
  }

  /** A view representing one half of a partition. */
  @SerialVersionUID(3L)
  class Partitioned[A](partition: Partition[A], cond: Boolean) extends AbstractView[A] {
    def iterator = partition.underlying.iterator.filter(x => partition.p(x) == cond)
    override def knownSize: Int = if (partition.underlying.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  /** A class that splits an underlying collection into two views */
  @SerialVersionUID(3L)
  class PartitionWith[A, A1, A2](val underlying: SomeIterableOps[A], val f: A => Either[A1, A2]) extends Serializable {

    /** The view consisting of all elements of the underlying collection
      *  that map to `Left`.
      */
    val left: View[A1] = new LeftPartitionedWith(this, f)


    /** The view consisting of all elements of the underlying collection
      *  that map to `Right`.
      */
    val right: View[A2] = new RightPartitionedWith(this, f)

  }

  @SerialVersionUID(3L)
  class LeftPartitionedWith[A, A1, A2](partitionWith: PartitionWith[A, A1, A2], f: A => Either[A1, A2]) extends AbstractView[A1] {
    def iterator = new AbstractIterator[A1] {
      private val self = partitionWith.underlying.iterator
      private var hd: A1 = _
      private var hdDefined: Boolean = false
      def hasNext = hdDefined || {
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
  class RightPartitionedWith[A, A1, A2](partitionWith: PartitionWith[A, A1, A2], f: A => Either[A1, A2]) extends AbstractView[A2] {
      def iterator = new AbstractIterator[A2] {
        private val self = partitionWith.underlying.iterator
        private var hd: A2 = _
        private var hdDefined: Boolean = false
        def hasNext = hdDefined || {
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
    override def knownSize = underlying.knownSize min other.knownSize
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
      def hasNext: Boolean = it.hasNext
    }
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = iterator.isEmpty
  }

  @SerialVersionUID(3L)
  private[collection] class Patched[A](underlying: SomeIterableOps[A], from: Int, other: IterableOnce[A], replaced: Int) extends AbstractView[A] {
    def iterator: Iterator[A] = underlying.iterator.patch(from, other.iterator, replaced)
    override def knownSize: Int = if (underlying.knownSize == 0 && other.knownSize == 0) 0 else super.knownSize
    override def isEmpty: Boolean = if (knownSize == 0) true else iterator.isEmpty
  }

  @SerialVersionUID(3L)
  class ZipWithIndex[A](underlying: SomeIterableOps[A]) extends AbstractView[(A, Int)] {
    def iterator: Iterator[(A, Int)] = underlying.iterator.zipWithIndex
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
  }

  @SerialVersionUID(3L)
  class Unzip[A, A1, A2](underlying: SomeIterableOps[A])(implicit asPair: A => (A1, A2)) extends Serializable {
    val first: View[A1] = new View.Map[A, A1](underlying, asPair(_)._1)
    val second: View[A2] = new View.Map[A, A2](underlying, asPair(_)._2)
  }

  @SerialVersionUID(3L)
  class Unzip3[A, A1, A2, A3](underlying: SomeIterableOps[A])(implicit asTriple: A => (A1, A2, A3)) extends Serializable {
    val first: View[A1] = new View.Map[A, A1](underlying, asTriple(_)._1)
    val second: View[A2] = new View.Map[A, A2](underlying, asTriple(_)._2)
    val third: View[A3] = new View.Map[A, A3](underlying, asTriple(_)._3)
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

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}

/** Explicit instantiation of the `View` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractView[+A] extends scala.collection.AbstractIterable[A] with View[A]
