package strawman.collection

import strawman.collection.mutable.{ArrayBuffer, Builder}

import scala.{Any, Boolean, Equals, Int, NoSuchElementException, Nothing, annotation, IndexOutOfBoundsException, throws}
import scala.Predef.{<:<, intWrapper}

/** Concrete collection type: View */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] {
  override def view = this

  def iterableFactory = View

  protected[this] def fromSpecificIterable(coll: Iterable[A]): View[A] = fromIterable(coll)

  protected[this] def newSpecificBuilder(): Builder[A, View[A]] =
    immutable.IndexedSeq.newBuilder().mapResult(_.view)

  final protected[this] def coll: this.type = this

  override def toString = "View(?)"

  override def className = "View"
}

/** This object reifies operations on views as case classes */
object View extends IterableFactory[View] {

  def fromIterator[A](it: => Iterator[A]): View[A] = new View[A] {
    def iterator() = it
  }

  /** Avoid copying if source collection is already a view. */
  def fromIterable[E](it: Iterable[E]): View[E] = it match {
    case it: View[E] => it
    case _ => View.fromIterator(it.iterator())
  }

  def empty[A]: View[A] = Empty

  def newBuilder[A](): Builder[A, View[A]] = ArrayBuffer.newBuilder[A]().mapResult(fromIterable)

  override def apply[A](xs: A*): View[A] = Elems(xs: _*)

  /** The empty view */
  case object Empty extends View[Nothing] {
    def iterator() = Iterator.empty
    override def knownSize = 0
  }

  /** A view with exactly one element */
  case class Single[A](a: A) extends View[A] {
    def iterator(): Iterator[A] =
      new Iterator[A] {
        private var notConsumed: Boolean = true
        def next(): A =
          if (notConsumed) {
            notConsumed = false
            a
          } else Iterator.empty.next()
        def hasNext: Boolean = notConsumed
      }
    override def knownSize: Int = 1
  }

  /** A view with given elements */
  case class Elems[A](xs: A*) extends View[A] {
    def iterator() = Iterator(xs: _*)
    override def knownSize = xs.length // should be: xs.knownSize, but A*'s are not sequences in this strawman.
  }

  /** A view filled with `n` identical elements */
  case class Fill[A](n: Int)(elem: => A) extends View[A] {
    def iterator() = Iterator.fill(n)(elem)
    override def knownSize: Int = 0 max n
  }

  /** A view containing values of a given function over a range of integer values starting from 0. */
  class Tabulate[A](n: Int)(f: Int => A) extends View[A] {
    def iterator(): Iterator[A] = Iterator.tabulate(n)(f)
    override def knownSize: Int = 0 max n
  }

  /** A view containing repeated applications of a function to a start value */
  class Iterate[A](start: A, len: Int)(f: A => A) extends View[A] {
    def iterator(): Iterator[A] = Iterator.iterate(start)(f).take(len)
    override def knownSize: Int = 0 max len
  }

  /** A view that filters an underlying collection. */
  class Filter[A](val underlying: Iterable[A], val p: A => Boolean, val isFlipped: Boolean) extends View[A] {
    def iterator() = underlying.iterator().filterImpl(p, isFlipped)
  }

  object Filter {
    def apply[A](underlying: Iterable[A], p: A => Boolean, isFlipped: Boolean): Filter[A] =
      underlying match {
        case filter: Filter[A] if filter.isFlipped == isFlipped => new Filter(filter.underlying, a => filter.p(a) && p(a), isFlipped)
        case _ => new Filter(underlying, p, isFlipped)
      }
  }

  case class FilterKeys[K, V](underlying: Iterable[(K, V)], p: K => Boolean) extends View[(K, V)] {
    def iterator(): Iterator[(K, V)] = underlying.iterator().filter(kv => p(kv._1))
  }

  /** A view that removes the duplicated elements **/
  class Distinct[A](val underlying: Iterable[A]) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().distinct
  }
  /** A view that partitions an underlying collection into two views */
  case class Partition[A](underlying: Iterable[A], p: A => Boolean) {

    /** The view consisting of all elements of the underlying collection
     *  that satisfy `p`.
     */
    val first = Partitioned(this, true)

    /** The view consisting of all elements of the underlying collection
     *  that do not satisfy `p`.
     */
    val second = Partitioned(this, false)
  }

  /** A view representing one half of a partition. */
  case class Partitioned[A](partition: Partition[A], cond: Boolean) extends View[A] {
    def iterator() = partition.underlying.iterator().filter(x => partition.p(x) == cond)
  }

  /** A view that drops leading elements of the underlying collection. */
  case class Drop[A](underlying: Iterable[A], n: Int) extends View[A] {
    def iterator() = underlying.iterator().drop(n)
    protected val normN = n max 0
    override def knownSize =
      if (underlying.knownSize >= 0) (underlying.knownSize - normN) max 0 else -1
  }

  case class DropWhile[A](underlying: Iterable[A], p: A => Boolean) extends View[A] {
    def iterator() = underlying.iterator().dropWhile(p)
  }

  /** A view that takes leading elements of the underlying collection. */
  case class Take[A](underlying: Iterable[A], n: Int) extends View[A] {
    def iterator() = underlying.iterator().take(n)
    protected val normN = n max 0
    override def knownSize =
      if (underlying.knownSize >= 0) underlying.knownSize min normN else -1
  }

  case class TakeWhile[A](underlying: Iterable[A], p: A => Boolean) extends View[A] {
    def iterator(): Iterator[A] = underlying.iterator().takeWhile(p)
  }

  case class ScanLeft[A, B](underlying: Iterable[A], z: B, op: (B, A) => B) extends View[B] {
    def iterator(): Iterator[B] = underlying.iterator().scanLeft(z)(op)
    override def knownSize: Int =
      if (underlying.knownSize >= 0) underlying.knownSize + 1 else -1
  }

  /** A view that maps elements of the underlying collection. */
  case class Map[A, B](underlying: Iterable[A], f: A => B) extends View[B] {
    def iterator() = underlying.iterator().map(f)
    override def knownSize = underlying.knownSize
  }

  case class MapValues[K, V, W](underlying: Iterable[(K, V)], f: V => W) extends View[(K, W)] {
    def iterator(): Iterator[(K, W)] = underlying.iterator().map(kv => (kv._1, f(kv._2)))
    override def knownSize: Int = underlying.knownSize
  }

  /** A view that flatmaps elements of the underlying collection. */
  case class FlatMap[A, B](underlying: Iterable[A], f: A => IterableOnce[B]) extends View[B] {
    def iterator() = underlying.iterator().flatMap(f)
  }

  /** A view that concatenates elements of the prefix collection or iterator with the elements
   *  of the suffix collection or iterator.
   */
  case class Concat[A](prefix: Iterable[A], suffix: Iterable[A]) extends View[A] {
    def iterator() = prefix.iterator() ++ suffix.iterator()
    override def knownSize =
      if (prefix.knownSize >= 0 && suffix.knownSize >= 0) prefix.knownSize + prefix.knownSize
      else -1
  }

  /** A view that zips elements of the underlying collection with the elements
   *  of another collection or iterator.
   */
  case class Zip[A, B](underlying: Iterable[A], other: Iterable[B]) extends View[(A, B)] {
    def iterator() = underlying.iterator().zip(other)
    override def knownSize = underlying.knownSize min other.knownSize
  }

  /** A view that appends an element to its elements */
  case class Append[A](underlying: Iterable[A], elem: A) extends View[A] {
    def iterator(): Iterator[A] = Concat(underlying, View.Single(elem)).iterator()
    override def knownSize: Int = if (underlying.knownSize >= 0) underlying.knownSize + 1 else -1
  }

  /** A view that prepends an element to its elements */
  case class Prepend[A](elem: A, underlying: Iterable[A]) extends View[A] {
    def iterator(): Iterator[A] = Concat(View.Single(elem), underlying).iterator()
    override def knownSize: Int = if (underlying.knownSize >= 0) underlying.knownSize + 1 else -1
  }

  case class Updated[A](underlying: Iterable[A], index: Int, elem: A) extends View[A] {
    def iterator(): Iterator[A] = new Iterator[A] {
      private val it = underlying.iterator()
      private var i = 0
      def next(): A = {
        val value = if (i == index) { it.next(); elem } else it.next()
        i += 1
        value
      }
      def hasNext: Boolean = it.hasNext
    }
    override def knownSize: Int = underlying.knownSize
  }

  private[collection] class Patched[A](underlying: Iterable[A], from: Int, other: IterableOnce[A], replaced: Int) extends View[A] {
    if (from < 0 || (knownSize > -1 && from > knownSize)) throw new IndexOutOfBoundsException(from.toString)
    def iterator(): Iterator[A] = underlying.iterator().patch(from, other.iterator(), replaced)
  }

  case class ZipWithIndex[A](underlying: Iterable[A]) extends View[(A, Int)] {
    def iterator(): Iterator[(A, Int)] = underlying.iterator().zipWithIndex
    override def knownSize: Int = underlying.knownSize
  }

  case class Unzip[A, A1, A2](underlying: Iterable[A])(implicit asPair: A <:< (A1, A2)) {
    val first: View[A1] =
      new View[A1] {
        def iterator(): Iterator[A1] = underlying.iterator().map(_._1)
        override def knownSize: Int = underlying.knownSize
      }
    val second: View[A2] =
      new View[A2] {
        def iterator(): Iterator[A2] = underlying.iterator().map(_._2)
        override def knownSize: Int = underlying.knownSize
      }
  }

  case class PadTo[A](underlying: Iterable[A], len: Int, elem: A) extends View[A] {
    def iterator(): Iterator[A] = new Iterator[A] {
      private var i = 0
      private val it = underlying.iterator()
      def next(): A = {
        val a =
          if (it.hasNext) it.next()
          else if (i < len) elem
          else Iterator.empty.next()
        i += 1
        a
      }
      def hasNext: Boolean = it.hasNext || i < len
    }
    override def knownSize: Int = if (underlying.knownSize >= 0) underlying.knownSize max len else -1
  }

}

/** A trait representing indexable collections with finite length */
trait ArrayLike[+A] extends Any {
  def length: Int
  @throws[IndexOutOfBoundsException]
  def apply(i: Int): A
}

/** View defined in terms of indexing a range */
trait IndexedView[+A] extends View[A] with ArrayLike[A] with SeqOps[A, View, IndexedView[A]] { self =>

  final def toSeq: Seq[A] = to(IndexedSeq)

  override protected[this] def fromSpecificIterable(it: Iterable[A]): IndexedView[A] =
    it match {
      case v: IndexedView[A] => v
      case i: IndexedSeq[A] => i.view
      case _ => it.to(IndexedSeq).view
    }

  override protected[this] def newSpecificBuilder(): Builder[A, IndexedView[A]] =
    IndexedSeq.newBuilder[A]().mapResult(_.view)

  def iterator(): Iterator[A] = new Iterator[A] {
    private var current = 0
    def hasNext = current < self.length
    def next(): A = {
      val r = apply(current)
      current += 1
      r
    }
  }

  override def knownSize: Int = length

  override def take(n: Int): IndexedView[A] = new IndexedView.Take(this, n)
  override def takeRight(n: Int): IndexedView[A] = new IndexedView.TakeRight(this, n)
  override def drop(n: Int): IndexedView[A] = new IndexedView.Drop(this, n)
  override def dropRight(n: Int): IndexedView[A] = new IndexedView.DropRight(this, n)
  override def map[B](f: A => B): IndexedView[B] = new IndexedView.Map(this, f)
  override def reverse: IndexedView[A] = IndexedView.Reverse(this)
}

object IndexedView {

  class Take[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    private[this] val normN = n max 0
    def length = underlying.length min normN
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  class TakeRight[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    private[this] val delta = (underlying.length - (n max 0)) max 0
    def length = underlying.length - delta
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  class Drop[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    protected val normN = n max 0
    def length = (underlying.length - normN) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
  }

  class DropRight[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    private[this] val len = (underlying.length - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  class Map[A, B](underlying: IndexedView[A], f: A => B) extends IndexedView[B] {
    def length = underlying.length
    @throws[IndexOutOfBoundsException]
    def apply(n: Int) = f(underlying.apply(n))
  }

  case class Reverse[A](underlying: IndexedView[A]) extends IndexedView[A] {
    def length = underlying.length
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(length - 1 - i)
  }
}
