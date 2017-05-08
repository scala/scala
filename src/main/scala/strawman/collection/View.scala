package strawman.collection

import scala.{Any, Boolean, Equals, Int, Nothing, annotation}
import scala.Predef.intWrapper

/** Concrete collection type: View */
trait View[+A] extends Iterable[A] with IterableOps[A, View, View[A]] {
  override def view = this

  /** Avoid copying if source collection is already a view. */
  override def fromIterable[B](c: Iterable[B]): View[B] = c match {
    case c: View[B] => c
    case _ => View.fromIterator(c.iterator())
  }
  override protected[this] def fromSpecificIterable(coll: Iterable[A]): View[A] =
    fromIterable(coll)

  override def className = "View"
}

/** This object reifies operations on views as case classes */
object View {
  def fromIterator[A](it: => Iterator[A]): View[A] = new View[A] {
    def iterator() = it
  }

  /** The empty view */
  case object Empty extends View[Nothing] {
    def iterator() = Iterator.empty
    override def knownSize = 0
  }

  /** A view with given elements */
  case class Elems[A](xs: A*) extends View[A] {
    def iterator() = Iterator(xs: _*)
    override def knownSize = xs.length // should be: xs.knownSize, but A*'s are not sequences in this strawman.
  }

  /** A view filled with `n` identical elements */
  case class Fill[A](n: Int)(elem: => A) extends View[A] {
    def iterator() =
      new Iterator[A] {
        private var i = 0
        def hasNext: Boolean = i < n
        def next(): A = {
          i = i + 1
          if (i <= n) elem else Iterator.empty.next()
        }
      }
    override def knownSize: Int = n
  }

  /** A view that filters an underlying collection. */
  case class Filter[A](underlying: Iterable[A], p: A => Boolean) extends View[A] {
    def iterator() = underlying.iterator().filter(p)
  }

  /** A view that partitions an underlying collection into two views */
  case class Partition[A](underlying: Iterable[A], p: A => Boolean) {

    /** The view consisting of all elements of the underlying collection
     *  that satisfy `p`.
     */
    val left = Partitioned(this, true)

    /** The view consisting of all elements of the underlying collection
     *  that do not satisfy `p`.
     */
    val right = Partitioned(this, false)
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

  /** A view that takes leading elements of the underlying collection. */
  case class Take[A](underlying: Iterable[A], n: Int) extends View[A] {
    def iterator() = underlying.iterator().take(n)
    protected val normN = n max 0
    override def knownSize =
      if (underlying.knownSize >= 0) underlying.knownSize min normN else -1
  }

  /** A view that maps elements of the underlying collection. */
  case class Map[A, B](underlying: Iterable[A], f: A => B) extends View[B] {
    def iterator() = underlying.iterator().map(f)
    override def knownSize = underlying.knownSize
  }

  /** A view that flatmaps elements of the underlying collection. */
  case class FlatMap[A, B](underlying: Iterable[A], f: A => IterableOnce[B]) extends View[B] {
    def iterator() = underlying.iterator().flatMap(f)
  }

  /** A view that concatenates elements of the underlying collection with the elements
   *  of another collection or iterator.
   */
  case class Concat[A](underlying: Iterable[A], other: IterableOnce[A]) extends View[A] {
    def iterator() = underlying.iterator() ++ other
    override def knownSize = other match {
      case other: Iterable[_] if underlying.knownSize >= 0 && other.knownSize >= 0 =>
        underlying.knownSize + other.knownSize
      case _ =>
        -1
    }
  }

  /** A view that zips elements of the underlying collection with the elements
   *  of another collection or iterator.
   */
  case class Zip[A, B](underlying: Iterable[A], other: IterableOnce[B]) extends View[(A, B)] {
    def iterator() = underlying.iterator().zip(other)
    override def knownSize = other match {
      case other: Iterable[_] => underlying.knownSize min other.knownSize
      case _ => -1
    }
  }
}

/** A trait representing indexable collections with finite length */
trait ArrayLike[+A] extends Any {
  def length: Int
  def apply(i: Int): A
}

/** View defined in terms of indexing a range */
trait IndexedView[+A] extends View[A] with ArrayLike[A] { self =>

  def iterator(): Iterator[A] = new Iterator[A] {
    private var current = 0
    def hasNext = current < self.length
    def next(): A = {
      val r = apply(current)
      current += 1
      r
    }
  }

  override def take(n: Int): IndexedView[A] = new IndexedView.Take(this, n)
  override def drop(n: Int): IndexedView[A] = new IndexedView.Drop(this, n)
  override def map[B](f: A => B): IndexedView[B] = new IndexedView.Map(this, f)
  def reverse: IndexedView[A] = new IndexedView.Reverse(this)
}

object IndexedView {

  class Take[A](underlying: IndexedView[A], n: Int)
  extends View.Take(underlying, n) with IndexedView[A] {
    override def iterator() = super.iterator() // needed to avoid "conflicting overrides" error
    def length = underlying.length min normN
    def apply(i: Int) = underlying.apply(i)
  }

  class Drop[A](underlying: IndexedView[A], n: Int)
  extends View.Take(underlying, n) with IndexedView[A] {
    override def iterator() = super.iterator()
    def length = (underlying.length - normN) max 0
    def apply(i: Int) = underlying.apply(i + normN)
  }

  class Map[A, B](underlying: IndexedView[A], f: A => B)
  extends View.Map(underlying, f) with IndexedView[B] {
    override def iterator() = super.iterator()
    def length = underlying.length
    def apply(n: Int) = f(underlying.apply(n))
  }

  case class Reverse[A](underlying: IndexedView[A]) extends IndexedView[A] {
    def length = underlying.length
    def apply(i: Int) = underlying.apply(length - 1 - i)
  }
}
