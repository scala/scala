package strawman
package collection

import scala.{Any, Boolean, Equals, IllegalArgumentException, Int, NoSuchElementException, Nothing, annotation, IndexOutOfBoundsException, throws, AnyRef, Array, deprecated, `inline`}
import scala.Predef.{<:<, intWrapper}

/** A trait representing indexable collections with finite length */
trait ArrayLike[+A] extends Any {
  /** The finite size of the collection. */
  protected def finiteSize: Int
  @throws[IndexOutOfBoundsException]
  def apply(i: Int): A
}

/** View defined in terms of indexing a range */
trait IndexedView[+A] extends View[A] with SeqOps[A, View, View[A]] { self =>

  def iterator(): Iterator[A] = new AbstractIterator[A] {
    private var current = 0
    override def knownSize: Int = self.size - current
    def hasNext = current < self.size
    def next(): A = {
      val r = self.apply(current)
      current += 1
      r
    }
  }

  final override def knownSize: Int = length

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
    def length = underlying.size min normN
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  class TakeRight[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    private[this] val delta = (underlying.size - (n max 0)) max 0
    def length = underlying.size - delta
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  class Drop[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    protected val normN = n max 0
    def length = (underlying.size - normN) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
  }

  class DropRight[A](underlying: IndexedView[A], n: Int) extends IndexedView[A] {
    private[this] val len = (underlying.size - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  class Map[A, B](underlying: IndexedView[A], f: A => B) extends IndexedView[B] {
    def length = underlying.size
    @throws[IndexOutOfBoundsException]
    def apply(n: Int) = f(underlying.apply(n))
  }

  case class Reverse[A](underlying: IndexedView[A]) extends IndexedView[A] {
    def length = underlying.size
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(size - 1 - i)
  }
}
