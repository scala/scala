package strawman
package collection

import scala.{Any, Boolean, Equals, IllegalArgumentException, Int, NoSuchElementException, Nothing, annotation, IndexOutOfBoundsException, throws, AnyRef, Array, deprecated, `inline`}
import scala.Predef.{<:<, intWrapper}

/** View defined in terms of indexing a range */
trait IndexedView[+A] extends IndexedSeqOps[A, View, View[A]] with SeqView[A] { self =>

  override def view: IndexedView[A] = this

  override def iterator(): Iterator[A] = new AbstractIterator[A] {
    private var current = 0
    override def knownSize: Int = self.size - current
    def hasNext = current < self.size
    def next(): A = {
      val r = self.apply(current)
      current += 1
      r
    }
  }

  override def take(n: Int): IndexedView[A] = new IndexedView.Take(this, n)
  override def takeRight(n: Int): IndexedView[A] = new IndexedView.TakeRight(this, n)
  override def drop(n: Int): IndexedView[A] = new IndexedView.Drop(this, n)
  override def dropRight(n: Int): IndexedView[A] = new IndexedView.DropRight(this, n)
  override def map[B](f: A => B): IndexedView[B] = new IndexedView.Map(this, f)
  override def reverse: IndexedView[A] = new IndexedView.Reverse(this)
}

object IndexedView {

  class Id[+A](underlying: IndexedSeqOps[A, AnyConstr, _]) extends IndexedView[A] {
    def length: Int = underlying.length
    def apply(idx: Int): A = underlying(idx)
  }

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

  class Reverse[A](underlying: IndexedView[A]) extends IndexedView[A] {
    def length = underlying.size
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(size - 1 - i)
  }
}
