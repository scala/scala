package scala
package collection


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

  override def prepended[B >: A](elem: B): IndexedView[B] = new IndexedView.Prepended(elem, this)
  override def take(n: Int): IndexedView[A] = new IndexedView.Take(this, n)
  override def takeRight(n: Int): IndexedView[A] = new IndexedView.TakeRight(this, n)
  override def drop(n: Int): IndexedView[A] = new IndexedView.Drop(this, n)
  override def dropRight(n: Int): IndexedView[A] = new IndexedView.DropRight(this, n)
  override def map[B](f: A => B): IndexedView[B] = new IndexedView.Map(this, f)
  override def reverse: IndexedView[A] = new IndexedView.Reverse(this)
  override def slice(from: Int, until: Int): IndexedView[A] = new IndexedView.Slice(this, from, until)
}

object IndexedView {

  /** An `IndexedSeqOps` whose collection type and collection type constructor are unknown */
  type SomeIndexedSeqOps[A] = IndexedSeqOps[A, AnyConstr, _]

  class Id[+A](underlying: SomeIndexedSeqOps[A])
    extends SeqView.Id(underlying) with IndexedView[A]

  class Prepended[+A](elem: A, underlying: SomeIndexedSeqOps[A])
    extends SeqView.Prepended(elem, underlying) with IndexedView[A]

  class Take[A](underlying: SomeIndexedSeqOps[A], n: Int)
    extends SeqView.Take(underlying, n) with IndexedView[A]

  class TakeRight[A](underlying: SomeIndexedSeqOps[A], n: Int) extends AbstractIndexedView[A] {
    private[this] val delta = (underlying.size - (n max 0)) max 0
    def length = underlying.size - delta
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  class Drop[A](underlying: SomeIndexedSeqOps[A], n: Int) extends View.Drop[A](underlying, n) with IndexedView[A] {
    def length = (underlying.size - normN) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
  }

  class DropRight[A](underlying: SomeIndexedSeqOps[A], n: Int) extends AbstractIndexedView[A] {
    private[this] val len = (underlying.size - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  class Map[A, B](underlying: SomeIndexedSeqOps[A], f: A => B)
    extends SeqView.Map(underlying, f) with IndexedView[B]

  class Reverse[A](underlying: SomeIndexedSeqOps[A]) extends AbstractIndexedView[A] {
    def length = underlying.size
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(size - 1 - i)
  }

  class Slice[A](underlying: SomeIndexedSeqOps[A], from: Int, until: Int) extends AbstractIndexedView[A] {
    protected val lo = from max 0
    protected val hi = (until max 0) min underlying.length
    protected val len = (hi - lo) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int): A = underlying(lo + i)
    def length: Int = len
  }
}

/** Explicit instantiation of the `IndexedView` trait to reduce class file size in subclasses. */
abstract class AbstractIndexedView[+A] extends AbstractSeqView[A] with IndexedView[A]