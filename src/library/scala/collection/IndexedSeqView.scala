package scala
package collection


/** View defined in terms of indexing a range */
trait IndexedSeqView[+A] extends IndexedSeqOps[A, View, View[A]] with SeqView[A] { self =>

  override def view: IndexedSeqView[A] = this

  override def iterator: Iterator[A] = new IndexedSeqView.IndexedSeqViewIterator(this)

  override def appended[B >: A](elem: B): IndexedSeqView[B] = new IndexedSeqView.Appended(this, elem)
  override def prepended[B >: A](elem: B): IndexedSeqView[B] = new IndexedSeqView.Prepended(elem, this)
  override def take(n: Int): IndexedSeqView[A] = new IndexedSeqView.Take(this, n)
  override def takeRight(n: Int): IndexedSeqView[A] = new IndexedSeqView.TakeRight(this, n)
  override def drop(n: Int): IndexedSeqView[A] = new IndexedSeqView.Drop(this, n)
  override def dropRight(n: Int): IndexedSeqView[A] = new IndexedSeqView.DropRight(this, n)
  override def map[B](f: A => B): IndexedSeqView[B] = new IndexedSeqView.Map(this, f)
  override def reverse: IndexedSeqView[A] = new IndexedSeqView.Reverse(this)
  override def slice(from: Int, until: Int): IndexedSeqView[A] = new IndexedSeqView.Slice(this, from, until)

  def concat[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]): IndexedSeqView[B] = new IndexedSeqView.Concat(this, suffix)
  def appendedAll[B >: A](suffix: IndexedSeqView.SomeIndexedSeqOps[B]): IndexedSeqView[B] = new IndexedSeqView.Concat(this, suffix)
  def prependedAll[B >: A](prefix: IndexedSeqView.SomeIndexedSeqOps[B]): IndexedSeqView[B] = new IndexedSeqView.Concat(prefix, this)
}

object IndexedSeqView {

  @SerialVersionUID(3L)
  private final class IndexedSeqViewIterator[A](self: IndexedSeqView[A]) extends AbstractIterator[A] with Serializable {
    private[this] var current = 0
    override def knownSize: Int = self.size - current
    def hasNext = current < self.size
    def next(): A =
      if (hasNext) {
        val r = self.apply(current)
        current += 1
        r
      } else Iterator.empty.next()
  }

  /** An `IndexedSeqOps` whose collection type and collection type constructor are unknown */
  type SomeIndexedSeqOps[A] = IndexedSeqOps[A, AnyConstr, _]

  @SerialVersionUID(3L)
  class Id[+A](underlying: SomeIndexedSeqOps[A])
    extends SeqView.Id(underlying) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeIndexedSeqOps[A], elem: A)
    extends SeqView.Appended(underlying, elem) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeIndexedSeqOps[A])
    extends SeqView.Prepended(elem, underlying) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeIndexedSeqOps[A], suffix: SomeIndexedSeqOps[A])
    extends SeqView.Concat[A](prefix, suffix) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class Take[A](underlying: SomeIndexedSeqOps[A], n: Int)
    extends SeqView.Take(underlying, n) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class TakeRight[A](underlying: SomeIndexedSeqOps[A], n: Int) extends AbstractIndexedSeqView[A] {
    private[this] val delta = (underlying.size - (n max 0)) max 0
    def length = underlying.size - delta
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeIndexedSeqOps[A], n: Int) extends View.Drop[A](underlying, n) with IndexedSeqView[A] {
    def length = (underlying.size - normN) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
  }

  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeIndexedSeqOps[A], n: Int) extends AbstractIndexedSeqView[A] {
    private[this] val len = (underlying.size - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  @SerialVersionUID(3L)
  class Map[A, B](underlying: SomeIndexedSeqOps[A], f: A => B)
    extends SeqView.Map(underlying, f) with IndexedSeqView[B]

  @SerialVersionUID(3L)
  class Reverse[A](underlying: SomeIndexedSeqOps[A]) extends SeqView.Reverse[A](underlying) with IndexedSeqView[A]

  @SerialVersionUID(3L)
  class Slice[A](underlying: SomeIndexedSeqOps[A], from: Int, until: Int) extends AbstractIndexedSeqView[A] {
    protected val lo = from max 0
    protected val hi = (until max 0) min underlying.length
    protected val len = (hi - lo) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int): A = underlying(lo + i)
    def length: Int = len
  }
}

/** Explicit instantiation of the `IndexedSeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractIndexedSeqView[+A] extends AbstractSeqView[A] with IndexedSeqView[A]
