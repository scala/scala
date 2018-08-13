package scala
package collection


trait SeqView[+A] extends SeqOps[A, View, View[A]] with View[A] {

  override def view: SeqView[A] = this

  override def map[B](f: A => B): SeqView[B] = new SeqView.Map(this, f)
  override def appended[B >: A](elem: B): SeqView[B] = new SeqView.Appended(this, elem)
  override def prepended[B >: A](elem: B): SeqView[B] = new SeqView.Prepended(elem, this)
  override def reverse: SeqView[A] = new SeqView.Reverse(this)
  override def take(n: Int): SeqView[A] = new SeqView.Take(this, n)

  def concat[B >: A](suffix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(this, suffix)
  def appendedAll[B >: A](suffix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(this, suffix)
  def prependedAll[B >: A](prefix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(prefix, this)
}

object SeqView {

  /** A `SeqOps` whose collection type and collection type constructor are unknown */
  type SomeSeqOps[+A] = SeqOps[A, AnyConstr, _]

  /** A view that doesn’t apply any transformation to an underlying sequence */
  @SerialVersionUID(3L)
  class Id[+A](underlying: SeqOps[A, AnyConstr, _]) extends AbstractSeqView[A] {
    def apply(idx: Int): A = underlying.apply(idx)
    def length: Int = underlying.length
    def iterator: Iterator[A] = underlying.iterator
    override def knownSize: Int = underlying.knownSize
  }

  @SerialVersionUID(3L)
  class Map[+A, +B](underlying: SomeSeqOps[A], f: A => B) extends View.Map[A, B](underlying, f) with SeqView[B] {
    def apply(idx: Int): B = f(underlying(idx))
    def length: Int = underlying.length
  }

  @SerialVersionUID(3L)
  class Appended[+A](underlying: SomeSeqOps[A], elem: A) extends View.Appended(underlying, elem) with SeqView[A] {
    def apply(idx: Int): A = if (idx == underlying.length) elem else underlying(idx)
    def length: Int = underlying.length + 1
  }

  @SerialVersionUID(3L)
  class Prepended[+A](elem: A, underlying: SomeSeqOps[A]) extends View.Prepended(elem, underlying) with SeqView[A] {
    def apply(idx: Int): A = if (idx == 0) elem else underlying(idx - 1)
    def length: Int = underlying.length + 1
  }

  @SerialVersionUID(3L)
  class Concat[A](prefix: SomeSeqOps[A], suffix: SomeSeqOps[A]) extends View.Concat[A](prefix, suffix) with SeqView[A] {
    def apply(idx: Int): A = {
      val l = prefix.length
      if (idx < l) prefix(idx) else suffix(idx - l)
    }
    def length: Int = prefix.length + suffix.length
  }

  @SerialVersionUID(3L)
  class Reverse[A](underlying: SomeSeqOps[A]) extends AbstractSeqView[A] {
    def apply(i: Int) = underlying.apply(size - 1 - i)
    def length = underlying.size
    override def iterator: Iterator[A] = underlying.reverseIterator
  }

  @SerialVersionUID(3L)
  class Take[+A](underlying: SomeSeqOps[A], n: Int) extends View.Take(underlying, n) with SeqView[A] {
    def apply(idx: Int): A = if (idx < n) underlying(idx) else throw new IndexOutOfBoundsException(idx.toString)
    def length: Int = underlying.length min normN
  }
}

/** Explicit instantiation of the `SeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeqView[+A] extends AbstractView[A] with SeqView[A]
