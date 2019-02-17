/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection


trait SeqView[+A] extends SeqOps[A, View, View[A]] with View[A] {

  override def view: SeqView[A] = this

  override def map[B](f: A => B): SeqView[B] = new SeqView.Map(this, f)
  override def appended[B >: A](elem: B): SeqView[B] = new SeqView.Appended(this, elem)
  override def prepended[B >: A](elem: B): SeqView[B] = new SeqView.Prepended(elem, this)
  override def reverse: SeqView[A] = new SeqView.Reverse(this)
  override def take(n: Int): SeqView[A] = new SeqView.Take(this, n)
  override def drop(n: Int): SeqView[A] = new SeqView.Drop(this, n)
  override def takeRight(n: Int): SeqView[A] = new SeqView.TakeRight(this, n)
  override def dropRight(n: Int): SeqView[A] = new SeqView.DropRight(this, n)

  def concat[B >: A](suffix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(this, suffix)
  def appendedAll[B >: A](suffix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(this, suffix)
  def prependedAll[B >: A](prefix: SeqView.SomeSeqOps[B]): SeqView[B] = new SeqView.Concat(prefix, this)

  override def sorted[B >: A](implicit ord: Ordering[B]): SeqView[A] = new SeqView.Sorted(this, ord)
}

object SeqView {

  /** A `SeqOps` whose collection type and collection type constructor are unknown */
  private type SomeSeqOps[+A] = SeqOps[A, AnyConstr, _]

  /** A view that doesn’t apply any transformation to an underlying sequence */
  @SerialVersionUID(3L)
  class Id[+A](underlying: SomeSeqOps[A]) extends AbstractSeqView[A] {
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
    def apply(idx: Int): A = if (idx < n) {
      underlying(idx)
    } else {
      throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${if (underlying.knownSize >= 0) knownSize - 1 else "unknown"})")
    }
    def length: Int = underlying.length min normN
  }

  @SerialVersionUID(3L)
  class TakeRight[+A](underlying: SomeSeqOps[A], n: Int) extends View.TakeRight(underlying, n) with SeqView[A] {
    private[this] val delta = (underlying.size - (n max 0)) max 0
    def length = underlying.size - delta
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + delta)
  }

  @SerialVersionUID(3L)
  class Drop[A](underlying: SomeSeqOps[A], n: Int) extends View.Drop[A](underlying, n) with SeqView[A] {
    def length = (underlying.size - normN) max 0
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i + normN)
  }

  @SerialVersionUID(3L)
  class DropRight[A](underlying: SomeSeqOps[A], n: Int) extends View.DropRight[A](underlying, n) with SeqView[A] {
    private[this] val len = (underlying.size - (n max 0)) max 0
    def length = len
    @throws[IndexOutOfBoundsException]
    def apply(i: Int) = underlying.apply(i)
  }

  @SerialVersionUID(3L)
  class Sorted[A, B >: A](underlying: SomeSeqOps[A], ord: Ordering[B]) extends SeqView[A] {
    private[this] lazy val _sorted: Seq[A] = {
      val len = underlying.length
      if (len == 0) Nil
      else if (len == 1) List(underlying.head)
      else {
        val arr = new Array[Any](len) // Array[Any] =:= Array[AnyRef]
        underlying.copyToArray(arr)
        java.util.Arrays.sort(arr.asInstanceOf[Array[AnyRef]], ord.asInstanceOf[Ordering[AnyRef]])
        // casting the Array[AnyRef] to Array[A] and creating an ArraySeq from it
        // is safe because:
        //   - the ArraySeq is immutable, and items that are not of type A
        //     cannot be added to it
        //   - we know it only contains items of type A (and if this collection
        //     contains items of another type, we'd get a CCE anyway)
        //   - the cast doesn't actually do anything in the runtime because the
        //     type of A is not known and Array[_] is Array[AnyRef]
        immutable.ArraySeq.unsafeWrapArray(arr.asInstanceOf[Array[A]])
      }
    }

    def apply(i: Int): A = _sorted.apply(i)
    def length: Int = underlying.length
    def iterator: Iterator[A] = Iterator.empty ++ _sorted.iterator // very lazy
    override def knownSize: Int = underlying.knownSize
    override def isEmpty: Boolean = underlying.isEmpty
    override def to[C1](factory: Factory[A, C1]): C1 = _sorted.to(factory)

    override def sorted[B1 >: A](implicit ord1: Ordering[B1]): SeqView[A] =
      if (ord1 == this.ord) this else super.sorted(ord1)
  }
}

/** Explicit instantiation of the `SeqView` trait to reduce class file size in subclasses. */
@SerialVersionUID(3L)
abstract class AbstractSeqView[+A] extends AbstractView[A] with SeqView[A]
