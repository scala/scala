package scala.collection
package immutable

import generic._

trait StreamViewLike[+A,
		     +Coll,
		     +This <: StreamView[A, Coll] with StreamViewLike[A, Coll, This]]
extends SeqView[A, Coll]
   with SeqViewLike[A, Coll, This]
{ self =>

  override def force[B >: A, That](implicit bf: CanBuildFrom[Coll, B, That]) = {
    self.iterator.toStream.asInstanceOf[That]
  }

  trait Transformed[+B] extends StreamView[B, Coll] with super.Transformed[B] {
    override def toString = viewToString
  }

  trait EmptyView extends Transformed[Nothing] with super.EmptyView { }

  trait Forced[B] extends super.Forced[B] with Transformed[B] { }

  trait Sliced extends super.Sliced with Transformed[A] { }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B]

  trait FlatMapped[B] extends super.FlatMapped[B] with Transformed[B]

  trait Appended[B >: A] extends super.Appended[B] with Transformed[B]

  trait Filtered extends super.Filtered with Transformed[A]

  trait TakenWhile extends super.TakenWhile with Transformed[A]

  trait DroppedWhile extends super.DroppedWhile with Transformed[A]

  trait Zipped[B] extends super.Zipped[B] with Transformed[(A, B)]

  trait ZippedAll[A1 >: A, B] extends super.ZippedAll[A1, B] with Transformed[(A1, B)]

  trait Reversed extends super.Reversed with Transformed[A]

  trait Patched[B >: A] extends super.Patched[B] with Transformed[B]

  trait Prepended[B >: A] extends super.Prepended[B] with Transformed[B]

  /** boilerplate */
  protected override def newForced[B](xs: => collection.GenSeq[B]): Transformed[B] = new { val forced = xs } with Forced[B]
  protected override def newAppended[B >: A](that: collection.GenTraversable[B]): Transformed[B] = new { val rest = that } with Appended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with Mapped[B]
  protected override def newFlatMapped[B](f: A => collection.GenTraversableOnce[B]): Transformed[B] = new { val mapping = f } with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with Filtered
  protected override def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with Sliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with TakenWhile
  protected override def newZipped[B](that: collection.GenIterable[B]): Transformed[(A, B)] = new { val other = that } with Zipped[B]
  protected override def newZippedAll[A1 >: A, B](that: collection.GenIterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = {
    new { val other = that; val thisElem = _thisElem; val thatElem = _thatElem } with ZippedAll[A1, B]
  }
  protected override def newReversed: Transformed[A] = new Reversed { }
  protected override def newPatched[B >: A](_from: Int, _patch: collection.GenSeq[B], _replaced: Int): Transformed[B] = {
    new { val from = _from; val patch = _patch; val replaced = _replaced } with Patched[B]
  }
  protected override def newPrepended[B >: A](elem: B): Transformed[B] = new { protected[this] val fst = elem } with Prepended[B]

  override def stringPrefix = "StreamView"
}






