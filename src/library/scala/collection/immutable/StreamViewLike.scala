package scala
package collection
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

  trait TransformedM[+B] extends StreamView[B, Coll] with super.TransformedS[B] {
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private[collection] abstract class AbstractTransformedM[+B] extends super.AbstractTransformedS[B] with TransformedM[B]

  trait EmptyViewM extends TransformedM[Nothing] with super.EmptyViewS

  trait ForcedM[B] extends super.ForcedS[B] with TransformedM[B]

  trait SlicedM extends super.SlicedS with TransformedM[A]

  trait MappedM[B] extends super.MappedS[B] with TransformedM[B]

  trait FlatMappedM[B] extends super.FlatMappedS[B] with TransformedM[B]

  trait AppendedM[B >: A] extends super.AppendedS[B] with TransformedM[B]

  trait FilteredM extends super.FilteredS with TransformedM[A]

  trait TakenWhileM extends super.TakenWhileS with TransformedM[A]

  trait DroppedWhileM extends super.DroppedWhileS with TransformedM[A]

  trait ZippedM[B] extends super.ZippedS[B] with TransformedM[(A, B)]

  trait ZippedAllM[A1 >: A, B] extends super.ZippedAllS[A1, B] with TransformedM[(A1, B)]

  trait ReversedM extends super.ReversedS with TransformedM[A]

  trait PatchedM[B >: A] extends super.PatchedS[B] with TransformedM[B]

  trait PrependedM[B >: A] extends super.PrependedS[B] with TransformedM[B]

  /** boilerplate */
  protected override def newForced[B](xs: => scala.collection.GenSeq[B]): TransformedM[B] = new AbstractTransformedM[B] with ForcedM[B] { lazy val forced = xs }
  protected override def newAppended[B >: A](that: scala.collection.GenTraversable[B]): TransformedM[B] = new AbstractTransformedM[B] with AppendedM[B] { lazy val rest = that }
  protected override def newMapped[B](f: A => B): TransformedM[B] = new AbstractTransformedM[B] with MappedM[B] { lazy val mapping = f }
  protected override def newFlatMapped[B](f: A => scala.collection.GenTraversableOnce[B]): TransformedM[B] = new AbstractTransformedM[B] with FlatMappedM[B] { lazy val mapping = f }
  protected override def newFiltered(p: A => Boolean): TransformedM[A] = new AbstractTransformedM[A] with FilteredM { lazy val pred = p }
  protected override def newSliced(_endpoints: SliceInterval): TransformedM[A] = new AbstractTransformedM[A] with SlicedM { lazy val endpoints = _endpoints }
  protected override def newDroppedWhile(p: A => Boolean): TransformedM[A] = new AbstractTransformedM[A] with DroppedWhileM { lazy val pred = p }
  protected override def newTakenWhile(p: A => Boolean): TransformedM[A] = new AbstractTransformedM[A] with TakenWhileM { lazy val pred = p }
  protected override def newZipped[B](that: scala.collection.GenIterable[B]): TransformedM[(A, B)] = new AbstractTransformedM[(A, B)] with ZippedM[B] { lazy val other = that }
  protected override def newZippedAll[A1 >: A, B](that: scala.collection.GenIterable[B], _thisElem: A1, _thatElem: B): TransformedM[(A1, B)] = {
    new AbstractTransformedM[(A1, B)] with ZippedAllM[A1, B] { lazy val other = that; lazy val thisElem = _thisElem; lazy val thatElem = _thatElem }
  }
  protected override def newReversed: TransformedM[A] = new ReversedM { }
  protected override def newPatched[B >: A](_from: Int, _patch: scala.collection.GenSeq[B], _replaced: Int): TransformedM[B] = {
    new AbstractTransformedM[B] with PatchedM[B] { lazy val from = _from; lazy val patch = _patch; lazy val replaced = _replaced }
  }
  protected override def newPrepended[B >: A](elem: B): TransformedM[B] = new AbstractTransformedM[B] with PrependedM[B] { lazy protected[this] val fst = elem }

  override def stringPrefix = "StreamView"
}
