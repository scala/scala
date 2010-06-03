package scala.collection
package immutable



import scala.collection.generic.CanBuildFrom





trait StreamViewLike[+A,
		     +Coll,
		     +This <: StreamView[A, Coll] with StreamViewLike[A, Coll, This]]
extends SeqView[A, Coll]
   with SeqViewLike[A, Coll, This]
{ self =>

  override def force[B >: A, That](implicit bf: CanBuildFrom[Coll, B, That]) = {
    this.iterator.toStream.asInstanceOf[That]
  }

  trait Transformed[+B] extends StreamView[B, Coll] with super.Transformed[B]

  trait Forced[B] extends Transformed[B] with super.Forced[B]

  trait Sliced extends Transformed[A] with super.Sliced

  trait Mapped[B] extends Transformed[B] with super.Mapped[B]

  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B]

  trait Appended[B >: A] extends Transformed[B] with super.Appended[B]

  trait Filtered extends Transformed[A] with super.Filtered

  trait TakenWhile extends Transformed[A] with super.TakenWhile

  trait DroppedWhile extends Transformed[A] with super.DroppedWhile

  trait Zipped[B] extends Transformed[(A, B)] with super.Zipped[B]

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] with super.ZippedAll[A1, B]

  trait Reversed extends Transformed[A] with super.Reversed

  trait Patched[B >: A] extends Transformed[B] with super.Patched[B]

  trait Prepended[B >: A] extends Transformed[B] with super.Prepended[B]

  /** boilerplate */
  protected override def newForced[B](xs: => collection.Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newAppended[B >: A](that: collection.Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => collection.Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected override def newZipped[B](that: collection.Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
  protected override def newZippedAll[A1 >: A, B](that: collection.Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = {
    new ZippedAll[A1, B] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }
  }
  protected override def newReversed: Transformed[A] = new Reversed { }
  protected override def newPatched[B >: A](_from: Int, _patch: collection.Seq[B], _replaced: Int): Transformed[B] = {
    new Patched[B] { val from = _from; val patch = _patch; val replaced = _replaced }
  }
  protected override def newPrepended[B >: A](elem: B): Transformed[B] = new Prepended[B] { protected[this] val fst = elem }

}






