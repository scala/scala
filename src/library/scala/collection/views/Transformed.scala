/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package views

import generic.CanBuildFrom

/** These classes act as accumulators for the majority of methods in the
 *  collections hierarchy.  By creating abstract classes rather than using
 *  the traits exclusively, we avoid creating forwarders in dozens of distinct
 *  anonymous classes and reduce the size of scala-library.jar by over 200K.
 */
private[collection] trait Transformed
private[collection] abstract class TraversableLike[+B, +Coll] extends TraversableView[B, Coll] with Transformed {
  override def foreach[C](f: B => C): Unit
}
private[collection] abstract class IterableLike[+B, +Coll] extends TraversableLike[B, Coll] with IterableView[B, Coll] {
  override def iterator: Iterator[B]
}
private[collection] abstract class SeqLike[+B, +Coll] extends IterableLike[B, Coll] with SeqView[B, Coll] {
  override def length: Int
  override def apply(idx: Int): B
}
private[collection] abstract class IndexedSeqLike[+B, +Coll] extends SeqLike[B, Coll] with IndexedSeqView[B, Coll] {
  /** Override to use IndexedSeq's foreach; todo: see whether this is really faster */
  override def foreach[U](f: B => U) = super[IndexedSeqView].foreach(f)
}
private[collection] abstract class MutableIndexedSeq[B, +Coll] extends IndexedSeqLike[B, Coll] {
  def update(idx: Int, elem: B)
}

/** The boilerplate in the following traits factored out of the *ViewLike classes
 *  to reduce noise.  It exists only to specialize the return type of each method.
 *  It would be unnecessary if scala had virtual classes because the inner classes
 *  of subtraits would subclass the parent trait inner classes, and the same method
 *  would then suffice for both.
 */
private[collection] trait TraversableTransformations[+A, +Coll, +This <: TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]] {
  self: TraversableViewLike[A, Coll, This] =>

  /** Boilerplate methods, to override in each subclass. */
  protected def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
}

private[collection] trait IterableTransformations[+A, +Coll, +This <: IterableView[A, Coll] with IterableViewLike[A, Coll, This]]
  extends TraversableTransformations[A, Coll, This]
{
  self: IterableViewLike[A, Coll, This] =>

  /** Inherited from TraversableView */
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

  /** IterableView boilerplate contribution */
  protected def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] {
    val other = that
  }
  protected def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] {
    val other: Iterable[B] = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }
}

private[collection] trait SeqTransformations[+A, +Coll, +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends IterableTransformations[A, Coll, This]
{
  self: SeqViewLike[A, Coll, This] =>

  /** Inherited from IterableView */
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] =
    new ZippedAll[A1, B] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }

  /** SeqView boilerplate contribution */
  protected def newReversed: Transformed[A] = new Reversed { }
  protected def newPatched[B >: A](_from: Int, _patch: Seq[B], _replaced: Int): Transformed[B] =
    new Patched[B] { val from = _from; val patch = _patch; val replaced = _replaced }
}

private[collection] trait IndexedSeqTransformations[+A, +Coll, +This <: IndexedSeqView[A, Coll] with IndexedSeqViewLike[A, Coll, This]]
  extends SeqTransformations[A, Coll, This]
{
  self: IndexedSeqViewLike[A, Coll, This] =>

  /** Inherited from SeqView */
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] { val other = that }
  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] =
    new ZippedAll[A1, B] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }
  protected override def newReversed: Transformed[A] = new Reversed { }
  protected override def newPatched[B >: A](_from: Int, _patch: Seq[B], _replaced: Int): Transformed[B] =
    new Patched[B] { val from = _from; val patch = _patch; val replaced = _replaced }
}
