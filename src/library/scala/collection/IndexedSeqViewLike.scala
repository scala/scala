/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Seq.scala 16092 2008-09-12 10:37:06Z nielsen $


package scala.collection

import generic._
import TraversableView.NoBuilder

/** A template trait for a non-strict view of a IndexedSeq.
 *
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 * @since   2.8
 */
trait IndexedSeqViewLike[+A,
                         +Coll,
                         +This <: IndexedSeqView[A, Coll] with IndexedSeqViewLike[A, Coll, This]]
  extends IndexedSeq[A]
      with IndexedSeqLike[A, This]
      with SeqView[A, Coll]
      with SeqViewLike[A, Coll, This]
      with views.IndexedSeqTransformations[A, Coll, This]
{ self =>

  trait Transformed[+B] extends views.IndexedSeqLike[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced
  trait Mapped[B] extends Transformed[B] with super.Mapped[B]
  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B]
  trait Appended[B >: A] extends Transformed[B] with super.Appended[B]
  trait Filtered extends Transformed[A] with super.Filtered
  trait TakenWhile extends Transformed[A] with super.TakenWhile
  trait DroppedWhile extends Transformed[A] with super.DroppedWhile
  trait Reversed extends Transformed[A] with super.Reversed
  trait Patched[B >: A] extends Transformed[B] with super.Patched[B]

  trait Zipped[B] extends Transformed[(A, B)] {
    protected[this] val other: Iterable[B]
    def length = self.length min other.size
    def apply(idx: Int): (A, B) = (self.apply(idx), other.iterator drop idx next)
    override def stringPrefix = self.stringPrefix+"Z"
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] {
    protected[this] val other: Iterable[B]
    val thisElem: A1
    val thatElem: B
    override def iterator: Iterator[(A1, B)] =
      self.iterator.zipAll(other.iterator, thisElem, thatElem)

    def length = self.length max other.size
    def apply(idx: Int): (A1, B) = {
      val z1 = if (idx < self.length) self.apply(idx) else thisElem
      val z2 = if (idx < other.size) other drop idx head else thatElem
      (z1, z2)
    }
    override def stringPrefix = self.stringPrefix+"Z"
  }
  override def stringPrefix = "IndexedSeqView"
}
