/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Sequence.scala 16092 2008-09-12 10:37:06Z nielsen $


package scala.collection

import generic._
import TraversableView.NoBuilder

/** A non-strict projection of an iterable.
 *
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 */
trait VectorViewLike[+A,
                         +Coll,
                         +This <: VectorView[A, Coll] with VectorViewLike[A, Coll, This]]
  extends Vector[A] with VectorLike[A, This] with SequenceView[A, Coll] with SequenceViewLike[A, Coll, This]
{ self =>

  trait Transformed[+B] extends VectorView[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced {
    /** Override to use Vector's foreach; todo: see whether this is really faster */
    override def foreach[U](f: A =>  U) = super[Transformed].foreach(f)
  }

  trait Mapped[B] extends Transformed[B] with super.Mapped[B] {
    override def foreach[U](f: B =>  U) = super[Transformed].foreach(f)
  }

  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B] {
    override def foreach[U](f: B =>  U) = super[Transformed].foreach(f)
  }

  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    override def foreach[U](f: B =>  U) = super[Transformed].foreach(f)
  }

  trait Filtered extends Transformed[A] with super.Filtered {
    override def foreach[U](f: A =>  U) = super[Transformed].foreach(f)
  }

  trait TakenWhile extends Transformed[A] with super.TakenWhile {
    override def foreach[U](f: A =>  U) = super[Transformed].foreach(f)
  }

  trait DroppedWhile extends Transformed[A] with super.DroppedWhile {
    override def foreach[U](f: A =>  U) = super[Transformed].foreach(f)
  }

  trait Reversed extends Transformed[A] with super.Reversed {
    override def foreach[U](f: A =>  U) = super[Transformed].foreach(f)
  }

  trait Patched[B >: A] extends Transformed[B] with super.Patched[B] {
    override def foreach[U](f: B =>  U) = super[Transformed].foreach(f)
  }

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

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected override def newReversed: Transformed[A] = new Reversed { }
  protected override def newPatched[B >: A](_from: Int, _patch: Sequence[B], _replaced: Int): Transformed[B] = new Patched[B] {
    val from = _from; val patch = _patch; val replaced = _replaced
  }
  protected override def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] {
    val other = that
  }
  protected override def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] {
    val other: Iterable[B] = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }
  override def stringPrefix = "VectorView"
}
