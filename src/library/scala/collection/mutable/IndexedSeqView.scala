/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

import TraversableView.NoBuilder

/** A non-strict view of a mutable IndexedSeq.
 *  This is a leaf class which mixes methods returning a plain IndexedSeq view
 *  and methods returning a mutable IndexedSeq view.
 *  There is no associated `Like' class.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 * @since   2.8
 */
trait IndexedSeqView[A, +Coll] extends IndexedSeq[A]
                                  with IndexedSeqOptimized[A, IndexedSeqView[A, Coll]]
                                  with scala.collection.SeqView[A, Coll]
                                  with scala.collection.SeqViewLike[A, Coll, IndexedSeqView[A, Coll]] {
self =>

  def update(idx: Int, elem: A)

  trait Transformed[B] extends IndexedSeqView[B, Coll] with super.Transformed[B] {
    def update(idx: Int, elem: B)
  }

  trait Sliced extends Transformed[A] with super.Sliced {
    override def update(idx: Int, elem: A) =
      if (idx + from < until) self.update(idx + from, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
    override def slice(from1: Int, until1: Int): Transformed[A] =
      newSliced(from1 max 0, until1 max 0)
  }

  trait Filtered extends Transformed[A] with super.Filtered {
    override def update(idx: Int, elem: A) = self.update(index(idx), elem)
  }

  trait TakenWhile extends Transformed[A] with super.TakenWhile {
    override def update(idx: Int, elem: A) =
      if (idx < len) self.update(idx, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait DroppedWhile extends Transformed[A] with super.DroppedWhile {
    override def update(idx: Int, elem: A) =
      if (idx >= 0) self.update(idx + start, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
  }

  trait Reversed extends Transformed[A] with super.Reversed {
    override def update(idx: Int, elem: A) = self.update(length - 1 - idx, elem)
  }

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }
  protected override def newReversed: Transformed[A] = new Reversed { }

  // Todo: if we replace IndexedSeqView[A, Coll] below by
  // private[this] type This = IndexedSeqView[A, Coll]
  // The interpreter will display resX.This.
  // It shouldn't.

  override def filter(p: A => Boolean): IndexedSeqView[A, Coll] = newFiltered(p)
  override def init: IndexedSeqView[A, Coll]  = newSliced(0, size - 1).asInstanceOf[IndexedSeqView[A, Coll]]
  override def drop(n: Int): IndexedSeqView[A, Coll] = newSliced(n max 0, Int.MaxValue).asInstanceOf[IndexedSeqView[A, Coll]]
  override def take(n: Int): IndexedSeqView[A, Coll] = newSliced(0, n).asInstanceOf[IndexedSeqView[A, Coll]]
  override def slice(from: Int, until: Int): IndexedSeqView[A, Coll] = newSliced(from max 0, until).asInstanceOf[IndexedSeqView[A, Coll]]
  override def dropWhile(p: A => Boolean): IndexedSeqView[A, Coll] = newDroppedWhile(p).asInstanceOf[IndexedSeqView[A, Coll]]
  override def takeWhile(p: A => Boolean): IndexedSeqView[A, Coll] = newTakenWhile(p).asInstanceOf[IndexedSeqView[A, Coll]]
  override def span(p: A => Boolean): (IndexedSeqView[A, Coll], IndexedSeqView[A, Coll]) = (takeWhile(p), dropWhile(p))
  override def splitAt(n: Int): (IndexedSeqView[A, Coll], IndexedSeqView[A, Coll]) = (take(n), drop(n))
  override def reverse: IndexedSeqView[A, Coll] = newReversed.asInstanceOf[IndexedSeqView[A, Coll]]
}

/** $factoryInfo
 * Note that the canBuildFrom factories yield SeqViews, not IndexedSewqViews.
 * This is intentional, because not all operations yield again a mutable.IndexedSeqView.
 * For instance, map just gives a SeqView, which reflects the fact that
 * map cannot do its work and maintain a pointer into the original indexed sequence.
 */
object IndexedSeqView {
  type Coll = TraversableView[_, C] forSome {type C <: Traversable[_]}
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] =
    new CanBuildFrom[Coll, A, SeqView[A, Seq[_]]] {
      def apply(from: Coll) = new NoBuilder
      def apply() = new NoBuilder
    }
  implicit def arrCanBuildFrom[A]: CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] =
    new CanBuildFrom[TraversableView[_, Array[_]], A, SeqView[A, Array[A]]] {
      def apply(from: TraversableView[_, Array[_]]) = new NoBuilder
      def apply() = new NoBuilder
    }
}
