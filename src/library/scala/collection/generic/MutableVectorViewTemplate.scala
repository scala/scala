/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic

import TraversableView.NoBuilder

/** A non-strict projection of an iterable.
 * @author Sean McDirmid
 * @author Martin Odersky
 * @version 2.8
 */
trait MutableVectorViewTemplate[A,
                                +Coll <: mutable.Vector[_],
                                +This <: MutableVectorView[A, Coll] with MutableVectorViewTemplate[A, Coll, This]]
  extends mutable.Vector[A] with MutableVectorTemplate[A, This] with VectorView[A, Coll] with VectorViewTemplate[A, Coll, This]
{ self =>

  trait Transformed[B] extends MutableVectorView[B, Coll] with super.Transformed[B] {
    def update(idx: Int, elem: B)
  }

  trait Sliced extends Transformed[A] with super.Sliced {
    override def update(idx: Int, elem: A) =
      if (idx + from < until) self.update(idx + from, elem)
      else throw new IndexOutOfBoundsException(idx.toString)
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
}

