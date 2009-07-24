/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.generic
import scala.collection._

import TraversableView.NoBuilder

/** A base class for views of Iterables.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableViewTemplate[+A,
                           +Coll <: Iterable[_],
                           +This <: IterableView[A, Coll] with IterableViewTemplate[A, Coll, This]]
extends Iterable[A] with IterableTemplate[A, This] with TraversableView[A, Coll] with TraversableViewTemplate[A, Coll, This]
{ self =>

  trait Transformed[+B] extends IterableView[B, Coll] with super.Transformed[B]

  trait Sliced extends Transformed[A] with super.Sliced {
    override def iterator = self.iterator slice (from, until)
  }

  trait Mapped[B] extends Transformed[B] with super.Mapped[B] {
    override def iterator = self.iterator map mapping
  }

  trait FlatMapped[B] extends Transformed[B] with super.FlatMapped[B] {
    override def iterator = self.iterator flatMap (mapping(_).toIterable.iterator)
  }

  trait Appended[B >: A] extends Transformed[B] with super.Appended[B] {
    override def iterator = self.iterator ++ rest.toIterable.iterator
  }

  trait Filtered extends Transformed[A] with super.Filtered {
    override def iterator = self.iterator filter pred
  }

  trait TakenWhile extends Transformed[A] with super.TakenWhile {
    override def iterator = self.iterator takeWhile pred
  }

  trait DroppedWhile extends Transformed[A] with super.DroppedWhile {
    override def iterator = self.iterator dropWhile pred
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
}
