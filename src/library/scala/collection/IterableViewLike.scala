/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection

import generic._
import collection.immutable.Stream
import TraversableView.NoBuilder

/** A template trait for non-strict views of iterable collections.
 *  $iterableViewInfo
 *
 *  @define iterableViewInfo
 *  $viewInfo
 *  All views for iterable collections are defined by re-interpreting the `iterator` method.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 *  @tparam This the type of the view itself
 */
trait IterableViewLike[+A,
                       +Coll,
                       +This <: IterableView[A, Coll] with IterableViewLike[A, Coll, This]]
extends Iterable[A] with IterableLike[A, This] with TraversableView[A, Coll] with TraversableViewLike[A, Coll, This]
{ self =>

  trait Transformed[+B] extends IterableView[B, Coll] with super.Transformed[B]

  trait Forced[B] extends Transformed[B] with super.Forced[B] {
    override def iterator = forced.iterator
  }

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

  trait Zipped[B] extends Transformed[(A, B)] {
    protected[this] val other: Iterable[B]
    override def iterator: Iterator[(A, B)] = self.iterator zip other.iterator
    override def stringPrefix = self.stringPrefix+"Z"
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] {
    protected[this] val other: Iterable[B]
    protected[this] val thisElem: A1
    protected[this] val thatElem: B
    override def iterator: Iterator[(A1, B)] =
      self.iterator.zipAll(other.iterator, thisElem, thatElem)
  }

  override def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: CanBuildFrom[This, (A1, B), That]): That = {
    newZipped(that).asInstanceOf[That]
// was:    val b = bf(repr)
//    if (b.isInstanceOf[NoBuilder[_]]) newZipped(that).asInstanceOf[That]
//    else super.zip[A1, B, That](that)(bf)
  }

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[This, (A1, Int), That]): That =
    zip[A1, Int, That](Stream from 0)(bf)

  override def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[This, (A1, B), That]): That =
    newZippedAll(that, thisElem, thatElem).asInstanceOf[That]

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected def newZipped[B](that: Iterable[B]): Transformed[(A, B)] = new Zipped[B] {
    val other = that
  }
  protected def newZippedAll[A1 >: A, B](that: Iterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new ZippedAll[A1, B] {
    val other: Iterable[B] = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }
  protected override def newForced[B](xs: => Seq[B]): Transformed[B] = new Forced[B] { val forced = xs }
  protected override def newAppended[B >: A](that: Traversable[B]): Transformed[B] = new Appended[B] { val rest = that }
  protected override def newMapped[B](f: A => B): Transformed[B] = new Mapped[B] { val mapping = f }
  protected override def newFlatMapped[B](f: A => Traversable[B]): Transformed[B] = new FlatMapped[B] { val mapping = f }
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new Filtered { val pred = p }
  protected override def newSliced(_from: Int, _until: Int): Transformed[A] = new Sliced { val from = _from; val until = _until }
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new TakenWhile { val pred = p }

  override def grouped(size: Int): Iterator[This] =
    self.iterator.grouped(size).map(xs => newForced(xs).asInstanceOf[This])

  override def sliding[B >: A](size: Int, step: Int): Iterator[This] =
    self.iterator.sliding(size).map(xs => newForced(xs).asInstanceOf[This])

  override def stringPrefix = "IterableView"
}
