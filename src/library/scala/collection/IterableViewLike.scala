/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

import generic._
import immutable.Stream
import scala.language.implicitConversions

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
     extends Iterable[A]
        with IterableLike[A, This]
        with TraversableView[A, Coll]
        with TraversableViewLike[A, Coll, This]
{ self =>

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private[collection] abstract class AbstractTransformed[+B] extends Iterable[B] with super[TraversableViewLike].Transformed[B] with Transformed[B]

  trait Transformed[+B] extends IterableView[B, Coll] with super.Transformed[B] {
    def iterator: Iterator[B]
    override def foreach[U](f: B => U): Unit = iterator foreach f
    override def toString = viewToString
    override def isEmpty = !iterator.hasNext
  }

  trait EmptyView extends Transformed[Nothing] with super.EmptyView {
    final def iterator: Iterator[Nothing] = Iterator.empty
  }

  trait Forced[B] extends super.Forced[B] with Transformed[B] {
    def iterator = forced.iterator
  }

  trait Sliced extends super.Sliced with Transformed[A] {
    def iterator: Iterator[A] = self.iterator.slice(from, until)
  }

  trait Mapped[B] extends super.Mapped[B] with Transformed[B] {
    def iterator = self.iterator map mapping
  }

  trait FlatMapped[B] extends super.FlatMapped[B] with Transformed[B] {
    def iterator: Iterator[B] = self.iterator flatMap mapping
  }

  trait Appended[B >: A] extends super.Appended[B] with Transformed[B] {
    def iterator = self.iterator ++ rest
  }
  
  trait Prepended[B >: A] extends super.Prepended[B] with Transformed[B] {
    def iterator = fst.toIterator ++ self
  }

  trait Filtered extends super.Filtered with Transformed[A] {
    def iterator = self.iterator filter pred
  }

  trait TakenWhile extends super.TakenWhile with Transformed[A] {
    def iterator = self.iterator takeWhile pred
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[A] {
    def iterator = self.iterator dropWhile pred
  }

  trait Zipped[B] extends Transformed[(A, B)] {
    protected[this] val other: GenIterable[B]
    def iterator: Iterator[(A, B)] = self.iterator zip other.iterator
    final override protected[this] def viewIdentifier = "Z"
  }

  trait ZippedAll[A1 >: A, B] extends Transformed[(A1, B)] {
    protected[this] val other: GenIterable[B]
    protected[this] val thisElem: A1
    protected[this] val thatElem: B
    final override protected[this] def viewIdentifier = "Z"
    def iterator: Iterator[(A1, B)] =
      self.iterator.zipAll(other.iterator, thisElem, thatElem)
  }

  private[this] implicit def asThis(xs: Transformed[A]): This = xs.asInstanceOf[This]

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected def newZipped[B](that: GenIterable[B]): Transformed[(A, B)] = new { val other = that } with AbstractTransformed[(A, B)] with Zipped[B]
  protected def newZippedAll[A1 >: A, B](that: GenIterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new {
    val other: GenIterable[B] = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  } with AbstractTransformed[(A1, B)] with ZippedAll[A1, B]
  protected override def newForced[B](xs: => GenSeq[B]): Transformed[B] = new { val forced = xs } with AbstractTransformed[B] with Forced[B]
  protected override def newAppended[B >: A](that: GenTraversable[B]): Transformed[B] = new { val rest = that } with AbstractTransformed[B] with Appended[B]
  protected override def newPrepended[B >: A](that: GenTraversable[B]): Transformed[B] = new { val fst = that } with AbstractTransformed[B] with Prepended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with Mapped[B]
  protected override def newFlatMapped[B](f: A => GenTraversableOnce[B]): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with Filtered
  protected override def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with AbstractTransformed[A] with Sliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with TakenWhile

  // After adding take and drop overrides to IterableLike, these overrides (which do nothing
  // but duplicate the implementation in TraversableViewLike) had to be added to prevent the
  // overrides in IterableLike from besting the overrides in TraversableViewLike when mixed
  // together in e.g. SeqViewLike.  This is a suboptimal situation.  Examples of failing tests
  // are run/bug2876 and run/viewtest.
  protected override def newTaken(n: Int): Transformed[A] = newSliced(SliceInterval(0, n))
  protected override def newDropped(n: Int): Transformed[A] = newSliced(SliceInterval(n, Int.MaxValue))
  override def drop(n: Int): This = newDropped(n)
  override def take(n: Int): This = newTaken(n)

  override def zip[A1 >: A, B, That](that: GenIterable[B])(implicit bf: CanBuildFrom[This, (A1, B), That]): That = {
    newZipped(that).asInstanceOf[That]
// was:    val b = bf(repr)
//    if (b.isInstanceOf[NoBuilder[_]]) newZipped(that).asInstanceOf[That]
//    else super.zip[A1, B, That](that)(bf)
  }

  override def zipWithIndex[A1 >: A, That](implicit bf: CanBuildFrom[This, (A1, Int), That]): That =
    zip[A1, Int, That](Stream from 0)(bf)

  override def zipAll[B, A1 >: A, That](that: GenIterable[B], thisElem: A1, thatElem: B)(implicit bf: CanBuildFrom[This, (A1, B), That]): That =
    newZippedAll(that, thisElem, thatElem).asInstanceOf[That]

  override def grouped(size: Int): Iterator[This] =
    self.iterator grouped size map (x => newForced(x).asInstanceOf[This])

  override def sliding(size: Int, step: Int): Iterator[This] =
    self.iterator.sliding(size, step) map (x => newForced(x).asInstanceOf[This])

  override def sliding(size: Int): Iterator[This] =
    sliding(size, 1) // we could inherit this, but that implies knowledge of the way the super class is implemented.

  override def dropRight(n: Int): This =
    take(thisSeq.length - math.max(n, 0))

  override def takeRight(n: Int): This =
    drop(thisSeq.length - math.max(n, 0))

  override def stringPrefix = "IterableView"
}
