/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection

import generic._
import Seq.fill
import TraversableView.NoBuilder

/** A template trait for non-strict views of sequences.
 *  $seqViewInfo
 *
 *  @define seqViewInfo
 *  $viewInfo
 *  All views for sequences are defined by re-interpreting the `length` and
 * `apply` methods.
 *
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 *  @tparam A    the element type of the view
 *  @tparam Coll the type of the underlying collection containing the elements.
 *  @tparam This the type of the view itself
 */
trait SeqViewLike[+A,
                  +Coll,
                  +This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends Seq[A] with SeqLike[A, This] with IterableView[A, Coll] with IterableViewLike[A, Coll, This] with GenSeqViewLike[A, Coll, This]
{ self =>

  trait Transformed[+B] extends SeqView[B, Coll] with super[IterableViewLike].Transformed[B] with super[GenSeqViewLike].Transformed[B] {
    def length: Int
    def apply(idx: Int): B
    override def toString = viewToString
  }

  /** Explicit instantiation of the `Transformed` trait to reduce class file size in subclasses. */
  private[collection] abstract class AbstractTransformed[+B] extends Seq[B] with super[IterableViewLike].Transformed[B] with Transformed[B]

  trait EmptyView extends Transformed[Nothing] with super[IterableViewLike].EmptyView with super[GenSeqViewLike].EmptyView

  trait Forced[B] extends super[IterableViewLike].Forced[B] with super[GenSeqViewLike].Forced[B] with Transformed[B]

  trait Sliced extends super[IterableViewLike].Sliced with super[GenSeqViewLike].Sliced with Transformed[A]

  trait Mapped[B] extends super[IterableViewLike].Mapped[B] with super[GenSeqViewLike].Mapped[B] with Transformed[B]

  trait FlatMapped[B] extends super[IterableViewLike].FlatMapped[B] with super[GenSeqViewLike].FlatMapped[B] with Transformed[B]

  trait Appended[B >: A] extends super[IterableViewLike].Appended[B] with super[GenSeqViewLike].Appended[B] with Transformed[B]

  trait Filtered extends super[IterableViewLike].Filtered with super[GenSeqViewLike].Filtered with Transformed[A]

  trait TakenWhile extends super[IterableViewLike].TakenWhile with super[GenSeqViewLike].TakenWhile with Transformed[A]

  trait DroppedWhile extends super[IterableViewLike].DroppedWhile with super[GenSeqViewLike].DroppedWhile with Transformed[A]

  trait Zipped[B] extends super[IterableViewLike].Zipped[B] with super[GenSeqViewLike].Zipped[B] with Transformed[(A, B)]

  trait ZippedAll[A1 >: A, B] extends super[IterableViewLike].ZippedAll[A1, B] with super[GenSeqViewLike].ZippedAll[A1, B] with Transformed[(A1, B)]

  trait Reversed extends Transformed[A] with super[GenSeqViewLike].Reversed

  trait Patched[B >: A] extends Transformed[B] with super[GenSeqViewLike].Patched[B]

  trait Prepended[B >: A] extends Transformed[B] with super[GenSeqViewLike].Prepended[B]

  /** Boilerplate method, to override in each subclass
   *  This method could be eliminated if Scala had virtual classes
   */
  protected override def newForced[B](xs: => GenSeq[B]): Transformed[B] = new { val forced = xs } with AbstractTransformed[B] with Forced[B]
  protected override def newAppended[B >: A](that: GenTraversable[B]): Transformed[B] = new { val rest = that } with AbstractTransformed[B] with Appended[B]
  protected override def newMapped[B](f: A => B): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with Mapped[B]
  protected override def newFlatMapped[B](f: A => GenTraversableOnce[B]): Transformed[B] = new { val mapping = f } with AbstractTransformed[B] with FlatMapped[B]
  protected override def newFiltered(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with Filtered
  protected override def newSliced(_endpoints: SliceInterval): Transformed[A] = new { val endpoints = _endpoints } with AbstractTransformed[A] with Sliced
  protected override def newDroppedWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with DroppedWhile
  protected override def newTakenWhile(p: A => Boolean): Transformed[A] = new { val pred = p } with AbstractTransformed[A] with TakenWhile
  protected override def newZipped[B](that: GenIterable[B]): Transformed[(A, B)] = new { val other = that } with AbstractTransformed[(A, B)] with Zipped[B]
  protected override def newZippedAll[A1 >: A, B](that: GenIterable[B], _thisElem: A1, _thatElem: B): Transformed[(A1, B)] = new {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  } with AbstractTransformed[(A1, B)] with ZippedAll[A1, B]
  protected def newReversed: Transformed[A] = new AbstractTransformed[A] with Reversed
  protected def newPatched[B >: A](_from: Int, _patch: GenSeq[B], _replaced: Int): Transformed[B] = new {
    val from = _from
    val patch = _patch
    val replaced = _replaced
  } with AbstractTransformed[B] with Patched[B]
  protected def newPrepended[B >: A](elem: B): Transformed[B] = new { protected[this] val fst = elem } with AbstractTransformed[B] with Prepended[B]

  // see comment in IterableViewLike.
  protected override def newTaken(n: Int): Transformed[A] = newSliced(SliceInterval(0, n))
  protected override def newDropped(n: Int): Transformed[A] = newSliced(SliceInterval(n, Int.MaxValue))

  override def reverse: This = newReversed.asInstanceOf[This]

  override def patch[B >: A, That](from: Int, patch: GenSeq[B], replaced: Int)(implicit bf: CanBuildFrom[This, B, That]): That = {
    newPatched(from, patch, replaced).asInstanceOf[That]
// was:    val b = bf(repr)
//    if (b.isInstanceOf[NoBuilder[_]]) newPatched(from, patch, replaced).asInstanceOf[That]
//    else super.patch[B, That](from, patch, replaced)(bf)
  }

  override def padTo[B >: A, That](len: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    patch(length, fill(len - length)(elem), 0)

  override def reverseMap[B, That](f: A => B)(implicit bf: CanBuildFrom[This, B, That]): That =
    reverse map f

  override def updated[B >: A, That](index: Int, elem: B)(implicit bf: CanBuildFrom[This, B, That]): That = {
    require(0 <= index && index < length) // !!! can't call length like this.
    patch(index, List(elem), 1)(bf)
  }

  override def +:[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    newPrepended(elem).asInstanceOf[That]

  override def :+[B >: A, That](elem: B)(implicit bf: CanBuildFrom[This, B, That]): That =
    ++(Iterator.single(elem))(bf)

  override def union[B >: A, That](that: GenSeq[B])(implicit bf: CanBuildFrom[This, B, That]): That =
    newForced(thisSeq union that).asInstanceOf[That]

  override def diff[B >: A](that: GenSeq[B]): This =
    newForced(thisSeq diff that).asInstanceOf[This]

  override def intersect[B >: A](that: GenSeq[B]): This =
    newForced(thisSeq intersect that).asInstanceOf[This]

  override def sorted[B >: A](implicit ord: Ordering[B]): This =
    newForced(thisSeq sorted ord).asInstanceOf[This]

  override def stringPrefix = "SeqView"
}
