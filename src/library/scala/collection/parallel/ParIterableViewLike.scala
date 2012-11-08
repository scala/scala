/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.parallel

import scala.collection.Parallel
import scala.collection.{ IterableView, IterableViewLike }
import scala.collection.{ GenIterableView, GenIterableViewLike }
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.GenIterable
import scala.collection.GenSeq
import scala.collection.generic.{ CanBuildFrom, SliceInterval }
import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.immutable.ParRange
import scala.language.implicitConversions



/** A template view of a non-strict view of parallel iterable collection.
 *
 *  '''Note:''' Regular view traits have type parameters used to carry information
 *  about the type of the elements, type of the collection they are derived from and
 *  their own actual representation type. Parallel views have an additional parameter
 *  which carries information about the type of the sequential version of the view.
 *
 *  @tparam T         the type of the elements this view can traverse
 *  @tparam Coll      the type of the parallel collection this view is derived from
 *  @tparam CollSeq   the type of the sequential collection corresponding to the underlying parallel collection
 *  @tparam This      the actual representation type of this view
 *  @tparam ThisSeq   the type of the sequential representation of this view
 *
 *  @since 2.9
 */
trait ParIterableViewLike[+T,
                          +Coll <: Parallel,
                          +CollSeq,
                          +This <: ParIterableView[T, Coll, CollSeq] with ParIterableViewLike[T, Coll, CollSeq, This, ThisSeq],
                          +ThisSeq <: IterableView[T, CollSeq] with IterableViewLike[T, CollSeq, ThisSeq]]
extends GenIterableView[T, Coll]
   with GenIterableViewLike[T, Coll, This]
   with ParIterable[T]
   with ParIterableLike[T, This, ThisSeq]
{
self =>

  override def foreach[U](f: T => U): Unit = super[ParIterableLike].foreach(f)
  override protected[this] def newCombiner: Combiner[T, This] = throw new UnsupportedOperationException(this + ".newCombiner");
  protected[this] def viewIdentifier: String
  protected[this] def viewIdString: String

  protected def underlying: Coll

  /* wrappers */

  trait Transformed[+S] extends ParIterableView[S, Coll, CollSeq] with super.Transformed[S] {
    override def splitter: IterableSplitter[S]
    override def iterator = splitter
    def size = splitter.remaining
  }

  trait Sliced extends super.Sliced with Transformed[T] {
    // override def slice(from1: Int, until1: Int): This = newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
    def splitter: IterableSplitter[T] = self.splitter.slice(from, until)
    override def seq = self.seq.slice(from, until)
  }

  trait Mapped[S] extends super.Mapped[S] with Transformed[S]{
    def splitter: IterableSplitter[S] = self.splitter.map(mapping)
    override def seq = self.seq.map(mapping).asInstanceOf[IterableView[S, CollSeq]]
  }

  // only use if other is a ParIterable, otherwise force
  trait Appended[U >: T] extends super.Appended[U] with Transformed[U] {
    def restPar: ParIterable[U] = rest.asParIterable
    def splitter: IterableSplitter[U] = self.splitter.appendParIterable[U, IterableSplitter[U]](restPar.splitter)
    override def seq = self.seq.++(rest).asInstanceOf[IterableView[U, CollSeq]]
  }

  trait Forced[S] extends super.Forced[S] with Transformed[S] {
    def forcedPar: ParIterable[S] = forced.asParIterable
    def splitter: IterableSplitter[S] = forcedPar.splitter
    override def seq = forcedPar.seq.view.asInstanceOf[IterableView[S, CollSeq]]
  }

  // only use if other is a ParSeq, otherwise force
  trait Zipped[S] extends super.Zipped[S] with Transformed[(T, S)] {
    def otherPar: ParSeq[S] = other.asParSeq
    def splitter: IterableSplitter[(T, S)] = self.splitter zipParSeq otherPar.splitter
    override def seq = (self.seq zip other).asInstanceOf[IterableView[(T, S), CollSeq]]
  }

  // only use if other is a ParSeq, otherwise force
  trait ZippedAll[U >: T, S] extends super.ZippedAll[U, S] with Transformed[(U, S)] {
    def otherPar: ParSeq[S] = other.asParSeq
    def splitter: IterableSplitter[(U, S)] = self.splitter.zipAllParSeq(otherPar.splitter, thisElem, thatElem)
    override def seq = (self.seq.zipAll(other, thisElem, thatElem)).asInstanceOf[IterableView[(U, S), CollSeq]]
  }

  protected[this] def thisParSeq: ParSeq[T] = mutable.ParArray.fromTraversables(this.iterator)
  private[this] implicit def asThis(xs: Transformed[T]): This = xs.asInstanceOf[This]

  /* operation overrides */

  override def take(n: Int): This = newSliced(SliceInterval(0, n))
  override def drop(n: Int): This = newSliced(SliceInterval(n, splitter.remaining))
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))
  override def slice(from: Int, until: Int): This = newSliced(SliceInterval(from, until))
  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = newMapped(f).asInstanceOf[That]
  override def ++[U >: T, That](xs: GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]): That = newAppendedTryParIterable(xs.toTraversable).asInstanceOf[That]

  override def filter(p: T => Boolean): This = newForced(thisParSeq.filter(p)).asInstanceOf[This]
  override def filterNot(p: T => Boolean): This = newForced(thisParSeq.filterNot(p)).asInstanceOf[This]
  override def partition(p: T => Boolean): (This, This) = {
    val (t, f) = thisParSeq.partition(p)
    (newForced(t).asInstanceOf[This], newForced(f).asInstanceOf[This])
  }
  override def takeWhile(p: T => Boolean): This = newForced(thisParSeq.takeWhile(p)).asInstanceOf[This]
  override def dropWhile(p: T => Boolean): This = newForced(thisParSeq.dropWhile(p)).asInstanceOf[This]
  override def span(p: T => Boolean): (This, This) = {
    val (pref, suff) = thisParSeq.span(p)
    (newForced(pref).asInstanceOf[This], newForced(suff).asInstanceOf[This])
  }
  override def flatMap[S, That](f: T => GenTraversableOnce[S])(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisParSeq.flatMap(f)).asInstanceOf[That]

  override def zip[U >: T, S, That](that: GenIterable[S])(implicit bf: CanBuildFrom[This, (U, S), That]): That = newZippedTryParSeq(that).asInstanceOf[That]
  override def zipWithIndex[U >: T, That](implicit bf: CanBuildFrom[This, (U, Int), That]): That =
    newZipped(ParRange(0, splitter.remaining, 1, false)).asInstanceOf[That]
  override def zipAll[S, U >: T, That](that: GenIterable[S], thisElem: U, thatElem: S)(implicit bf: CanBuildFrom[This, (U, S), That]): That =
    newZippedAllTryParSeq(that, thisElem, thatElem).asInstanceOf[That]

  override def force[U >: T, That](implicit bf: CanBuildFrom[Coll, U, That]) = bf ifParallel { pbf =>
    tasksupport.executeAndWaitResult(new Force(pbf, splitter).mapResult(_.result).asInstanceOf[Task[That, ResultMapping[_, Force[U, That], That]]])
  } otherwise {
    val b = bf(underlying)
    b ++= this.iterator
    b.result
  }

  /* wrapper virtual ctors */

  protected def newSliced(_endpoints: SliceInterval): Transformed[T] = new { val endpoints = _endpoints } with Sliced
  protected def newMapped[S](f: T => S): Transformed[S] = new Mapped[S] { val mapping = f }
  protected def newForced[S](xs: => GenSeq[S]): Transformed[S] = new Forced[S] { val forced = xs }
  protected def newAppended[U >: T](that: GenTraversable[U]): Transformed[U] = new Appended[U] { val rest = that }
  protected def newDroppedWhile(p: T => Boolean) = unsupported
  protected def newTakenWhile(p: T => Boolean) = unsupported
  protected def newFlatMapped[S](f: T => GenTraversableOnce[S]) = unsupported
  protected def newFiltered(p: T => Boolean) = unsupported
  protected def newZipped[S](that: GenIterable[S]): Transformed[(T, S)] = new Zipped[S] { val other = that }
  protected def newZippedAll[U >: T, S](that: GenIterable[S], _thisElem: U, _thatElem: S): Transformed[(U, S)] = new ZippedAll[U, S] {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }

  /* argument sequence dependent ctors */

  protected def newForcedTryParIterable[S](xs: => GenSeq[S]): Transformed[S] = {
    if (xs.isParIterable) newForced[S](xs)
    else newForced(mutable.ParArray.fromTraversables(xs))
  }
  protected def newAppendedTryParIterable[U >: T](that: GenTraversable[U]): Transformed[U] = {
    // we only append if `that` is a parallel iterable, i.e. it has a splitter
    if (that.isParIterable) newAppended(that)
    else newAppended(mutable.ParArray.fromTraversables(that))
  }
  protected def newZippedTryParSeq[S](that: GenIterable[S]): Transformed[(T, S)] = {
    if (that.isParSeq) newZipped[S](that)
    else newZipped[S](mutable.ParArray.fromTraversables(that))
  }
  protected def newZippedAllTryParSeq[S, U >: T](that: GenIterable[S], thisElem: U, thatElem: S): Transformed[(U, S)] = {
    if (that.isParSeq) newZippedAll(that, thisElem, thatElem)
    else newZippedAll(mutable.ParArray.fromTraversables(that), thisElem, thatElem)
  }

  /* tasks */

  protected[this] class Force[U >: T, That](cbf: CanCombineFrom[Coll, U, That], protected[this] val pit: IterableSplitter[T])
  extends Transformer[Combiner[U, That], Force[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.copy2builder[U, That, Combiner[U, That]](reuse(prev, cbf(self.underlying)))
    protected[this] def newSubtask(p: IterableSplitter[T]) = new Force(cbf, p)
    override def merge(that: Force[U, That]) = result = result combine that.result
  }

}











