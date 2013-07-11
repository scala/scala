/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection.parallel

import scala.collection.{ Parallel, SeqView, SeqViewLike, GenSeqView, GenSeqViewLike, GenSeq }
import scala.collection.{ GenIterable, GenTraversable, GenTraversableOnce, Iterator }
import scala.collection.generic.{ CanBuildFrom, SliceInterval }
import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.immutable.ParRange


/** A template view of a non-strict view of parallel sequence.
 *
 *  @tparam T             the type of the elements in this view
 *  @tparam Coll          type of the collection this view is derived from
 *  @tparam CollSeq       type of the sequential collection corresponding to the underlying parallel collection
 *  @tparam This          actual representation type of this view
 *  @tparam ThisSeq       type of the sequential version of this view
 *
 *  @since 2.9
 */
trait ParSeqViewLike[+T,
                     +Coll <: Parallel,
                     +CollSeq,
                     +This <: ParSeqView[T, Coll, CollSeq] with ParSeqViewLike[T, Coll, CollSeq, This, ThisSeq],
                     +ThisSeq <: SeqView[T, CollSeq] with SeqViewLike[T, CollSeq, ThisSeq]]
extends GenSeqView[T, Coll]
   with GenSeqViewLike[T, Coll, This]
   with ParIterableView[T, Coll, CollSeq]
   with ParIterableViewLike[T, Coll, CollSeq, This, ThisSeq]
   with ParSeq[T]
   with ParSeqLike[T, This, ThisSeq]
{
self =>

  trait Transformed[+S] extends ParSeqView[S, Coll, CollSeq]
  with super[ParIterableView].Transformed[S] with super[GenSeqViewLike].Transformed[S] {
    override def splitter: SeqSplitter[S]
    override def iterator = splitter
    override def size = length
  }

  trait Sliced extends super[GenSeqViewLike].Sliced with super[ParIterableViewLike].Sliced with Transformed[T] {
    // override def slice(from1: Int, until1: Int): This = newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
    override def splitter = self.splitter.psplit(from, until - from)(1)
    override def seq = self.seq.slice(from, until)
  }

  trait Mapped[S] extends super[GenSeqViewLike].Mapped[S] with super[ParIterableViewLike].Mapped[S] with Transformed[S] {
    override def splitter = self.splitter.map(mapping)
    override def seq = self.seq.map(mapping).asInstanceOf[SeqView[S, CollSeq]]
  }

  trait Appended[U >: T] extends super[GenSeqViewLike].Appended[U] with super[ParIterableViewLike].Appended[U] with Transformed[U] {
    override def restPar: ParSeq[U] = rest.asParSeq
    override def splitter = self.splitter.appendParSeq[U, SeqSplitter[U]](restPar.splitter)
    override def seq = self.seq.++(rest).asInstanceOf[SeqView[U, CollSeq]]
  }

  trait Forced[S] extends super[GenSeqViewLike].Forced[S] with super[ParIterableViewLike].Forced[S] with Transformed[S] {
    override def forcedPar: ParSeq[S] = forced.asParSeq
    override def splitter: SeqSplitter[S] = forcedPar.splitter
    override def seq = forcedPar.seq.view.asInstanceOf[SeqView[S, CollSeq]]
  }

  trait Zipped[S] extends super[GenSeqViewLike].Zipped[S] with super[ParIterableViewLike].Zipped[S] with Transformed[(T, S)] {
    override def splitter = self.splitter zipParSeq otherPar.splitter
    override def seq = (self.seq zip other).asInstanceOf[SeqView[(T, S), CollSeq]]
  }

  trait ZippedAll[U >: T, S] extends super[GenSeqViewLike].ZippedAll[U, S] with super[ParIterableViewLike].ZippedAll[U, S] with Transformed[(U, S)] {
    override def splitter: SeqSplitter[(U, S)] = self.splitter.zipAllParSeq(otherPar.splitter, thisElem, thatElem)
    override def seq = (self.seq.zipAll(other, thisElem, thatElem)).asInstanceOf[SeqView[(U, S), CollSeq]]
  }

  trait Reversed extends super.Reversed with Transformed[T] {
    override def splitter: SeqSplitter[T] = self.splitter.reverse
    override def seq = self.seq.reverse.asInstanceOf[SeqView[T, CollSeq]]
  }

  // use only with ParSeq patches, otherwise force
  trait Patched[U >: T] extends super.Patched[U] with Transformed[U] {
    def patchPar: ParSeq[U] = patch.asInstanceOf[ParSeq[U]]
    override def splitter: SeqSplitter[U] = self.splitter.patchParSeq[U](from, patchPar.splitter, replaced)
    override def seq = self.seq.patch(from, patch, replaced).asInstanceOf[SeqView[U, CollSeq]]
  }

  // !!!
  //
  // What is up with this trait and method, why are they here doing
  // nothing but throwing exceptions, without even being deprecated?
  // They're not implementing something abstract; why aren't they
  // just removed?
  //
  // use Patched instead
  trait Prepended[U >: T] extends super.Prepended[U] with Transformed[U] {
    unsupported
  }
  protected def newPrepended[U >: T](elem: U): Transformed[U] = unsupported

  /* wrapper virtual ctors */

  protected override def newSliced(_endpoints: SliceInterval): Transformed[T] = new { val endpoints = _endpoints } with Sliced
  protected override def newAppended[U >: T](that: GenTraversable[U]): Transformed[U] = {
    // we only append if `that` is a parallel sequence, i.e. it has a precise splitter
    if (that.isParSeq) new Appended[U] { val rest = that }
    else newForced(mutable.ParArray.fromTraversables(this, that))
  }
  protected override def newForced[S](xs: => GenSeq[S]): Transformed[S] = {
    if (xs.isParSeq) new Forced[S] { val forced = xs }
    else new Forced[S] { val forced = mutable.ParArray.fromTraversables(xs) }
  }
  protected override def newMapped[S](f: T => S): Transformed[S] = new Mapped[S] { val mapping = f }
  protected override def newZipped[S](that: GenIterable[S]): Transformed[(T, S)] = new Zipped[S] { val other = that }
  protected override def newZippedAll[U >: T, S](that: GenIterable[S], _thisElem: U, _thatElem: S): Transformed[(U, S)] = new ZippedAll[U, S] {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }
  protected def newReversed: Transformed[T] = new Reversed { }
  protected def newPatched[U >: T](_from: Int, _patch: GenSeq[U], _replaced: Int): Transformed[U] = new {
    val from = _from
    val patch = _patch
    val replaced = _replaced
  } with Patched[U]

  /* operation overrides */

  /* sliced */
  override def slice(from: Int, until: Int): This = newSliced(SliceInterval(from, until)).asInstanceOf[This]
  override def take(n: Int): This = newSliced(SliceInterval(0, n)).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(SliceInterval(n, length)).asInstanceOf[This]
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))

  /* appended */
  override def ++[U >: T, That](xs: GenTraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]): That = newAppended(xs.toTraversable).asInstanceOf[That]
  override def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = ++(Iterator.single(elem))(bf)
  //override def union[U >: T, That](that: GenSeq[U])(implicit bf: CanBuildFrom[This, U, That]): That = this ++ that

  /* misc */
  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = newMapped(f).asInstanceOf[That]
  override def zip[U >: T, S, That](that: GenIterable[S])(implicit bf: CanBuildFrom[This, (U, S), That]): That = newZippedTryParSeq(that).asInstanceOf[That]
  override def zipWithIndex[U >: T, That](implicit bf: CanBuildFrom[This, (U, Int), That]): That =
    newZipped(ParRange(0, splitter.remaining, 1, inclusive = false)).asInstanceOf[That]
  override def reverse: This = newReversed.asInstanceOf[This]
  override def reverseMap[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = reverse.map(f)

  /* patched */
  override def updated[U >: T, That](index: Int, elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = {
    require(0 <= index && index < length)
    patch(index, List(elem), 1)(bf)
  }
  override def padTo[U >: T, That](len: Int, elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = patch(length, Seq.fill(len - length)(elem), 0)
  override def +:[U >: T, That](elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = patch(0, mutable.ParArray.fromTraversables(Iterator.single(elem)), 0)
  override def patch[U >: T, That](from: Int, patch: GenSeq[U], replace: Int)(implicit bf: CanBuildFrom[This, U, That]): That = newPatched(from, patch, replace).asInstanceOf[That]

  /* forced */
  // override def diff[U >: T](that: GenSeq[U]): This = newForced(thisParSeq diff that).asInstanceOf[This]
  // override def intersect[U >: T](that: GenSeq[U]): This = newForced(thisParSeq intersect that).asInstanceOf[This]
  // override def sorted[U >: T](implicit ord: Ordering[U]): This = newForced(thisParSeq sorted ord).asInstanceOf[This]
  override def collect[S, That](pf: PartialFunction[T, S])(implicit bf: CanBuildFrom[This, S, That]): That = filter(pf.isDefinedAt).map(pf)(bf)
  override def scanLeft[S, That](z: S)(op: (S, T) => S)(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisParSeq.scanLeft(z)(op)).asInstanceOf[That]
  override def scanRight[S, That](z: S)(op: (T, S) => S)(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisParSeq.scanRight(z)(op)).asInstanceOf[That]
  override def groupBy[K](f: T => K): immutable.ParMap[K, This] = thisParSeq.groupBy(f).map(kv => (kv._1, newForced(kv._2).asInstanceOf[This]))
  override def force[U >: T, That](implicit bf: CanBuildFrom[Coll, U, That]) = bf ifParallel { pbf =>
    tasksupport.executeAndWaitResult(new Force(pbf, splitter).mapResult(_.result).asInstanceOf[Task[That, _]])
  } otherwise {
    val b = bf(underlying)
    b ++= this.iterator
    b.result()
  }

  /* tasks */

  protected[this] class Force[U >: T, That](cbf: CanCombineFrom[Coll, U, That], protected[this] val pit: SeqSplitter[T])
  extends Transformer[Combiner[U, That], Force[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.copy2builder[U, That, Combiner[U, That]](reuse(prev, cbf(self.underlying)))
    protected[this] def newSubtask(p: SuperParIterator) = new Force(cbf, down(p))
    override def merge(that: Force[U, That]) = result = result combine that.result
  }
}
