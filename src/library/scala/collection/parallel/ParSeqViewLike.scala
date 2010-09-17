package scala.collection.parallel





import scala.collection.SeqView
import scala.collection.SeqViewLike
import scala.collection.Parallel
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.CanCombineFrom







/** A template view of a non-strict view of parallel sequence.
 *
 *  @tparam T             the type of the elements in this view
 *  @tparam Coll          type of the collection this view is derived from
 *  @tparam CollSeq       TODO
 *  @tparam This          actual representation type of this view
 *  @tparam ThisSeq       type of the sequential version of this view
 *
 *  @since 2.8
 */
trait ParSeqViewLike[+T,
                     +Coll <: Parallel,
                     +CollSeq,
                     +This <: ParSeqView[T, Coll, CollSeq] with ParSeqViewLike[T, Coll, CollSeq, This, ThisSeq],
                     +ThisSeq <: SeqView[T, CollSeq] with SeqViewLike[T, CollSeq, ThisSeq]]
extends SeqView[T, Coll]
   with SeqViewLike[T, Coll, This]
   with ParIterableView[T, Coll, CollSeq]
   with ParIterableViewLike[T, Coll, CollSeq, This, ThisSeq]
   with ParSeq[T]
   with ParSeqLike[T, This, ThisSeq]
{
  self =>

  type SCPI = SignalContextPassingIterator[ParIterator]

  trait Transformed[+S] extends ParSeqView[S, Coll, CollSeq]
  with super[ParIterableView].Transformed[S] with super[SeqView].Transformed[S] {
    override def parallelIterator = new Elements(0, length) with SCPI {}
    override def iterator = parallelIterator
    environment = self.environment
  }

  trait Forced[S] extends super.Forced[S] with Transformed[S] {
    // cheating here - knowing that `underlying` of `self.seq` is of type `CollSeq`,
    // we use it to obtain a view of the correct type - not the most efficient thing
    // in the universe, but without making `newForced` more accessible, or adding
    // a `forced` method to `SeqView`, this is the best we can do
    def seq = self.seq.take(0).++(forced).asInstanceOf[SeqView[S, CollSeq]]
  }

  trait Filtered extends super.Filtered with Transformed[T] {
    def seq = self.seq filter pred
  }

  trait Sliced extends super.Sliced with Transformed[T] {
    override def slice(from1: Int, until1: Int): This = newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
    def seq = self.seq.slice(from, until)
  }

  trait Appended[U >: T] extends super.Appended[U] with Transformed[U] {
    def seq = self.seq.++(rest).asInstanceOf[SeqView[U, CollSeq]]
  }

  trait Mapped[S] extends super.Mapped[S] with Transformed[S]{
    def seq = self.seq.map(mapping).asInstanceOf[SeqView[S, CollSeq]]
  }

  trait FlatMapped[S] extends super.FlatMapped[S] with Transformed[S] {
    def seq = self.seq.flatMap(mapping).asInstanceOf[SeqView[S, CollSeq]]
  }

  trait TakenWhile extends super.TakenWhile with Transformed[T] {
    def seq = self.seq takeWhile pred
  }

  trait DroppedWhile extends super.DroppedWhile with Transformed[T] {
    def seq = self.seq dropWhile pred
  }

  trait Zipped[S] extends super.Zipped[S] with Transformed[(T, S)] {
    def seq = (self.seq zip other).asInstanceOf[SeqView[(T, S), CollSeq]]
  }

  trait ZippedAll[T1 >: T, S] extends super.ZippedAll[T1, S] with Transformed[(T1, S)] {
    def seq = self.seq.zipAll(other, thisElem, thatElem).asInstanceOf[SeqView[(T1, S), CollSeq]]
  }

  trait Reversed extends super.Reversed with Transformed[T] {
    def seq = self.seq.reverse
  }

  trait Patched[U >: T] extends super.Patched[U] with Transformed[U] {
    def seq = self.seq.patch(from, patch, replaced).asInstanceOf[SeqView[U, CollSeq]]
  }

  trait Prepended[U >: T] extends super.Prepended[U] with Transformed[U] {
    def seq = (fst +: self.seq).asInstanceOf[SeqView[U, CollSeq]]
  }

  protected override def newFiltered(p: T => Boolean): Transformed[T] = new Filtered { val pred = p }
  protected override def newSliced(f: Int, u: Int): Transformed[T] = new Sliced { val from = f; val until = u }
  protected override def newAppended[U >: T](that: Traversable[U]): Transformed[U] = new Appended[U] { val rest = that }
  protected override def newMapped[S](f: T => S): Transformed[S] = new Mapped[S] { val mapping = f }
  protected override def newFlatMapped[S](f: T => Traversable[S]): Transformed[S] = new FlatMapped[S] { val mapping = f }
  protected override def newDroppedWhile(p: T => Boolean): Transformed[T] = new DroppedWhile { val pred = p }
  protected override def newTakenWhile(p: T => Boolean): Transformed[T] = new TakenWhile { val pred = p }
  protected override def newZipped[S](that: Iterable[S]): Transformed[(T, S)] = new Zipped[S] { val other = that }
  protected override def newZippedAll[T1 >: T, S](that: Iterable[S], _thisElem: T1, _thatElem: S): Transformed[(T1, S)] = new ZippedAll[T1, S] { val other = that; val thisElem = _thisElem; val thatElem = _thatElem }
  protected override def newReversed: Transformed[T] = new Reversed { }
  protected override def newPatched[U >: T](_from: Int, _patch: Seq[U], _replaced: Int): Transformed[U] = new Patched[U] { val from = _from; val patch = _patch; val replaced = _replaced }
  protected override def newPrepended[U >: T](elem: U): Transformed[U] = new Prepended[U] { protected[this] val fst = elem }

  override def filter(p: T => Boolean): This = newFiltered(p).asInstanceOf[This]
  override def filterNot(p: T => Boolean): This = newFiltered(!p(_)).asInstanceOf[This]
  override def partition(p: T => Boolean): (This, This) = (filter(p), filterNot(p))
  override def slice(from: Int, until: Int): This = newSliced(from, until).asInstanceOf[This]
  override def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(n, length).asInstanceOf[This]
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))
  override def ++[U >: T, That](xs: TraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]): That = newAppended(xs.toTraversable).asInstanceOf[That]
  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = newMapped(f).asInstanceOf[That]
  override def flatMap[S, That](f: T => Traversable[S])(implicit bf: CanBuildFrom[This, S, That]): That = newFlatMapped(f).asInstanceOf[That]
  override def collect[S, That](pf: PartialFunction[T, S])(implicit bf: CanBuildFrom[This, S, That]): That = filter(pf.isDefinedAt).map(pf)(bf)
  override def takeWhile(p: T => Boolean): This = newTakenWhile(p).asInstanceOf[This]
  override def dropWhile(p: T => Boolean): This = newDroppedWhile(p).asInstanceOf[This]
  override def span(p: T => Boolean): (This, This) = (takeWhile(p), dropWhile(p))
  override def scanLeft[S, That](z: S)(op: (S, T) => S)(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisSeq.scanLeft(z)(op)).asInstanceOf[That]
  override def scanRight[S, That](z: S)(op: (T, S) => S)(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisSeq.scanRight(z)(op)).asInstanceOf[That]
  override def groupBy[K](f: T => K): collection.immutable.Map[K, This] = thisSeq.groupBy(f).mapValues(xs => newForced(xs).asInstanceOf[This])
  override def +:[U >: T, That](elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = newPrepended(elem).asInstanceOf[That]
  override def reverse: This = newReversed.asInstanceOf[This]
  override def patch[U >: T, That](from: Int, patch: Seq[U], replaced: Int)(implicit bf: CanBuildFrom[This, U, That]): That = newPatched(from, patch, replaced).asInstanceOf[That]
  override def padTo[U >: T, That](len: Int, elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = patch(length, Seq.fill(len - length)(elem), 0)
  override def reverseMap[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = reverse.map(f)
  override def updated[U >: T, That](index: Int, elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = {
    require(0 <= index && index < length)
    patch(index, List(elem), 1)(bf)
  }
  override def :+[U >: T, That](elem: U)(implicit bf: CanBuildFrom[This, U, That]): That = ++(Iterator.single(elem))(bf)
  override def union[U >: T, That](that: Seq[U])(implicit bf: CanBuildFrom[This, U, That]): That = this ++ that
  override def diff[U >: T](that: Seq[U]): This = newForced(thisSeq diff that).asInstanceOf[This]
  override def intersect[U >: T](that: Seq[U]): This = newForced(thisSeq intersect that).asInstanceOf[This]
  override def sorted[U >: T](implicit ord: Ordering[U]): This = newForced(thisSeq sorted ord).asInstanceOf[This]

  override def force[U >: T, That](implicit bf: CanBuildFrom[Coll, U, That]) = bf ifParallel { pbf =>
    executeAndWaitResult(new Force(pbf, parallelIterator) mapResult { _.result })
  } otherwise {
    val b = bf(underlying)
    b ++= this.iterator
    b.result
  }

  /* tasks */

  protected[this] class Force[U >: T, That](cbf: CanCombineFrom[Coll, U, That], protected[this] val pit: ParSeqIterator[T])
  extends Transformer[Combiner[U, That], Force[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.copy2builder[U, That, Combiner[U, That]](reuse(prev, cbf(self.underlying)))
    protected[this] def newSubtask(p: SuperParIterator) = new Force(cbf, down(p))
    override def merge(that: Force[U, That]) = result = result combine that.result
  }

}




















