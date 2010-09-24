package scala.collection.parallel




import scala.collection.Parallel
import scala.collection.TraversableViewLike
import scala.collection.IterableView
import scala.collection.IterableViewLike
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.CanCombineFrom
import scala.collection.parallel.immutable.ParRange




/** A template view of a non-strict view of parallel iterable collection.
 *
 *  '''Note:''' Regular view traits have type parameters used to carry information
 *  about the type of the elements, type of the collection they are derived from and
 *  their own actual representation type. Parallel views have an additional parameter
 *  which carries information about the type of the sequential version of the view.
 *
 *  @tparam T         the type of the elements this view can traverse
 *  @tparam Coll      the type of the collection this view is derived from
 *  @tparam CollSeq   TODO
 *  @tparam This      the actual representation type of this view
 *  @tparam ThisSeq   the type of the sequential representation of this view
 *
 *  @since 2.8
 */
trait ParIterableViewLike[+T,
                          +Coll <: Parallel,
                          +CollSeq,
                          +This <: ParIterableView[T, Coll, CollSeq] with ParIterableViewLike[T, Coll, CollSeq, This, ThisSeq],
                          +ThisSeq <: IterableView[T, CollSeq] with IterableViewLike[T, CollSeq, ThisSeq]]
extends IterableView[T, Coll]
   with IterableViewLike[T, Coll, This]
   with ParIterable[T]
   with ParIterableLike[T, This, ThisSeq]
{
self =>

  override protected[this] def newCombiner: Combiner[T, This] = throw new UnsupportedOperationException(this + ".newCombiner");

  type SCPI = SignalContextPassingIterator[ParIterator]

  /* wrappers */

  trait Transformed[+S] extends ParIterableView[S, Coll, CollSeq] with super.Transformed[S] {
    override def parallelIterator: ParIterableIterator[S]
    override def iterator = parallelIterator
    environment = self.environment
  }

  trait Sliced extends super.Sliced with Transformed[T] {
    override def slice(from1: Int, until1: Int): This = newSliced(from1 max 0, until1 max 0).asInstanceOf[This]
    def parallelIterator: ParIterableIterator[T] = self.parallelIterator.slice(from, until)
    def seq = self.seq.slice(from, until)
  }

  trait Mapped[S] extends super.Mapped[S] with Transformed[S]{
    def parallelIterator: ParIterableIterator[S] = self.parallelIterator.map(mapping)
    def seq = self.seq.map(mapping).asInstanceOf[IterableView[S, CollSeq]]
  }

  // only use if other is a ParIterable, otherwise force
  trait Appended[U >: T] extends super.Appended[U] with Transformed[U] {
    def restPar: ParIterable[U] = rest.asParIterable
    def parallelIterator: ParIterableIterator[U] = self.parallelIterator.appendParIterable[U, ParIterableIterator[U]](restPar.parallelIterator)
    def seq = self.seq.++(rest).asInstanceOf[IterableView[U, CollSeq]]
  }

  trait Forced[S] extends super.Forced[S] with Transformed[S] {
    def forcedPar: ParIterable[S] = forced.asParIterable
    def parallelIterator: ParIterableIterator[S] = forcedPar.parallelIterator
    def seq = forcedPar.seq.view.asInstanceOf[IterableView[S, CollSeq]]
  }

  // only use if other is a ParSeq, otherwise force
  trait Zipped[S] extends super.Zipped[S] with Transformed[(T, S)] {
    def otherPar: ParSeq[S] = other.asParSeq
    def parallelIterator: ParIterableIterator[(T, S)] = self.parallelIterator zipParSeq otherPar.parallelIterator
    def seq = (self.seq zip other).asInstanceOf[IterableView[(T, S), CollSeq]]
  }

  // only use if other is a ParSeq, otherwise force
  trait ZippedAll[U >: T, S] extends super.ZippedAll[U, S] with Transformed[(U, S)] {
    def otherPar: ParSeq[S] = other.asParSeq
    def parallelIterator: ParIterableIterator[(U, S)] = self.parallelIterator.zipAllParSeq(otherPar.parallelIterator, thisElem, thatElem)
    def seq = (self.seq.zipAll(other, thisElem, thatElem)).asInstanceOf[IterableView[(U, S), CollSeq]]
  }

  protected[this] def thisParSeq: ParSeq[T] = mutable.ParArray.fromTraversables(this.iterator)

  /* operation overrides */

  override def take(n: Int): This = newSliced(0, n).asInstanceOf[This]
  override def drop(n: Int): This = newSliced(n, parallelIterator.remaining).asInstanceOf[This]
  override def splitAt(n: Int): (This, This) = (take(n), drop(n))
  override def slice(from: Int, until: Int): This = newSliced(from, until).asInstanceOf[This]
  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = newMapped(f).asInstanceOf[That]
  override def ++[U >: T, That](xs: TraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]): That = newAppendedTryParIterable(xs.toTraversable).asInstanceOf[That]

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
  override def flatMap[S, That](f: T => Traversable[S])(implicit bf: CanBuildFrom[This, S, That]): That = newForced(thisParSeq.flatMap(f)).asInstanceOf[That]

  override def zip[U >: T, S, That](that: Iterable[S])(implicit bf: CanBuildFrom[This, (U, S), That]): That = newZippedTryParSeq(that).asInstanceOf[That]
  override def zipWithIndex[U >: T, That](implicit bf: CanBuildFrom[This, (U, Int), That]): That =
    newZipped(new ParRange(0, parallelIterator.remaining, 1, false)).asInstanceOf[That]
  override def zipAll[S, U >: T, That](that: Iterable[S], thisElem: U, thatElem: S)(implicit bf: CanBuildFrom[This, (U, S), That]): That =
    newZippedAllTryParSeq(that, thisElem, thatElem).asInstanceOf[That]

  override def force[U >: T, That](implicit bf: CanBuildFrom[Coll, U, That]) = bf ifParallel { pbf =>
    executeAndWaitResult(new Force(pbf, parallelIterator) mapResult { _.result })
  } otherwise {
    val b = bf(underlying)
    b ++= this.iterator
    b.result
  }

  /* wrapper virtual ctors */

  protected override def newSliced(f: Int, u: Int): Transformed[T] = new Sliced { val from = f; val until = u }
  protected override def newMapped[S](f: T => S): Transformed[S] = new Mapped[S] { val mapping = f }
  protected override def newForced[S](xs: => Seq[S]): Transformed[S] = new Forced[S] { val forced = xs }
  protected override def newAppended[U >: T](that: Traversable[U]): Transformed[U] = new Appended[U] { val rest = that }
  protected override def newDroppedWhile(p: T => Boolean) = unsupported
  protected override def newTakenWhile(p: T => Boolean) = unsupported
  protected override def newFlatMapped[S](f: T => Traversable[S]) = unsupported
  protected override def newFiltered(p: T => Boolean) = unsupported
  protected override def newZipped[S](that: Iterable[S]): Transformed[(T, S)] = new Zipped[S] { val other = that }
  protected override def newZippedAll[U >: T, S](that: Iterable[S], _thisElem: U, _thatElem: S): Transformed[(U, S)] = new ZippedAll[U, S] {
    val other = that
    val thisElem = _thisElem
    val thatElem = _thatElem
  }

  /* argument sequence dependent ctors */

  protected def newForcedTryParIterable[S](xs: => Seq[S]): Transformed[S] = {
    if (xs.isParIterable) newForced[S](xs)
    else newForced(mutable.ParArray.fromTraversables(xs))
  }
  protected def newAppendedTryParIterable[U >: T](that: Traversable[U]): Transformed[U] = {
    // we only append if `that` is a parallel iterable, i.e. it has a splitter
    if (that.isParIterable) newAppended(that)
    else newAppended(mutable.ParArray.fromTraversables(that))
  }
  protected def newZippedTryParSeq[S](that: Iterable[S]): Transformed[(T, S)] = {
    if (that.isParSeq) newZipped[S](that)
    else newZipped[S](mutable.ParArray.fromTraversables(that))
  }
  protected def newZippedAllTryParSeq[S, U >: T](that: Iterable[S], thisElem: U, thatElem: S): Transformed[(U, S)] = {
    if (that.isParSeq) newZippedAll(that, thisElem, thatElem)
    else newZippedAll(mutable.ParArray.fromTraversables(that), thisElem, thatElem)
  }

  /* tasks */

  protected[this] class Force[U >: T, That](cbf: CanCombineFrom[Coll, U, That], protected[this] val pit: ParIterableIterator[T])
  extends Transformer[Combiner[U, That], Force[U, That]] {
    var result: Combiner[U, That] = null
    def leaf(prev: Option[Combiner[U, That]]) = result = pit.copy2builder[U, That, Combiner[U, That]](reuse(prev, cbf(self.underlying)))
    protected[this] def newSubtask(p: ParIterableIterator[T]) = new Force(cbf, p)
    override def merge(that: Force[U, That]) = result = result combine that.result
  }

}










