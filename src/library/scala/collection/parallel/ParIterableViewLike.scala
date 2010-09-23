package scala.collection.parallel




import scala.collection.Parallel
import scala.collection.TraversableViewLike
import scala.collection.IterableView
import scala.collection.IterableViewLike
import scala.collection.generic.CanBuildFrom




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
    def restAsParIterable: ParIterable[U] = rest.asParIterable
    def parallelIterator: ParIterableIterator[U] = self.parallelIterator.appendIterable[U, ParIterableIterator[U]](restAsParIterable.parallelIterator)
    def seq = self.seq.++(rest).asInstanceOf[IterableView[U, CollSeq]]
  }

  trait Forced[S] extends super.Forced[S] with Transformed[S] {
    def forcedPar: ParIterable[S] = forced.asParIterable
    def parallelIterator: ParIterableIterator[S] = forcedPar.parallelIterator
    // cheating here - knowing that `underlying` of `self.seq` is of type `CollSeq`,
    // we use it to obtain a view of the correct type - not the most efficient thing
    // in the universe, but without making `newForced` more accessible, or adding
    // a `forced` method to `SeqView`, this is the best we can do
    def seq = self.seq.take(0).++(forced).asInstanceOf[IterableView[S, CollSeq]]
  }

  /* operation overrides */

  override def slice(from: Int, until: Int): This = newSliced(from, until).asInstanceOf[This]
  override def map[S, That](f: T => S)(implicit bf: CanBuildFrom[This, S, That]): That = newMapped(f).asInstanceOf[That]
  override def ++[U >: T, That](xs: TraversableOnce[U])(implicit bf: CanBuildFrom[This, U, That]): That = newAppended(xs.toTraversable).asInstanceOf[That]

  /* wrapper virtual ctors */

  protected override def newSliced(f: Int, u: Int): Transformed[T] = new Sliced { val from = f; val until = u }
  protected override def newMapped[S](f: T => S): Transformed[S] = new Mapped[S] { val mapping = f }
  protected override def newForced[S](xs: => Seq[S]): Transformed[S] = new Forced[S] { val forced = xs }
  protected override def newAppended[U >: T](that: Traversable[U]): Transformed[U] = {
    // we only append if `that` is a parallel iterable, i.e. it has a splitter
    if (that.isParIterable) new Appended[U] { val rest = that }
    else newForced(mutable.ParArray.fromTraversables(this, that))
  }

}










