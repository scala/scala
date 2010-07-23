package scala.collection.parallel




import scala.collection.Parallel
import scala.collection.TraversableViewLike
import scala.collection.IterableView
import scala.collection.IterableViewLike





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

  type CPI = SignalContextPassingIterator[ParIterator]

  trait Transformed[+S] extends ParIterableView[S, Coll, CollSeq] with super.Transformed[S]

}










