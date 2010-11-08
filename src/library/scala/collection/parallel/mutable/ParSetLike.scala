package scala.collection
package parallel.mutable



import scala.collection.mutable.Set
import scala.collection.mutable.Builder








trait ParSetLike[T,
                 +Repr <: ParSetLike[T, Repr, Sequential] with ParSet[T],
                 +Sequential <: mutable.Set[T] with mutable.SetLike[T, Sequential]]
extends mutable.SetLike[T, Repr]
   with collection.parallel.ParIterableLike[T, Repr, Sequential]
   with collection.parallel.ParSetLike[T, Repr, Sequential]
{ self =>

  protected[this] override def newBuilder: Builder[T, Repr] = newCombiner

  protected[this] override def newCombiner: parallel.Combiner[T, Repr]

  override def empty: Repr

}








































