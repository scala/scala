package scala.collection.parallel



import scala.collection.SetLike
import scala.collection.Set
import scala.collection.mutable.Builder








trait ParSetLike[T,
                 +Repr <: ParSetLike[T, Repr, Sequential] with ParSet[T],
                 +Sequential <: Set[T] with SetLike[T, Sequential]]
extends SetLike[T, Repr]
   with ParIterableLike[T, Repr, Sequential]
{ self =>

  protected[this] override def newBuilder: Builder[T, Repr] = newCombiner

  protected[this] override def newCombiner: Combiner[T, Repr]

  override def empty: Repr

}




















































