package scala.collection.generic





import collection.mutable.Builder
import collection.parallel.Combiner
import collection.parallel.ParSet
import collection.parallel.ParSetLike






abstract class ParSetFactory[CC[X] <: ParSet[X] with ParSetLike[X, CC[X], _] with GenericParTemplate[X, CC]]
  extends SetFactory[CC]
     with GenericParCompanion[CC]
{
  def newBuilder[A]: Combiner[A, CC[A]] = newCombiner[A]

  def newCombiner[A]: Combiner[A, CC[A]]

  class GenericCanCombineFrom[A] extends CanCombineFrom[CC[_], A, CC[A]] {
    override def apply(from: Coll) = from.genericCombiner[A]
    override def apply() = newCombiner[A]
  }
}




