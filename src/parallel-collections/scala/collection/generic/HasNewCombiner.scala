package scala.collection.generic



import scala.collection.parallel.Combiner



trait HasNewCombiner[+T, +Repr] {
  protected[this] def newCombiner: Combiner[T, Repr]
}















