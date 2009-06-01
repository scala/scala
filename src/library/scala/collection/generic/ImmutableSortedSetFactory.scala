package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class ImmutableSortedSetFactory[CC[A] <: immutable.SortedSet[A] with SortedSetTemplate[A, CC[A]]] extends SortedSetFactory[CC]{
  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]] = new SetBuilder[A, CC[A]](empty)
}
