package scala.collection.generic

/** A template for companion objects of immutable.Map and subclasses thereof.
 */
abstract class ImmutableSortedMapFactory[CC[A, B] <: immutable.SortedMap[A, B] with SortedMapTemplate[A, B, CC[A, B]]]
  extends SortedMapFactory[CC] {

  def newBuilder[A, B](implicit ord: Ordering[A]): Builder[(A, B), CC[A, B]] =
    new MapBuilder[A, B, CC[A, B]](empty(ord))
}
