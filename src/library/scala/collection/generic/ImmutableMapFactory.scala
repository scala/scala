package scala.collection.generic

/** A template for companion objects of immutable.Map and subclasses thereof.
 */
abstract class ImmutableMapFactory[CC[A, +B] <: immutable.Map[A, B] with ImmutableMapTemplate[A, B, CC[A, B]]]
  extends MapFactory[CC] {

  def newBuilder[A, B] = new ImmutableMapBuilder[A, B, CC[A, B]](empty[A, B])
}
