package scala.collection.generic

/** A template for companion objects of immutable.Map and subclasses thereof.
 */
abstract class ImmutableMapFactory[CC[A, +B] <: immutable.Map[A, B] with MapTemplate[A, B, CC[A, B]]] {

  def newBuilder[A, B]: Builder[(A, B), CC[A, B]] = new ImmutableMapBuilder[A, B, CC[A, B]](empty[A, B])

  def empty[A, B]: CC[A, B]

  def apply[A, B](elems: (A, B)*): CC[A, B] = {
    val b = newBuilder[A, B]
    b ++= Iterable.fromOld(elems)
    b.result
  }
}
