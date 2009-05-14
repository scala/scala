package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class MapFactory[CC[A, B] <: Map[A, B] with MapTemplate[A, B, CC[A, B]]] {

  type Coll = CC[_, _]

  def newBuilder[A, B]: Builder[(A, B), CC[A, B]]

  def empty[A, B]: CC[A, B]

  def apply[A, B](elems: (A, B)*): CC[A, B] = (newBuilder[A, B] ++= elems).result

  class MapBuilderFactory[A, B] extends BuilderFactory[(A, B), CC[A, B], Coll] {
    def apply(from: Coll) = newBuilder[A, B]
  }
}
