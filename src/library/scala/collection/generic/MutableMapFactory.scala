package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class MutableMapFactory[CC[A, B] <: mutable.Map[A, B] with MutableMapTemplate[A, B, CC[A, B]]] {

  def newBuilder[A, B]: Builder[(A, B), CC[A, B]] = new MutableMapBuilder(empty[A, B])

  def empty[A, B]: CC[A, B]

  def apply[A, B](elems: (A, B)*): CC[A, B] = {
    val b = newBuilder[A, B]
    b ++= Iterable.fromOld(elems)
    b.result
  }
}
