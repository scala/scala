package scala.collection.generic

/** A template for companion objects of mutable.Map and subclasses thereof.
 */
abstract class MutableMapFactory[CC[A, B] <: mutable.Map[A, B] with MutableMapTemplate[A, B, CC[A, B]]]
  extends MapFactory[CC] {

  def newBuilder[A, B] = new MutableMapBuilder[A, B, CC[A, B]](empty[A, B])
}
