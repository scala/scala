package scala.collection.generic

/** A template for companion objects of Map and subclasses thereof.
 */
abstract class SetFactory[CC[A] <: Set[A] with SetTemplate[A, CC[A]]] {

  def newBuilder[A]: Builder[A, CC[A], Any] = new AddingBuilder[A, CC[A]](empty[A])

  def empty[A]: CC[A]

  def apply[A](elems: A*): CC[A] = {
    var s = empty[A]
    for (elem <- elems) s = s + elem
    s
  }
}
