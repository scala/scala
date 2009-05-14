package scala.collection.generic

/** A template for companion objects of Map and subclasses thereof.
 */
abstract class SetFactory[CC[X] <: Set[X] with SetTemplate[X, CC[X]]]
  extends Companion[CC] {

  def newBuilder[A]: Builder[A, CC[A]] = new AddingBuilder[A, CC[A]](empty[A])

  def setBuilderFactory[A] = new BuilderFactory[A, CC[A], CC[_]] {
    def apply(from: CC[_]) = newBuilder[A]
  }
}
