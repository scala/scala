package scala.collection.generic

/** A template for companion objects of Set and subclasses thereof.
 */
abstract class SortedSetFactory[CC[A] <: SortedSet[A] with SortedSetTemplate[A, CC[A]]] {
  type Coll = CC[_]

  def newBuilder[A](implicit ord: Ordering[A]): Builder[A, CC[A]]

  def empty[A](implicit ord: Ordering[A]): CC[A]

  def apply[A](elems: A*)(implicit ord: Ordering[A]): CC[A] = (newBuilder[A](ord) ++= elems).result

  implicit def newBuilderFactory[A](implicit ord : Ordering[A]) : BuilderFactory[A, CC[A], Coll] = new SortedSetBuilderFactory()(ord);

  class SortedSetBuilderFactory[A](implicit ord: Ordering[A]) extends BuilderFactory[A, CC[A], Coll] {
    def apply(from: Coll) = newBuilder[A](ord)
  }
}
