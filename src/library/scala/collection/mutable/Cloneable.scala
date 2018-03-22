package scala.collection.mutable


/** A trait for cloneable collections.
  *
  *  @since 2.8
  *
  *  @tparam C    Type of the collection, covariant and with reference types as upperbound.
  */
trait Cloneable[+C <: AnyRef] extends scala.Cloneable {
  override def clone(): C = super.clone().asInstanceOf[C]
}
