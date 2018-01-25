package strawman.collection.mutable

import scala.AnyRef

/** A trait for cloneable collections.
  *
  *  @since 2.8
  *
  *  @tparam A    Type of the collection, covariant and with reference types as upperbound.
  */
trait Cloneable[+A <: AnyRef] extends scala.Cloneable {
  override def clone(): A = super.clone().asInstanceOf[A]
}
