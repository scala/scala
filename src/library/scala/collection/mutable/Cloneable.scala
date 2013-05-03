/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

/** A trait for cloneable collections.
 *
 *  @since 2.8
 *
 *  @tparam A    Type of the elements contained in the collection, covariant and with reference types as upperbound.
 */
trait Cloneable[+A <: AnyRef] extends scala.Cloneable {
  override def clone(): A = super.clone().asInstanceOf[A]
}
