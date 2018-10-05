/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
