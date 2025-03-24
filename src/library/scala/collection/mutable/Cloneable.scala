/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.mutable


/** A trait for cloneable collections.
  *
  *  @tparam C    Type of the collection, covariant and with reference types as upperbound.
  */
trait Cloneable[+C <: AnyRef] extends scala.Cloneable {
  override def clone(): C = super.clone().asInstanceOf[C]
}
