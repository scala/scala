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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala


/** A tuple of 1 elements; the canonical representation of a [[scala.Product1]].
 *
 *  @constructor  Create a new tuple with 1 elements.
 *  @param  _1   Element 1 of this Tuple1
 */
final case class Tuple1[@specialized(Int, Long, Double) +T1](_1: T1)
  extends Product1[T1]
{
  override def toString(): String = "(" + _1 + ")"
  
}
