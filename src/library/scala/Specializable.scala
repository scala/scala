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

/** A common supertype for companions of specializable types.
 *  Should not be extended in user code.
 */
trait Specializable

object Specializable {
  // No type parameter in @specialized annotation.
  trait SpecializedGroup

  // Smuggle a list of types by way of a tuple upon which Group is parameterized.
  class Group[T >: Null](value: T) extends SpecializedGroup

  final val Primitives:  Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit)] = null
  final val Everything:  Group[(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit, AnyRef)] = null
  final val Bits32AndUp: Group[(Int, Long, Float, Double)] = null
  final val Integral:    Group[(Byte, Short, Int, Long, Char)] = null
  final val AllNumeric:  Group[(Byte, Short, Int, Long, Char, Float, Double)] = null
  final val BestOfBreed: Group[(Int, Double, Boolean, Unit, AnyRef)] = null
}
