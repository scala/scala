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

package scala.jdk

import java.util.{Optional, OptionalDouble, OptionalInt, OptionalLong}
import java.{lang => jl}

import scala.annotation.implicitNotFound

/** A type class implementing conversions from a generic Scala `Option` or Java `Optional` to
  * a specialized Java variant (for `Double`, `Int` and `Long`).
  *
  * @tparam A the primitive type wrapped in an option
  * @tparam O the specialized Java `Optional` wrapping an element of type `A`
  */
@implicitNotFound("No specialized Optional type exists for elements of type ${A}")
sealed abstract class OptionShape[A, O] {
  /** Converts from `Optional` to the specialized variant `O` */
  def fromJava(o: Optional[A]): O
  /** Converts from `Option` to the specialized variant `O` */
  def fromScala(o: Option[A]): O
}

object OptionShape {
  implicit val doubleOptionShape: OptionShape[Double, OptionalDouble] = new OptionShape[Double, OptionalDouble] {
    def fromJava(o: Optional[Double]): OptionalDouble =
      if (o.isPresent) OptionalDouble.of(o.get) else OptionalDouble.empty

    def fromScala(o: Option[Double]): OptionalDouble = o match {
      case Some(d) => OptionalDouble.of(d)
      case _ => OptionalDouble.empty
    }
  }
  implicit val jDoubleOptionShape: OptionShape[jl.Double, OptionalDouble] = doubleOptionShape.asInstanceOf[OptionShape[jl.Double, OptionalDouble]]

  implicit val intOptionShape: OptionShape[Int, OptionalInt] = new OptionShape[Int, OptionalInt] {
    def fromJava(o: Optional[Int]): OptionalInt =
      if (o.isPresent) OptionalInt.of(o.get) else OptionalInt.empty

    def fromScala(o: Option[Int]): OptionalInt = o match {
      case Some(d) => OptionalInt.of(d)
      case _ => OptionalInt.empty
    }
  }
  implicit val jIntegerOptionShape: OptionShape[jl.Integer, OptionalInt] = intOptionShape.asInstanceOf[OptionShape[jl.Integer, OptionalInt]]

  implicit val longOptionShape: OptionShape[Long, OptionalLong] = new OptionShape[Long, OptionalLong] {
    def fromJava(o: Optional[Long]): OptionalLong =
      if (o.isPresent) OptionalLong.of(o.get) else OptionalLong.empty

    def fromScala(o: Option[Long]): OptionalLong = o match {
      case Some(d) => OptionalLong.of(d)
      case _ => OptionalLong.empty
    }
  }
  implicit val jLongOptionShape: OptionShape[jl.Long, OptionalLong] = longOptionShape.asInstanceOf[OptionShape[jl.Long, OptionalLong]]
}
