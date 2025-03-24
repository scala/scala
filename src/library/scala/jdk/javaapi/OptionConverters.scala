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

package scala.jdk.javaapi

import java.util.{Optional, OptionalDouble, OptionalInt, OptionalLong}
import java.{lang => jl}

/** This object contains methods that convert between Scala `Option` and Java `Optional` types.
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[scala.jdk.OptionConverters]].
  *
  * @define primitiveNote Note: this method uses the boxed type `java.lang.X` instead of the
  *                       primitive type `scala.X` to improve compatibility when using it in
  *                       Java code (the Scala compiler emits `C[Int]` as `C[Object]` in bytecode
  *                       due to [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In
  *                       Scala code, add `import scala.jdk.OptionConverters._` and use the
  *                       extension methods instead.
  */
object OptionConverters {
  /** Convert a Scala `Option` to a Java `Optional` */
  def toJava[A](o: Option[A]): Optional[A] = o match {
    case Some(a) => Optional.ofNullable(a)
    case _ => Optional.empty[A]
  }

  /** Convert a Scala `Option[java.lang.Double]` to a Java `OptionalDouble`
    *
    * $primitiveNote
    */
  def toJavaOptionalDouble(o: Option[jl.Double]): OptionalDouble = o match {
    case Some(a) => OptionalDouble.of(a)
    case _ => OptionalDouble.empty
  }

  /** Convert a Scala `Option[java.lang.Integer]` to a Java `OptionalInt`
    *
    * $primitiveNote
    */
  def toJavaOptionalInt(o: Option[jl.Integer]): OptionalInt = o match {
    case Some(a) => OptionalInt.of(a)
    case _ => OptionalInt.empty
  }

  /** Convert a Scala `Option[java.lang.Long]` to a Java `OptionalLong`
    *
    * $primitiveNote
    */
  def toJavaOptionalLong(o: Option[jl.Long]): OptionalLong = o match {
    case Some(a) => OptionalLong.of(a)
    case _ => OptionalLong.empty
  }

  /** Convert a Java `Optional` to a Scala `Option` */
  def toScala[A](o: Optional[A]): Option[A] = if (o.isPresent) Some(o.get) else None

  /** Convert a Java `OptionalDouble` to a Scala `Option[java.lang.Double]`
    *
    * $primitiveNote
    */
  def toScala(o: OptionalDouble): Option[jl.Double] = if (o.isPresent) Some(o.getAsDouble) else None

  /** Convert a Java `OptionalInt` to a Scala `Option[java.lang.Integer]`
    *
    * $primitiveNote
    */
  def toScala(o: OptionalInt): Option[jl.Integer] = if (o.isPresent) Some(o.getAsInt) else None

  /** Convert a Java `OptionalLong` to a Scala `Option[java.lang.Long]`
    *
    * $primitiveNote
    */
  def toScala(o: OptionalLong): Option[jl.Long] = if (o.isPresent) Some(o.getAsLong) else None
}
