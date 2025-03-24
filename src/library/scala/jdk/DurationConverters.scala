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

import java.time.{Duration => JDuration}

import scala.concurrent.duration.FiniteDuration

/** This object provides extension methods that convert between Scala and Java duration types.
  *
  * When writing Java code, use the explicit conversion methods defined in
  * [[javaapi.DurationConverters]] instead.
  */
object DurationConverters {
  implicit class JavaDurationOps(private val duration: JDuration) extends AnyVal {
    /** Convert a Java duration to a Scala duration, see [[javaapi.DurationConverters.toScala]]. */
    def toScala: FiniteDuration = javaapi.DurationConverters.toScala(duration)
  }

  implicit final class ScalaDurationOps(private val duration: FiniteDuration) extends AnyVal {
    /** Convert a Scala duration to a Java duration, see [[javaapi.DurationConverters.toJava]]. */
    def toJava: JDuration = javaapi.DurationConverters.toJava(duration)
  }
}
