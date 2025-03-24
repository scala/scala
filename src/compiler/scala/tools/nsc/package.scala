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

package scala.tools

import scala.reflect.internal.util.StringContextStripMarginOps

package object nsc {
  type Mode = scala.reflect.internal.Mode
  val Mode = scala.reflect.internal.Mode

  def EXPRmode = Mode.EXPRmode

  type Phase = scala.reflect.internal.Phase
  val NoPhase = scala.reflect.internal.NoPhase

  type Variance = scala.reflect.internal.Variance
  val Variance = scala.reflect.internal.Variance

  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  type MissingRequirementError = scala.reflect.internal.MissingRequirementError
  val MissingRequirementError = scala.reflect.internal.MissingRequirementError

  @deprecated("Use scala.reflect.internal.util.ListOfNil", "2.11.0")
  lazy val ListOfNil = scala.reflect.internal.util.ListOfNil

  /** Adds the `sm` interpolator to a [[scala.StringContext]].
   */
  implicit val `strip margin`: StringContext => StringContextStripMarginOps = StringContextStripMarginOps
}
