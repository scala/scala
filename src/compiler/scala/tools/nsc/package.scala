/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

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
}
