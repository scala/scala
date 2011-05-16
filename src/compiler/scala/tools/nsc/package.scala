/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

package object nsc {
  @deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
  type InterpreterSettings = interpreter.ISettings
  @deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
  val InterpreterResults   = interpreter.Results

  type Phase = scala.reflect.common.Phase
  val NoPhase = scala.reflect.common.NoPhase

  type FatalError = scala.reflect.common.FatalError
  val FatalError = scala.reflect.common.FatalError

  type MissingRequirementError = scala.reflect.common.MissingRequirementError
  val MissingRequirementError = scala.reflect.common.MissingRequirementError
}
