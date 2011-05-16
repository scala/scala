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

  type Phase = scala.reflect.internal.Phase
  val NoPhase = scala.reflect.internal.NoPhase

  type FatalError = scala.reflect.internal.FatalError
  val FatalError = scala.reflect.internal.FatalError

  type MissingRequirementError = scala.reflect.internal.MissingRequirementError
  val MissingRequirementError = scala.reflect.internal.MissingRequirementError
}
