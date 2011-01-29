/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools

package object nsc {
  @deprecated("Use a class in the scala.tools.nsc.interpreter package.")
  type InterpreterSettings = interpreter.ISettings
  @deprecated("Use a class in the scala.tools.nsc.interpreter package.")
  val InterpreterResults   = interpreter.Results
}