/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Stephane Micheloud
 */

package scala.tools.nsc

/** Loads `compiler.properties` from the jar archive file.
 */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory   = "compiler"
  protected def pickJarBasedOn = classOf[Global]

  // settings based on jar properties, falling back to System prefixed by "scala."
  def residentPromptString = scalaPropOrElse("resident.prompt", "\nnsc> ")
  def shellPromptString    = scalaPropOrElse("shell.prompt", "\nscala> ")
  // message to display at EOF (which by default ends with
  // a newline so as not to break the user's terminal)
  def shellInterruptedString = scalaPropOrElse("shell.interrupted", f":quit$lineSeparator")

  // derived values
  def isEmacsShell         = propOrEmpty("env.emacs") != ""
}
