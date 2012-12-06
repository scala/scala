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

  // settings based on jar properties
  def fileEndingString     = scalaPropOrElse("file.ending", ".scala|.java")
  def residentPromptString = scalaPropOrElse("resident.prompt", "\nnsc> ")
  def shellPromptString    = scalaPropOrElse("shell.prompt", "\nscala> ")

  // derived values
  def isEmacsShell         = propOrEmpty("env.emacs") != ""
}
