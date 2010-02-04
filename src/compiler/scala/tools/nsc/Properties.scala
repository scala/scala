/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Stephane Micheloud
 */

// $Id$

package scala.tools.nsc

/** Loads compiler.properties from the jar. */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "compiler"
  protected def pickJarBasedOn  = classOf[Global]

  // settings based on jar properties
  def fileEndingString      = prop("file.ending", ".scala|.java")
  def residentPromptString  = prop("resident.prompt", "\nnsc> ")
  def shellPromptString     = prop("shell.prompt", "\nscala> ")

  // derived values
  def cmdName               = if (isWin) "scala.bat" else "scala"
  def fileEndings           = fileEndingString.split("""\|""").toList
}
