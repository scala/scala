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
  val fileEndingString      = prop("file.ending", ".scala|.java")
  val residentPromptString  = prop("resident.prompt", "\nnsc> ")
  val shellPromptString     = prop("shell.prompt", "\nscala> ")

  // derived values
  val cmdName               = if (isWin) "scala.bat" else "scala"
  val fileEndings           = fileEndingString.split("""\|""").toList

}
