/* NSC -- new Scala compiler
 * Copyright 2006-2009 LAMP/EPFL
 * @author  Stephane Micheloud
 */

// $Id$

package scala.tools.nsc
import scala.util.PropertiesTrait

/** A utility to load the compiler properties from a Java properties file
 *  included in the jar.
 */
object Properties extends PropertiesTrait {
  protected def propCategory    = "compiler"
  protected def pickJarBasedOn  = classOf[Global]

  // settings based on jar properties
  val fileEndingString      = prop("file.ending", ".scala|.java")
  val residentPromptString  = prop("resident.prompt", "\nnsc> ")
  val shellPromptString     = prop("shell.prompt", "\nscala> ")

  // settings based on System properties
  val isWin                 = sysprop("os.name", "") startsWith "Windows"
  val scalaHome             = sysprop("scala.home", null)
  val envClasspath          = sysprop("env.classpath", null)
  val msilILasm             = sysprop("msil.ilasm", "")
  val cmdName               = if (isWin) "scala.bat" else "scala"
}
