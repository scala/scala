/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Stephane Micheloud
 */


package scala.tools.nsc

/** Loads compiler.properties from the jar. */
object Properties extends scala.util.PropertiesTrait {
  protected def propCategory    = "compiler"
  protected def pickJarBasedOn  = classOf[Global]

  def isJava5 = javaVersion startsWith "1.5"
  def isJava6 = javaVersion startsWith "1.6"

  // settings based on jar properties
  def fileEndingString      = scalaPropOrElse("file.ending", ".scala|.java")
  def residentPromptString  = scalaPropOrElse("resident.prompt", "\nnsc> ")
  def shellPromptString     = scalaPropOrElse("shell.prompt", "\nscala> ")

  // settings based on system properties
  def msilLibPath           = propOrNone("msil.libpath")

  // derived values
  def isEmacsShell          = propOrEmpty("env.emacs") != ""
  def fileEndings           = fileEndingString.split("""\|""").toList
}
