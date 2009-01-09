/* NSC -- new Scala compiler
 * Copyright 2006-2009 LAMP/EPFL
 * @author  Stephane Micheloud
 */

// $Id$

package scala.tools.nsc

/** A utility to load the compiler properties from a Java properties file
 *  included in the jar.
 */
object Properties {

  /** The name of the properties file */
  private val propFilename = "/compiler.properties"

  /** The loaded properties */
  private val props = {
    val props = new java.util.Properties
    val stream = classOf[Global].getResourceAsStream(propFilename)
    if (stream != null)
      props.load(stream)
    props
  }

  val isWin = System.getProperty("os.name") startsWith "Windows"

  /** The version number of the jar this was loaded from, or
    * "(unknown)" if it cannot be determined.
    */
  val versionString: String = {
    val defaultString = "(unknown)"
    "version " + props.getProperty("version.number", defaultString)
  }

  val copyrightString: String = {
    val defaultString = "(c) 2002-2009 LAMP/EPFL"
    props.getProperty("copyright.string", defaultString)
  }

  val encodingString: String = {
    val defaultString = "UTF8" //"ISO-8859-1"
    props.getProperty("file.encoding", defaultString)
  }

  val fileEndingString: String = {
    val defaultString = ".scala|.java"
    props.getProperty("file.ending", defaultString)
  }

  val residentPromptString: String = {
    val defaultString = "\nnsc> "
    props.getProperty("resident.prompt", defaultString)
  }

  val shellPromptString: String = {
    val defaultString = "\nscala> "
    props.getProperty("shell.prompt", defaultString)
  }

  val scalaHome: String =
    System.getProperty("scala.home")

  val envClasspath: String =
    System.getProperty("env.classpath")

  val cmdName: String =
    if (isWin) "scala.bat" else "scala"

  val msilILasm: String =
    System.getProperty("msil.ilasm", "")
}
