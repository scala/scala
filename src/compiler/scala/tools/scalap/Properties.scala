/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2007, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

// $Id: Properties.scala 9834 2007-01-31 16:31:25Z michelou $

package scala.tools.scalap

/** A utility to load the compiler properties from a Java properties file
 *  included in the jar.
 */
object Properties {

  /** The name of the properties file */
  private val propFilename = "/decoder.properties"

  /** The loaded properties */
  private val props = {
    val props = new java.util.Properties
    val stream = classOf[Classfile].getResourceAsStream(propFilename)
    if (stream != null)
      props.load(stream)
    props
  }

  /** The version number of the jar this was loaded from, or
    * "(unknown)" if it cannot be determined.
    */
  val versionString: String = {
    val defaultString = "(unknown)"
    "version " + props.getProperty("version.number")
  }

  val copyrightString: String = {
    val defaultString = "(c) 2002-2006 LAMP/EPFL"
    props.getProperty("copyright.string", defaultString)
  }

  val scalaHome: String =
    System.getProperty("scala.home")

  val cmdName: String = {
    val isWin = System.getProperty("os.name") startsWith "Windows"
    if (isWin) "scala.bat" else "scala"
  }

}
