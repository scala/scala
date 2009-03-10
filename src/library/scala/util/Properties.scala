/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util

private[scala] trait PropertiesTrait
{
  import java.io.{ IOException, PrintWriter }
  protected def propCategory: String      // specializes the remainder of the values
  protected def pickJarBasedOn: Class[_]  // props file comes from jar containing this

  /** The name of the properties file */
  protected val propFilename = "/" + propCategory + ".properties"

  /** The loaded properties */
  protected lazy val props: java.util.Properties = {
    val props = new java.util.Properties
    val stream = pickJarBasedOn getResourceAsStream propFilename
    if (stream ne null)
      quietlyDispose(props load stream, stream.close)

    props
  }

  private def quietlyDispose(action: => Unit, disposal: => Unit) =
    try     { action }
    finally {
        try     { disposal }
        catch   { case _: IOException => }
    }

  // for values based on system properties
  protected def sysprop(name: String, default: String) =
    System.getProperty(name, default)

  // for values based on propFilename
  protected def prop(name: String, default: String): String =
    props.getProperty(name, default)

  // XXX file.encoding should not be here, as it causes system setting to
  // be ignored.  See https://lampsvn.epfl.ch/trac/scala/ticket/1581

  /** The version number of the jar this was loaded from plus "version " prefix,
   *  or "version (unknown)" if it cannot be determined.
   */
  val versionString         = "version " + prop("version.number", "(unknown)")
  val copyrightString       = prop("copyright.string", "(c) 2002-2009 LAMP/EPFL")
  val encodingString        = prop("file.encoding", "UTF8")

  // provide a main method so version info can be obtained by running this
  private val writer = new java.io.PrintWriter(Console.err, true)
  private val versionMsg = "Scala %s %s -- %s".format(propCategory, versionString, copyrightString)
  def main(args: Array[String]) { writer println versionMsg }
}

/** A utility to load the library properties from a Java properties file
 *  included in the jar.
 *
 *  @author Stephane Micheloud
 */
object Properties extends PropertiesTrait {
  protected def propCategory    = "library"
  protected def pickJarBasedOn  = classOf[Application]
}
