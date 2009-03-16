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

  protected def onull[T <: AnyRef](x: T) = if (x eq null) None else Some(x)
  private def quietlyDispose(action: => Unit, disposal: => Unit) =
    try     { action }
    finally {
        try     { disposal }
        catch   { case _: IOException => }
    }

  // for values based on system properties
  def sysprop(name: String): String                   = sysprop(name, "")
  def sysprop(name: String, default: String): String  = System.getProperty(name, default)
  def syspropset(name: String, value: String)         = System.setProperty(name, value)

  // for values based on propFilename
  def prop(name: String): String                      = props.getProperty(name, "")
  def prop(name: String, default: String): String     = props.getProperty(name, default)

  // XXX file.encoding should not be here, as it causes system setting to
  // be ignored.  See https://lampsvn.epfl.ch/trac/scala/ticket/1581

  /** The version number of the jar this was loaded from plus "version " prefix,
   *  or "version (unknown)" if it cannot be determined.
   */
  val versionString         = "version " + prop("version.number", "(unknown)")
  val copyrightString       = prop("copyright.string", "(c) 2002-2009 LAMP/EPFL")
  val encodingString        = prop("file.encoding", "UTF8")
  val isWin                 = sysprop("os.name") startsWith "Windows"
  val isMac                 = sysprop("java.vendor") startsWith "Apple"
  val javaClassPath         = sysprop("java.class.path")
  val javaHome              = sysprop("java.home")
  val javaVmName            = sysprop("java.vm.name")
  val javaVmVersion         = sysprop("java.vm.version")
  val javaVmInfo            = sysprop("java.vm.info")
  val javaVersion           = sysprop("java.version")
  val scalaHome             = sysprop("scala.home", null) // XXX places do null checks...

  // provide a main method so version info can be obtained by running this
  private val writer = new java.io.PrintWriter(Console.err, true)
  def versionMsg            = "Scala %s %s -- %s".format(propCategory, versionString, copyrightString)
  def main(args: Array[String]) { writer println versionMsg }
}

/** Loads library.properties from the jar. */
object Properties extends PropertiesTrait {
  protected def propCategory    = "library"
  protected def pickJarBasedOn  = classOf[Application]
}
