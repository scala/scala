/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.util

import java.io.{ IOException, PrintWriter }
import java.util.jar.Attributes.{ Name => AttributeName }

/** Loads `library.properties` from the jar. */
object Properties extends PropertiesTrait {
  protected def propCategory    = "library"
  protected def pickJarBasedOn  = classOf[Option[_]]

  /** Scala manifest attributes.
   */
  val ScalaCompilerVersion = new AttributeName("Scala-Compiler-Version")
}

private[scala] trait PropertiesTrait {
  protected def propCategory: String      // specializes the remainder of the values
  protected def pickJarBasedOn: Class[_]  // props file comes from jar containing this

  /** The name of the properties file */
  protected val propFilename = "/" + propCategory + ".properties"

  /** The loaded properties */
  protected lazy val scalaProps: java.util.Properties = {
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

  def propIsSet(name: String)                   = System.getProperty(name) != null
  def propIsSetTo(name: String, value: String)  = propOrNull(name) == value
  def propOrElse(name: String, alt: String)     = System.getProperty(name, alt)
  def propOrEmpty(name: String)                 = propOrElse(name, "")
  def propOrNull(name: String)                  = propOrElse(name, null)
  def propOrNone(name: String)                  = Option(propOrNull(name))
  def propOrFalse(name: String)                 = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  def setProp(name: String, value: String)      = System.setProperty(name, value)
  def clearProp(name: String)                   = System.clearProperty(name)

  def envOrElse(name: String, alt: String)      = Option(System getenv name) getOrElse alt
  def envOrNone(name: String)                   = Option(System getenv name)

  // for values based on propFilename
  def scalaPropOrElse(name: String, alt: String): String = scalaProps.getProperty(name, alt)
  def scalaPropOrEmpty(name: String): String             = scalaPropOrElse(name, "")
  def scalaPropOrNone(name: String): Option[String]      = Option(scalaProps.getProperty(name))

  /** The numeric portion of the runtime Scala version, if this is a final
   *  release.  If for instance the versionString says "version 2.9.0.final",
   *  this would return Some("2.9.0").
   *
   *  @return Some(version) if this is a final release build, None if
   *  it is an RC, Beta, etc. or was built from source, or if the version
   *  cannot be read.
   */
  val releaseVersion = 
    for {
      v <- scalaPropOrNone("maven.version.number")
      if !(v endsWith "-SNAPSHOT")
    } yield v

  /** The development Scala version, if this is not a final release.
   *  The precise contents are not guaranteed, but it aims to provide a
   *  unique repository identifier (currently the svn revision) in the
   *  fourth dotted segment if the running version was built from source.
   *
   *  @return Some(version) if this is a non-final version, None if this
   *  is a final release or the version cannot be read.
   */
  val developmentVersion = 
    for {
      v <- scalaPropOrNone("maven.version.number")
      if v endsWith "-SNAPSHOT"
      ov <- scalaPropOrNone("version.number")
    } yield ov

  /** Either the development or release version if known, otherwise
   *  the empty string.
   */
  def versionNumberString = scalaPropOrEmpty("version.number")

  /** The version number of the jar this was loaded from plus "version " prefix,
   *  or "version (unknown)" if it cannot be determined.
   */
  val versionString         = "version " + scalaPropOrElse("version.number", "(unknown)")
  val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2013, LAMP/EPFL")

  /** This is the encoding to use reading in source files, overridden with -encoding
   *  Note that it uses "prop" i.e. looks in the scala jar, not the system properties.
   */
  def sourceEncoding        = scalaPropOrElse("file.encoding", "UTF-8")
  def sourceReader          = scalaPropOrElse("source.reader", "scala.tools.nsc.io.SourceReader")

  /** This is the default text encoding, overridden (unreliably) with
   *  `JAVA_OPTS="-Dfile.encoding=Foo"`
   */
  def encodingString        = propOrElse("file.encoding", "UTF-8")

  /** The default end of line character.
   */
  def lineSeparator         = propOrElse("line.separator", "\n")

  /** Various well-known properties.
   */
  def javaClassPath         = propOrEmpty("java.class.path")
  def javaHome              = propOrEmpty("java.home")
  def javaVendor            = propOrEmpty("java.vendor")
  def javaVersion           = propOrEmpty("java.version")
  def javaVmInfo            = propOrEmpty("java.vm.info")
  def javaVmName            = propOrEmpty("java.vm.name")
  def javaVmVendor          = propOrEmpty("java.vm.vendor")
  def javaVmVersion         = propOrEmpty("java.vm.version")
  def osName                = propOrEmpty("os.name")
  def scalaHome             = propOrEmpty("scala.home")
  def tmpDir                = propOrEmpty("java.io.tmpdir")
  def userDir               = propOrEmpty("user.dir")
  def userHome              = propOrEmpty("user.home")
  def userName              = propOrEmpty("user.name")

  /** Some derived values.
   */
  def isWin                 = osName startsWith "Windows"
  def isMac                 = javaVendor startsWith "Apple"

  // This is looking for javac, tools.jar, etc.
  // Tries JDK_HOME first, then the more common but likely jre JAVA_HOME,
  // and finally the system property based javaHome.
  def jdkHome              = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  def versionMsg            = "Scala %s %s -- %s".format(propCategory, versionString, copyrightString)
  def scalaCmd              = if (isWin) "scala.bat" else "scala"
  def scalacCmd             = if (isWin) "scalac.bat" else "scalac"

  /** Can the java version be determined to be at least as high as the argument?
   *  Hard to properly future proof this but at the rate 1.7 is going we can leave
   *  the issue for our cyborg grandchildren to solve.
   */
  def isJavaAtLeast(version: String) = {
    val okVersions = version match {
      case "1.5"    => List("1.5", "1.6", "1.7")
      case "1.6"    => List("1.6", "1.7")
      case "1.7"    => List("1.7")
      case _        => Nil
    }
    okVersions exists (javaVersion startsWith _)
  }

  // provide a main method so version info can be obtained by running this
  def main(args: Array[String]) {
    val writer = new PrintWriter(Console.err, true)
    writer println versionMsg
  }
}
