/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util

import java.io.{IOException, PrintWriter}
import java.util.jar.Attributes.{Name => AttributeName}
import scala.annotation.tailrec

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
  def propOrElse(name: String, alt: => String)  = Option(System.getProperty(name)).getOrElse(alt)
  def propOrEmpty(name: String)                 = propOrElse(name, "")
  def propOrNull(name: String)                  = propOrElse(name, null)
  def propOrNone(name: String)                  = Option(propOrNull(name))
  def propOrFalse(name: String)                 = propOrNone(name) exists (x => List("yes", "on", "true") contains x.toLowerCase)
  def setProp(name: String, value: String)      = System.setProperty(name, value)
  def clearProp(name: String)                   = System.clearProperty(name)

  def envOrElse(name: String, alt: => String)   = Option(System getenv name) getOrElse alt
  def envOrNone(name: String)                   = Option(System getenv name)

  def envOrSome(name: String, alt: => Option[String])    = envOrNone(name) orElse alt

  // for values based on propFilename, falling back to System properties
  def scalaPropOrElse(name: String, alt: => String): String = scalaPropOrNone(name).getOrElse(alt)
  def scalaPropOrEmpty(name: String): String             = scalaPropOrElse(name, "")
  def scalaPropOrNone(name: String): Option[String]      = Option(scalaProps.getProperty(name)).orElse(propOrNone("scala." + name))

  /** The version of the Scala runtime, if this is not a snapshot.
   */
  val releaseVersion = scalaPropOrNone("maven.version.number").filterNot(_.endsWith("-SNAPSHOT"))

  /** The version of the Scala runtime, if this is a snapshot.
   */
  val developmentVersion = scalaPropOrNone("maven.version.number").filter(_.endsWith("-SNAPSHOT")).flatMap(_ => scalaPropOrNone("version.number"))

  /** The version of the Scala runtime, or the empty string if unknown.
   *
   *  Note that the version of the Scala library need not correlate with the version of the Scala compiler
   *  used to emit either the library or user code.
   *
   *  For example, Scala 3.0 and 3.1 use the Scala 2.13 library, which is reflected in this version string.
   *  For the Dotty version, see `dotty.tools.dotc.config.Properties.versionNumberString`.
   */
  def versionNumberString = scalaPropOrEmpty("version.number")

  /** A verbose alternative to [[versionNumberString]].
   */
  val versionString         = s"version ${scalaPropOrElse("version.number", "(unknown)")}"
  val copyrightString       = scalaPropOrElse("copyright.string", "Copyright 2002-2022, LAMP/EPFL and Lightbend, Inc.")

  /** This is the encoding to use reading in source files, overridden with -encoding.
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
  def lineSeparator         = System.lineSeparator()

  /* Various well-known properties. */
  def javaClassPath         = propOrEmpty("java.class.path")
  def javaHome              = propOrEmpty("java.home")
  def javaVendor            = propOrEmpty("java.vendor")
  def javaVersion           = propOrEmpty("java.version")
  def javaVmInfo            = propOrEmpty("java.vm.info")
  def javaVmName            = propOrEmpty("java.vm.name")
  def javaVmVendor          = propOrEmpty("java.vm.vendor")
  def javaVmVersion         = propOrEmpty("java.vm.version")
  def javaSpecVersion       = propOrEmpty("java.specification.version")
  def javaSpecVendor        = propOrEmpty("java.specification.vendor")
  def javaSpecName          = propOrEmpty("java.specification.name")
  def osName                = propOrEmpty("os.name")
  def scalaHome             = propOrEmpty("scala.home")
  def tmpDir                = propOrEmpty("java.io.tmpdir")
  def userDir               = propOrEmpty("user.dir")
  def userHome              = propOrEmpty("user.home")
  def userName              = propOrEmpty("user.name")

  /* Some derived values. */
  /** Returns `true` iff the underlying operating system is a version of Microsoft Windows. */
  def isWin                 = osName startsWith "Windows"
  // See https://mail.openjdk.java.net/pipermail/macosx-port-dev/2012-November/005148.html for
  // the reason why we don't follow developer.apple.com/library/mac/#technotes/tn2002/tn2110.
  /** Returns `true` iff the underlying operating system is a version of Apple Mac OSX.  */
  def isMac                 = osName startsWith "Mac OS X"
  /** Returns `true` iff the underlying operating system is a Linux distribution. */
  def isLinux               = osName startsWith "Linux"

  /* Some runtime values. */
  private[scala] def isAvian = javaVmName contains "Avian"

  private[scala] def coloredOutputEnabled: Boolean = propOrElse("scala.color", "auto") match {
    case "auto" => System.console() != null && !isWin
    case s      => s == "" || "true".equalsIgnoreCase(s)
  }

  // This is looking for javac, tools.jar, etc.
  // Tries JDK_HOME first, then the more common but likely jre JAVA_HOME,
  // and finally the system property based javaHome.
  def jdkHome               = envOrElse("JDK_HOME", envOrElse("JAVA_HOME", javaHome))

  private[scala] def versionFor(command: String) = s"Scala $command $versionString -- $copyrightString"

  def versionMsg            = versionFor(propCategory)
  def scalaCmd              = if (isWin) "scala.bat" else "scala"
  def scalacCmd             = if (isWin) "scalac.bat" else "scalac"

  /** Compares the given specification version to the specification version of the platform.
   *
   *  @param version a specification version number (legacy forms acceptable)
   *  @return `true` if the specification version of the current runtime
   *    is equal to or higher than the version denoted by the given string.
   *  @throws NumberFormatException if the given string is not a version string
   *
   *  @example {{{
   *  // In this example, the runtime's Java specification is assumed to be at version 8.
   *  isJavaAtLeast("1.8")            // true
   *  isJavaAtLeast("8")              // true
   *  isJavaAtLeast("9")              // false
   *  isJavaAtLeast("9.1")            // false
   *  isJavaAtLeast("1.9")            // throws
   *  }}}
   */
  def isJavaAtLeast(version: String): Boolean = {
    def versionOf(s: String, depth: Int): (Int, String) =
      s.indexOf('.') match {
        case 0 =>
          (-2, s.substring(1))
        case 1 if depth == 0 && s.charAt(0) == '1' =>
          val r0 = s.substring(2)
          val (v, r) = versionOf(r0, 1)
          val n = if (v > 8 || r0.isEmpty) -2 else v   // accept 1.8, not 1.9 or 1.
          (n, r)
        case -1 =>
          val n = if (!s.isEmpty) s.toInt else if (depth == 0) -2 else 0
          (n, "")
        case i  =>
          val r = s.substring(i + 1)
          val n = if (depth < 2 && r.isEmpty) -2 else s.substring(0, i).toInt
          (n, r)
      }
    @tailrec
    def compareVersions(s: String, v: String, depth: Int): Int = {
      if (depth >= 3) 0
      else {
        val (sn, srest) = versionOf(s, depth)
        val (vn, vrest) = versionOf(v, depth)
        if (vn < 0) -2
        else if (sn < vn) -1
        else if (sn > vn) 1
        else compareVersions(srest, vrest, depth + 1)
      }
    }
    compareVersions(javaSpecVersion, version, 0) match {
      case -2 => throw new NumberFormatException(s"Not a version: $version")
      case i  => i >= 0
    }
  }

  /** Compares the given specification version to the major version of the platform.
   *  @param version a specification major version number
   */
  def isJavaAtLeast(version: Int): Boolean = isJavaAtLeast(math.max(version, 0).toString)

  // provide a main method so version info can be obtained by running this
  def main(args: Array[String]): Unit = {
    val writer = new PrintWriter(Console.err, true)
    writer println versionMsg
  }
}
