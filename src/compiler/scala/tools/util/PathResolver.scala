/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools
package util

import scala.tools.reflect.WrappedProperties.AccessControl
import scala.tools.nsc.{ Settings }
import scala.tools.nsc.util.{ ClassPath, JavaClassPath }
import scala.reflect.io.{ File, Directory, Path, AbstractFile }
import scala.reflect.runtime.ReflectionUtils
import ClassPath.{ JavaContext, DefaultJavaContext, join, split }
import PartialFunction.condOpt
import scala.language.postfixOps

// Loosely based on the draft specification at:
// https://wiki.scala-lang.org/display/SIW/Classpath

object PathResolver {
  // Imports property/environment functions which suppress security exceptions.
  import AccessControl._
  import scala.compat.Platform.EOL

  implicit class MkLines(val t: TraversableOnce[_]) extends AnyVal {
    def mkLines: String = t.mkString("", EOL, EOL)
    def mkLines(header: String, indented: Boolean = false, embraced: Boolean = false): String = {
      val space = "\u0020"
      val sep = if (indented) EOL + space * 2 else EOL
      val (lbrace, rbrace) = if (embraced) (space + "{", EOL + "}") else ("", "")
      t.mkString(header + lbrace + sep, sep, rbrace + EOL)
    }
  }
  implicit class AsLines(val s: String) extends AnyVal {
    // sm"""...""" could do this in one pass
    def asLines = s.trim.stripMargin.lines.mkLines
  }

  /** pretty print class path */
  def ppcp(s: String) = split(s) match {
    case Nil      => ""
    case Seq(x)   => x
    case xs       => xs.mkString(EOL, EOL, "")
  }

  /** Values found solely by inspecting environment or property variables.
   */
  object Environment {
    private def searchForBootClasspath = (
      systemProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
    )

    /** Environment variables which java pays attention to so it
     *  seems we do as well.
     */
    def sourcePathEnv       = envOrElse("SOURCEPATH", "")

    def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
    def javaExtDirs         = propOrEmpty("java.ext.dirs")
    def scalaHome           = propOrEmpty("scala.home")
    def scalaExtDirs        = propOrEmpty("scala.ext.dirs")

    /** The java classpath and whether to use it. */
    def javaUserClassPath   = propOrElse("java.class.path", "")
    def useJavaClassPath    = propOrFalse("scala.usejavacp")

    override def toString = s"""
      |object Environment {
      |  scalaHome          = $scalaHome (useJavaClassPath = $useJavaClassPath)
      |  javaBootClassPath  = <${javaBootClassPath.length} chars>
      |  javaExtDirs        = ${ppcp(javaExtDirs)}
      |  javaUserClassPath  = ${ppcp(javaUserClassPath)}
      |  scalaExtDirs       = ${ppcp(scalaExtDirs)}
      |}""".asLines
  }

  /** Default values based on those in Environment as interpreted according
   *  to the path resolution specification.
   */
  object Defaults {
    def scalaSourcePath   = Environment.sourcePathEnv
    def javaBootClassPath = Environment.javaBootClassPath
    def javaUserClassPath = Environment.javaUserClassPath
    def javaExtDirs       = Environment.javaExtDirs
    def useJavaClassPath  = Environment.useJavaClassPath

    def scalaHome         = Environment.scalaHome
    def scalaHomeDir      = Directory(scalaHome)
    def scalaLibDir       = Directory(scalaHomeDir / "lib")
    def scalaClassesDir   = Directory(scalaHomeDir / "classes")

    def scalaLibAsJar     = File(scalaLibDir / "scala-library.jar")
    def scalaLibAsDir     = Directory(scalaClassesDir / "library")

    def scalaLibDirFound: Option[Directory] =
      if (scalaLibAsJar.isFile) Some(scalaLibDir)
      else if (scalaLibAsDir.isDirectory) Some(scalaClassesDir)
      else None

    def scalaLibFound =
      if (scalaLibAsJar.isFile) scalaLibAsJar.path
      else if (scalaLibAsDir.isDirectory) scalaLibAsDir.path
      else ""

    // XXX It must be time for someone to figure out what all these things
    // are intended to do.  This is disabled here because it was causing all
    // the scala jars to end up on the classpath twice: one on the boot
    // classpath as set up by the runner (or regular classpath under -nobootcp)
    // and then again here.
    def scalaBootClassPath  = ""
    def scalaExtDirs = Environment.scalaExtDirs
    def scalaPluginPath = (scalaHomeDir / "misc" / "scala-devel" / "plugins").path

    override def toString = s"""
      |object Defaults {
      |  scalaHome            = $scalaHome
      |  javaBootClassPath    = ${ppcp(javaBootClassPath)}
      |  scalaLibDirFound     = $scalaLibDirFound
      |  scalaLibFound        = $scalaLibFound
      |  scalaBootClassPath   = ${ppcp(scalaBootClassPath)}
      |  scalaPluginPath      = ${ppcp(scalaPluginPath)}
      |}""".asLines
  }

  /** Locations discovered by supplemental heuristics.
   */
  object SupplementalLocations {

    /** The platform-specific support jar.
     *
     *  Usually this is `tools.jar` in the jdk/lib directory of the platform distribution.
     *
     *  The file location is determined by probing the lib directory under JDK_HOME or JAVA_HOME,
     *  if one of those environment variables is set, then the lib directory under java.home,
     *  and finally the lib directory under the parent of java.home. Or, as a last resort,
     *  search deeply under those locations (except for the parent of java.home, on the notion
     *  that if this is not a canonical installation, then that search would have little
     *  chance of succeeding).
     */
    def platformTools: Option[File] = {
      val jarName = "tools.jar"
      def jarPath(path: Path) = (path / "lib" / jarName).toFile
      def jarAt(path: Path) = {
        val f = jarPath(path)
        if (f.isFile) Some(f) else None
      }
      val jdkDir = {
        val d = Directory(jdkHome)
        if (d.isDirectory) Some(d) else None
      }
      def deeply(dir: Directory) = dir.deepFiles find (_.name == jarName)

      val home    = envOrSome("JDK_HOME", envOrNone("JAVA_HOME")) map (p => Path(p))
      val install = Some(Path(javaHome))

      (home flatMap jarAt) orElse (install flatMap jarAt) orElse (install map (_.parent) flatMap jarAt) orElse
        (jdkDir flatMap deeply)
    }
    override def toString = s"""
      |object SupplementalLocations {
      |  platformTools        = $platformTools
      |}""".asLines
  }

  // used in PathResolver constructor
  private object NoImplClassJavaContext extends JavaContext {
    override def isValidName(name: String): Boolean =
      !ReflectionUtils.scalacShouldntLoadClassfile(name)
  }

  // called from scalap
  def fromPathString(path: String, context: JavaContext = DefaultJavaContext): JavaClassPath = {
    val s = new Settings()
    s.classpath.value = path
    new PathResolver(s, context) result
  }

  /** With no arguments, show the interesting values in Environment and Defaults.
   *  If there are arguments, show those in Calculated as if those options had been
   *  given to a scala runner.
   */
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(Environment)
      println(Defaults)
    }
    else {
      val settings = new Settings()
      val rest = settings.processArguments(args.toList, processAll = false)._2
      val pr = new PathResolver(settings)
      println(" COMMAND: 'scala %s'".format(args.mkString(" ")))
      println("RESIDUAL: 'scala %s'\n".format(rest.mkString(" ")))
      pr.result.show()
    }
  }
}

class PathResolver(settings: Settings, context: JavaContext) {
  import PathResolver.{ Defaults, Environment, AsLines, MkLines, ppcp }

  def this(settings: Settings) = this(settings,
      if (settings.YnoLoadImplClass) PathResolver.NoImplClassJavaContext
      else DefaultJavaContext)

  private def cmdLineOrElse(name: String, alt: String) = {
    (commandLineFor(name) match {
      case Some("") => None
      case x        => x
    }) getOrElse alt
  }

  private def commandLineFor(s: String): Option[String] = condOpt(s) {
    case "javabootclasspath"  => settings.javabootclasspath.value
    case "javaextdirs"        => settings.javaextdirs.value
    case "bootclasspath"      => settings.bootclasspath.value
    case "extdirs"            => settings.extdirs.value
    case "classpath" | "cp"   => settings.classpath.value
    case "sourcepath"         => settings.sourcepath.value
  }

  /** Calculated values based on any given command line options, falling back on
   *  those in Defaults.
   */
  object Calculated {
    def scalaHome           = Defaults.scalaHome
    def useJavaClassPath    = settings.usejavacp.value || Defaults.useJavaClassPath
    def useManifestClassPath= settings.usemanifestcp.value
    def javaBootClassPath   = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def javaExtDirs         = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def javaUserClassPath   = if (useJavaClassPath) Defaults.javaUserClassPath else ""
    def scalaBootClassPath  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def scalaExtDirs        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    /** Scaladoc doesn't need any bootstrapping, otherwise will create errors such as:
     * [scaladoc] ../scala-trunk/src/reflect/scala/reflect/macros/Reifiers.scala:89: error: object api is not a member of package reflect
     * [scaladoc] case class ReificationException(val pos: reflect.api.PositionApi, val msg: String) extends Throwable(msg)
     * [scaladoc]                                              ^
     * because the bootstrapping will look at the sourcepath and create package "reflect" in "<root>"
     * and then when typing relative names, instead of picking <root>.scala.relect, typedIdentifier will pick up the
     * <root>.reflect package created by the bootstrapping. Thus, no bootstrapping for scaladoc!
     * TODO: we should refactor this as a separate -bootstrap option to have a clean implementation, no? */
    def sourcePath          = if (!settings.isScaladoc) cmdLineOrElse("sourcepath", Defaults.scalaSourcePath) else ""

    /** Against my better judgment, giving in to martin here and allowing
     *  CLASSPATH to be used automatically.  So for the user-specified part
     *  of the classpath:
     *
     *  - If -classpath or -cp is given, it is that
     *  - Otherwise, if CLASSPATH is set, it is that
     *  - If neither of those, then "." is used.
     */
    def userClassPath = (
      if (!settings.classpath.isDefault)
        settings.classpath.value
      else sys.env.getOrElse("CLASSPATH", ".")
    )

    import context._

    // Assemble the elements!
    def basis = List[Traversable[ClassPath[AbstractFile]]](
      classesInPath(javaBootClassPath),             // 1. The Java bootstrap class path.
      contentsOfDirsInPath(javaExtDirs),            // 2. The Java extension class path.
      classesInExpandedPath(javaUserClassPath),     // 3. The Java application class path.
      classesInPath(scalaBootClassPath),            // 4. The Scala boot class path.
      contentsOfDirsInPath(scalaExtDirs),           // 5. The Scala extension class path.
      classesInExpandedPath(userClassPath),         // 6. The Scala application class path.
      classesInManifest(useManifestClassPath),      // 8. The Manifest class path.
      sourcesInPath(sourcePath)                     // 7. The Scala source path.
    )

    lazy val containers = basis.flatten.distinct

    override def toString = s"""
      |object Calculated {
      |  scalaHome            = $scalaHome
      |  javaBootClassPath    = ${ppcp(javaBootClassPath)}
      |  javaExtDirs          = ${ppcp(javaExtDirs)}
      |  javaUserClassPath    = ${ppcp(javaUserClassPath)}
      |    useJavaClassPath   = $useJavaClassPath
      |  scalaBootClassPath   = ${ppcp(scalaBootClassPath)}
      |  scalaExtDirs         = ${ppcp(scalaExtDirs)}
      |  userClassPath        = ${ppcp(userClassPath)}
      |  sourcePath           = ${ppcp(sourcePath)}
      |}""".asLines
  }

  def containers = Calculated.containers

  lazy val result = {
    val cp = new JavaClassPath(containers.toIndexedSeq, context)
    if (settings.Ylogcp) {
      Console print f"Classpath built from ${settings.toConciseString} %n"
      Console print s"Defaults: ${PathResolver.Defaults}"
      Console print s"Calculated: $Calculated"

      val xs = (Calculated.basis drop 2).flatten.distinct
      Console print (xs mkLines (s"After java boot/extdirs classpath has ${xs.size} entries:", indented = true))
    }
    cp
  }

  def asURLs = result.asURLs
}
