/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import java.net.{ URL, MalformedURLException }
import scala.util.Properties._
import nsc.{ Settings, GenericRunnerSettings }
import nsc.util.{ ClassPath, JavaClassPath, ScalaClassLoader }
import nsc.io.{ File, Directory, Path }
import ClassPath.{ JavaContext, DefaultJavaContext, join, split }
import PartialFunction.condOpt

// Loosely based on the draft specification at:
// https://lampsvn.epfl.ch/trac/scala/wiki/Classpath

object PathResolver {
  def firstNonEmpty(xs: String*)            = xs find (_ != "") getOrElse ""

  private def fileOpt(f: Path): Option[String]      = f ifFile (_.path)
  private def dirOpt(d: Path): Option[String]       = d ifDirectory (_.path)
  private def expandToPath(p: Path)                 = join(ClassPath.expandPath(p.path, true): _*)
  private def expandToContents(p: Path)             = join(ClassPath.expandDir(p.path): _*)

  /** Map all classpath elements to absolute paths and reconstruct the classpath.
    */
  def makeAbsolute(cp: String) = ClassPath.map(cp, x => Path(x).toAbsolute.path)

  /** pretty print class path */
  def ppcp(s: String) = split(s) match {
    case Nil      => ""
    case Seq(x)   => x
    case xs       => xs map ("\n" + _) mkString
  }

  /** Values found solely by inspecting environment or property variables.
   */
  object Environment {
    private def searchForBootClasspath = {
      import scala.collection.JavaConversions._
      System.getProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
    }
    private def searchForScalaHome = {
      for (url <- ScalaClassLoader originOfClass classOf[ScalaObject] ; if url.getProtocol == "file") yield
        File(url.getFile).parent.path
    } getOrElse ""

    /** Environment variables which java pays attention to so it
     *  seems we do as well.
     */
    def classPathEnv        =  envOrElse("CLASSPATH", "")
    def sourcePathEnv       =  envOrElse("SOURCEPATH", "")

    def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
    def javaExtDirs         = propOrEmpty("java.ext.dirs")
    def scalaHome           = propOrEmpty("scala.home")
    def scalaExtDirs        = propOrEmpty("scala.ext.dirs")

    /** The java classpath and whether to use it. */
    def javaUserClassPath   = propOrElse("java.class.path", "")
    def useJavaClassPath    = propOrFalse("scala.usejavacp")

    override def toString = """
      |object Environment {
      |  scalaHome          = %s (useJavaClassPath = %s)
      |  javaBootClassPath  = <%d chars>
      |  javaExtDirs        = %s
      |  javaUserClassPath  = %s
      |  scalaExtDirs       = %s
      |}""".trim.stripMargin.format(
        scalaHome, useJavaClassPath,
        javaBootClassPath.length,
        ppcp(javaExtDirs),
        ppcp(javaUserClassPath),
        ppcp(scalaExtDirs)
      )
  }

  /** Default values based on those in Environment as interpretered according
   *  to the path resolution specification.
   */
  object Defaults {
    /* Against my better judgment, giving in to martin here and allowing
     * CLASSPATH as the default if no -cp is given.  Only if there is no
     * command line option or environment variable is "." used.
     */
    def scalaUserClassPath  = firstNonEmpty(Environment.classPathEnv, ".")
    def scalaSourcePath     = Environment.sourcePathEnv

    def javaBootClassPath = Environment.javaBootClassPath
    def javaUserClassPath = Environment.javaUserClassPath
    def javaExtDirs       = Environment.javaExtDirs
    def useJavaClassPath  = Environment.useJavaClassPath

    def scalaHome         = Environment.scalaHome
    def scalaHomeDir      = Directory(scalaHome)
    def scalaHomeExists   = scalaHomeDir.isDirectory
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

    def scalaBootClassPath  = scalaLibDirFound match {
      case Some(dir) if scalaHomeExists => join(ClassPath expandDir dir.path: _*)
      case _                            => ""
    }

    def scalaExtDirs        = Environment.scalaExtDirs
    def scalaPluginDirs     = List("misc", "scala-devel", "plugins")
    def scalaPluginPath     = join(scalaPluginDirs map (scalaHomeDir / _ path): _*)

    override def toString = """
      |object Defaults {
      |  scalaHome            = %s
      |  javaBootClassPath    = %s
      |  scalaLibDirFound     = %s
      |  scalaLibFound        = %s
      |  scalaBootClassPath   = %s
      |  scalaPluginPath      = %s
      |}""".trim.stripMargin.format(
        scalaHome,
        ppcp(javaBootClassPath),
        scalaLibDirFound, scalaLibFound,
        ppcp(scalaBootClassPath), ppcp(scalaPluginPath)
      )
  }

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
      val rest = settings.processArguments(args.toList, false)._2
      val pr = new PathResolver(settings)
      println(" COMMAND: 'scala %s'".format(args.mkString(" ")))
      println("RESIDUAL: 'scala %s'\n".format(rest.mkString(" ")))
      pr.result.show
    }
  }
}
import PathResolver.{ Defaults, Environment, firstNonEmpty, ppcp }

class PathResolver(settings: Settings, context: JavaContext) {
  def this(settings: Settings) = this(settings, if (settings.inline.value) new JavaContext else DefaultJavaContext)

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
    def javaBootClassPath   = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def javaExtDirs         = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def javaUserClassPath   = if (useJavaClassPath) Defaults.javaUserClassPath else ""
    def scalaBootClassPath  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def scalaExtDirs        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    def userClassPath       = cmdLineOrElse("classpath", Defaults.scalaUserClassPath)
    def sourcePath          = cmdLineOrElse("sourcepath", Defaults.scalaSourcePath)

    import context._

    // Assemble the elements!
    def basis = List(
      classesInPath(javaBootClassPath),             // 1. The Java bootstrap class path.
      contentsOfDirsInPath(javaExtDirs),            // 2. The Java extension class path.
      classesInExpandedPath(javaUserClassPath),     // 3. The Java application class path.
      classesInPath(scalaBootClassPath),            // 4. The Scala boot class path.
      contentsOfDirsInPath(scalaExtDirs),           // 5. The Scala extension class path.
      classesInExpandedPath(userClassPath),         // 6. The Scala application class path.
      sourcesInPath(sourcePath)                     // 7. The Scala source path.
    )

    lazy val containers = basis.flatten.distinct

    override def toString = """
      |object Calculated {
      |  scalaHome            = %s
      |  javaBootClassPath    = %s
      |  javaExtDirs          = %s
      |  javaUserClassPath    = %s
      |    useJavaClassPath   = %s
      |  scalaBootClassPath   = %s
      |  scalaExtDirs         = %s
      |  userClassPath        = %s
      |  sourcePath           = %s
      |}""".trim.stripMargin.format(
        scalaHome,
        ppcp(javaBootClassPath), ppcp(javaExtDirs), ppcp(javaUserClassPath),
        useJavaClassPath,
        ppcp(scalaBootClassPath), ppcp(scalaExtDirs), ppcp(userClassPath),
        ppcp(sourcePath)
      )
  }

  def containers = Calculated.containers

  lazy val result = {
    val cp = new JavaClassPath(containers, context)
    if (settings.Ylogcp.value) {
      Console.println("Classpath built from " + settings.toConciseString)
      Console.println("Defaults: " + PathResolver.Defaults)

      val xs = (Calculated.basis drop 2).flatten.distinct
      println("After java boot/extdirs classpath has %d entries:" format xs.size)
      xs foreach (x => println("  " + x))
    }
    cp
  }

  def asURLs = result.asURLs
}
