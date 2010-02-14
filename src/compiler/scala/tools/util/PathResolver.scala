/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import java.net.{ URL, MalformedURLException }
import nsc.{ Settings }
import nsc.util.{ ClassPath, JavaClassPath, ScalaClassLoader }
import nsc.io.{ File, Directory, Path }
import ClassPath.{ JavaContext, DefaultJavaContext, join, split }
import PartialFunction.condOpt

// Mostly based on the specification at:
// https://lampsvn.epfl.ch/trac/scala/wiki/Classpath
//

object PathResolver {
  private def propOrElse(name: String, alt: String) = System.getProperty(name, alt)
  private def envOrElse(name: String, alt: String)  = Option(System getenv name) getOrElse alt

  private def fileOpt(f: Path): Option[String]      = f ifFile (_.path)
  private def dirOpt(d: Path): Option[String]       = d ifDirectory (_.path)
  private def expandToPath(p: Path)                 = join(ClassPath.expandPath(p.path, true))
  private def expandToContents(p: Path)             = join(ClassPath.expandDir(p.path))

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

    def classPathEnv        =  envOrElse("CLASSPATH", "")
    def toolPathEnv         =  envOrElse("TOOL_CLASSPATH", "")    // XXX this should go
    def classPathProp       = propOrElse("java.class.path", "")
    def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
    def javaExtDirs         = propOrElse("java.ext.dirs", "")
    def userHome            = propOrElse("user.home", "")
    def scalaHome           = propOrElse("scala.home", "")
    def scalaExtDirs        = propOrElse("scala.ext.dirs", "")    // XXX not in spec

    // XXX note "." not used yet
    def classPath           = if (classPathProp != "") classPathProp else classPathEnv

    override def toString = """
      |object Environment {
      |  classPathEnv       = %s
      |  toolPathEnv        = %s
      |  classPathProp      = %s
      |  javaBootClassPath  = %s
      |  javaExtDirs        = %s
      |  userHome           = %s
      |  scalaHome          = %s
      |  scalaExtDirs       = %s
      |}""".trim.stripMargin.format(
        ppcp(classPathEnv), ppcp(toolPathEnv), ppcp(classPathProp), ppcp(javaBootClassPath),
        ppcp(javaExtDirs), userHome, scalaHome, ppcp(scalaExtDirs)
      )
  }

  /** Default values based on those in Environment as interpretered according
   *  to the path resolution specification.
   */
  object Defaults {
    private lazy val guessedScalaHome = {
      for (url <- ScalaClassLoader originOfClass classOf[ScalaObject] ; if url.getProtocol == "file") yield
        File(url.getFile).parent.path
    } getOrElse ""

    private def translateScalaHome(s: String) = s.replaceAll("""${SCALA_HOME}""", scalaHome)

    def scalaHome         = Environment.scalaHome match { case "" => guessedScalaHome ; case x => x }
    def scalaHomeDir      = Directory(scalaHome)
    def scalaLibDir       = Directory(scalaHomeDir / "lib")
    def scalaClassesDir   = Directory(scalaHomeDir / "classes")

    def scalaLibJar       = File(scalaLibDir / "scala-library.jar")
    def scalaLibClassDir  = Directory(scalaClassesDir / "library")
    def scalaLibFound: Option[Directory] =
      if (scalaLibJar.isFile) Some(scalaLibDir)
      else if (scalaLibClassDir.isDirectory) Some(scalaClassesDir)
      else None

    def scalaBootClassPath    = scalaLibFound map expandToContents getOrElse ""
    def scalaExtDirs          = Environment.scalaExtDirs
    def scalaToolPath         = Environment.toolPathEnv match {
      case ""   => scalaBootClassPath
      // XXX doubtful this is accomplishing anything
      case x    => expandToPath(x.replaceAll("""\$\{SCALA_HOME\}""", scalaHome))
    }

    def scalaPluginDirs     = List("misc", "scala-devel", "plugins")
    def scalaPluginPath     = join(scalaPluginDirs map (scalaHomeDir / _ path))

    // The class path that a runner script uses to interpret a program is called the “execution class path”.
    // The execution class path is the concatenation of the following sub-path.
    // If a class is available in multiple locations, it must be loaded from that with the lowest number.
    def executionPath = List(
      // 1. The Java bootstrap class path.
      Environment.javaBootClassPath,
      // 2. The Java extension class path.
      Environment.javaExtDirs,
      // 3. The first available path below.
      //    * The fixed class path (TOOL_CLASSPATH) set in the runner script when it was generated
      //      (which can be set as the "classpath" attribute when using the scalatool Ant task).
      //      This path may contain absolute locations, or locations relative to Scala's home by using
      //      the shell variable ${SCALA_HOME} in the path.
      //    * The class path formed by all JAR and ZIP files and all folders in Scala's home lib folder.
      scalaToolPath
    )

    override def toString = """
      |object Defaults {
      |  scalaHome            = %s
      |  scalaLibFound        = %s
      |  scalaBootClassPath   = %s
      |  scalaPluginPath      = %s
      |}""".trim.stripMargin.format(
        scalaHome, scalaLibFound, ppcp(scalaBootClassPath), ppcp(scalaPluginPath)
      )
  }

  def executionPath = join(Defaults.executionPath)

  /** The original logic of MainGenericRunner.
   */
  def genericRunnerClassPath: String = {
    // this is to make the interpreter work when running without the scala script
    // (e.g. from eclipse). Before, "java.class.path" was added to the user classpath
    // in Settings; this was changed to match the behavior of Sun's javac.  So the
    // classpath generated here consist of only the scala lib/* jars if scalaHome
    // can be found, and the whole user classpath otherwise.
    if (Environment.scalaHome == "") Environment.classPath
    else Defaults.scalaBootClassPath
  }
  private def classPathContainersFromSettings(settings: Settings, context: JavaContext) = {
    val pr = new PathResolver(settings)
    import context._
    import pr.Calculated._

    // XXX how do the contents of lib/* break down between bootclasspath and extdirs?
    // XXX and what exactly is codebase for?
    val sources = List(
      classesInPath(javaBootClassPath),           // -javabootclasspath multiple entries, no expansion
      contentsOfDirsInPath(scalaBootClassPath),   // -bootclasspath     ???
      contentsOfDirsInPath(javaExtDirs),          // -javaextdirs       multiple dirs, each expands to contents
      contentsOfDirsInPath(scalaExtDirs),         // -extdirs           ???
      classesInExpandedPath(classPath),           // -classpath         multiple entries, first expanding *s
      classesAtAllURLS(codeBase),                 // -Ycodebase         ??? multiple URLs
      sourcesInPath(sourcePath)                   // -sourcepath        multiple source entries, no expansion
    )

    if (settings.Ylogcp.value)
      Console.println("PathResolver calculated classpath:\n" + pr.Calculated)

    sources.flatten
  }
  def urlsFromSettings(settings: Settings): List[URL] = urlsFromSettings(settings, DefaultJavaContext)
  def urlsFromSettings(settings: Settings, context: JavaContext): List[URL] =
    classPathContainersFromSettings(settings, context) flatMap (_.asURLs)

  private def contextFromSettings(s: Settings) =
    if (s.inline.value) new JavaContext else DefaultJavaContext

  def fromArgumentString(argString: String): JavaClassPath =
    fromArgumentList(splitParams(argString, _ => ()))

  def fromArgumentList(args: List[String]): JavaClassPath = {
    val settings = new Settings()
    settings.processArguments(args, false)
    fromSettings(settings, contextFromSettings(settings))
  }

  def fromSettings(settings: Settings): JavaClassPath =
    fromSettings(settings, contextFromSettings(settings))

  def fromSettings(settings: Settings, context: JavaContext): JavaClassPath = {
    val containers = classPathContainersFromSettings(settings, context)
    new JavaClassPath(containers, context)
  }

  def fromPathString(path: String): JavaClassPath = fromPathString(path, DefaultJavaContext)
  def fromPathString(path: String, context: JavaContext): JavaClassPath =
    new JavaClassPath(context.classesInExpandedPath(path), context)

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
      println(pr.Calculated)
    }
  }

  /**
   * Split command line parameters by space, properly process quoted parameter
   */
  def splitParams(line: String, errorFn: String => Unit): List[String] = {
    def parse(from: Int, i: Int, args: List[String]): List[String] = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' =>
            val args1 = fetchArg(from, i) :: args
            val j = skipS(i + 1)
            if (j >= 0) {
              parse(j, j, args1)
            } else args1
          case '"' =>
            val j = skipTillQuote(i + 1)
            if (j > 0) {
              parse(from, j + 1, args)
            } else {
              errorFn("Parameters '" + line + "' with unmatched quote at " + i + ".")
              Nil
            }
          case _ => parse(from, i + 1, args)
        }
      } else { // done
        if (i > from) {
          fetchArg(from, i) :: args
        } else args
      }
    }

    def fetchArg(from: Int, until: Int) = {
      if (line.charAt(from) == '"') {
        line.substring(from + 1, until - 1)
      } else {
        line.substring(from, until)
      }
    }

    def skipTillQuote(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case '"' => i
          case _ => skipTillQuote(i + 1)
        }
      } else -1
    }

    def skipS(i: Int): Int = {
      if (i < line.length) {
        line.charAt(i) match {
          case ' ' => skipS(i + 1)
          case _ => i
        }
      } else -1
    }

    // begin split
    val j = skipS(0)
    if (j >= 0) {
      parse(j, j, Nil).reverse
    } else Nil
  }
}
import PathResolver.{ Defaults, Environment, ppcp }

class PathResolver(settings: Settings) {
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
    case "Ycodebase"          => settings.Ycodebase.value
  }

  /** Calculated values based on any given command line options, falling back on
   *  those in Defaults.
   */
  object Calculated {
    def javaBootClassPath   = cmdLineOrElse("javabootclasspath", Environment.javaBootClassPath)
    def scalaBootClassPath  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def javaExtDirs         = cmdLineOrElse("javaextdirs", Environment.javaExtDirs)
    def scalaExtDirs        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    def classPath           = cmdLineOrElse("classpath", Environment.classPath)
    def sourcePath          = cmdLineOrElse("sourcepath", "")
    def codeBase            = cmdLineOrElse("Ycodebase", "")

    def referencePath = List(
      // 1. The value of -javabootclasspath if it is set, or the Java bootstrap class path.
      javaBootClassPath,
      // 2. The value of -bootclasspath if it is set,
      //    or the lib/scala-library.jar file of Scala's home if it is available,
      //    or the classes/library folder of Scala's home if it is available.
      scalaBootClassPath,
      // 3. All JAR and ZIP files present in any folder listed by the value of -javaextdirs, if it is set,
      //    or the Java extension class path.
      javaExtDirs,
      // 4. All JAR and ZIP files present in any folder listed by the value of -extdirs, if it is set.
      scalaExtDirs,
      // 5. The first available path below.
      //    * The value of -classpath or -cp.
      //    *  ---> XXX what about java.class.path?
      //    * The value of the CLASSPATH environment variable.
      //    * The current directory (that is the location of ".").
      if (classPath == "") "." else classPath
    )

    override def toString = """
      |object Calculated {
      |  javaBootClassPath    = %s
      |  scalaBootClassPath   = %s
      |  javaExtDirs          = %s
      |  scalaExtDirs         = %s
      |  classPath            = %s
      |  sourcePath           = %s
      |  codeBase             = %s
      |}""".trim.stripMargin.format(
        ppcp(javaBootClassPath), ppcp(scalaBootClassPath),
        ppcp(javaExtDirs), ppcp(scalaExtDirs),
        ppcp(classPath), ppcp(sourcePath), codeBase
      )
  }

  def referencePath = join(Calculated.referencePath)
}
