/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import java.net.{ URL, MalformedURLException }
import nsc.{ Settings, GenericRunnerSettings }
import nsc.util.{ ClassPath, JavaClassPath, ScalaClassLoader }
import nsc.io.{ File, Directory, Path }
import ClassPath.{ JavaContext, DefaultJavaContext, join, split }
import PartialFunction.condOpt

// Mostly based on the specification at:
// https://lampsvn.epfl.ch/trac/scala/wiki/Classpath
//

object PathResolver {
  // val debugLogger = {
  //   val f = File("/tmp/path-resolve-log.txt")
  //   if (f.exists) f.truncate()
  //   else f.createFile()
  //
  //   val res = f.bufferedWriter()
  //   res write ("Started debug log: %s\n".format(new java.util.Date))
  //   res
  // }
  // def log(msg: Any) = {
  //   Console println msg
  //   debugLogger.write(msg.toString + "\n")
  //   debugLogger flush
  // }

  private def propOrElse(name: String, alt: String) = System.getProperty(name, alt)
  private def envOrElse(name: String, alt: String)  = Option(System getenv name) getOrElse alt
  private def firstNonEmpty(xs: String*)            = xs find (_ != "") getOrElse ""

  private def fileOpt(f: Path): Option[String]      = f ifFile (_.path)
  private def dirOpt(d: Path): Option[String]       = d ifDirectory (_.path)
  private def expandToPath(p: Path)                 = join(ClassPath.expandPath(p.path, true))
  private def expandToContents(p: Path)             = join(ClassPath.expandDir(p.path))

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

    def classPathEnv        =  envOrElse("CLASSPATH", "")
    def sourcePathEnv       =  envOrElse("SOURCEPATH", "")        // not used
    def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
    def javaUserClassPath   = propOrElse("java.class.path", "")
    def javaExtDirs         = propOrElse("java.ext.dirs", "")
    def userHome            = propOrElse("user.home", "")
    def scalaHome           = propOrElse("scala.home", "")
    def scalaExtDirs        = propOrElse("scala.ext.dirs", "")

    override def toString = """
      |object Environment {
      |  javaBootClassPath  = %s
      |  javaUserClassPath  = %s
      |  javaExtDirs        = %s
      |  userHome           = %s
      |  scalaHome          = %s
      |  scalaExtDirs       = %s
      |}""".trim.stripMargin.format(
        ppcp(javaBootClassPath), ppcp(javaUserClassPath), ppcp(javaExtDirs),
        userHome, scalaHome, ppcp(scalaExtDirs)
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

    // XXX review these semantics
    def javaBootClassPath = join(Seq(Environment.javaBootClassPath, Environment.javaUserClassPath))   // ... ignoring Environment.classPathEnv
    def javaExtDirs       = Environment.javaExtDirs

    def scalaHome         = firstNonEmpty(Environment.scalaHome, guessedScalaHome)
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

    def scalaBootClassPath = scalaLibDirFound match {
      case Some(dir)    => join(ClassPath expandDir dir.path)
      case _            => ""
    }

    def scalaExtDirs        = Environment.scalaExtDirs
    def scalaPluginDirs     = List("misc", "scala-devel", "plugins")
    def scalaPluginPath     = join(scalaPluginDirs map (scalaHomeDir / _ path))

    // The class path that a runner script uses to interpret a program is called the “execution class path”.
    // The execution class path is the concatenation of the following sub-path.
    // If a class is available in multiple locations, it must be loaded from that with the lowest number.
    def executionPath = List(
      // 1. The Java bootstrap class path.
      javaBootClassPath,
      // 2. The Java extension class path.
      javaExtDirs,
      // 3. The class path formed by all JAR and ZIP files and all folders in Scala's home lib folder.
      scalaBootClassPath
    )

    override def toString = """
      |object Defaults {
      |  javaBootClassPath    = %s
      |  scalaHome            = %s
      |  scalaLibDirFound     = %s
      |  scalaLibFound        = %s
      |  scalaBootClassPath   = %s
      |  scalaPluginPath      = %s
      |}""".trim.stripMargin.format(
        ppcp(javaBootClassPath),
        scalaHome, scalaLibDirFound, scalaLibFound,
        ppcp(scalaBootClassPath), ppcp(scalaPluginPath)
      )
  }

  def executionPath = join(Defaults.executionPath)
  def executionPathURLs = fromPathString(executionPath).asURLs

  private def classPathContainersFromSettings(settings: Settings, context: JavaContext) = {
    val pr = new PathResolver(settings)
    import context._
    import pr.Calculated._

    // XXX how should the contents of lib/* break down between bootclasspath and extdirs?
    // XXX what exactly is codebase for?
    val sources = List(
      classesInPath(javaBootClassPath),           // -javabootclasspath multiple entries, no expansion
      contentsOfDirsInPath(scalaBootClassPath),   // -bootclasspath     ???
      contentsOfDirsInPath(javaExtDirs),          // -javaextdirs       multiple dirs, each expands to contents
      contentsOfDirsInPath(scalaExtDirs),         // -extdirs           ???
      classesInExpandedPath(userClassPath),       // -classpath         multiple entries, first expanding *s
      classesAtAllURLS(codeBase),                 // -Ycodebase         ??? multiple URLs
      sourcesInPath(sourcePath)                   // -sourcepath        multiple source entries, no expansion
    )

    if (settings.Ylogcp.value)
      Console.println("PathResolver calculated classpath:\n" + pr.Calculated)

    sources.flatten
  }
  def urlsFromSettings(settings: Settings): List[URL] = urlsFromSettings(settings, DefaultJavaContext)
  def urlsFromSettings(settings: Settings, context: JavaContext): List[URL] =
    classPathContainersFromSettings(settings, context) flatMap (_.asURLs) distinct

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
  def fromPathString(path: String, context: JavaContext): JavaClassPath = {
    val s = new Settings()
    s.classpath.value = path
    fromSettings(s, context)
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
import PathResolver.{ Defaults, Environment, firstNonEmpty, ppcp }

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
    def scalaHome           = Defaults.scalaHome
    def javaBootClassPath   = cmdLineOrElse("javabootclasspath", Defaults.javaBootClassPath)
    def scalaBootClassPath  = cmdLineOrElse("bootclasspath", Defaults.scalaBootClassPath)
    def javaExtDirs         = cmdLineOrElse("javaextdirs", Defaults.javaExtDirs)
    def scalaExtDirs        = cmdLineOrElse("extdirs", Defaults.scalaExtDirs)
    def userClassPath       = cmdLineOrElse("classpath", "")
    def sourcePath          = cmdLineOrElse("sourcepath", "")
    def codeBase            = cmdLineOrElse("Ycodebase", "")
    def dotPath             = if (settings.userSuppliedClassPath == "") "." else ""

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
      userClassPath,
      dotPath
    )

    override def toString = """
      |object Calculated {
      |  scalaHome            = %s
      |  javaBootClassPath    = %s
      |  scalaBootClassPath   = %s
      |  javaExtDirs          = %s
      |  scalaExtDirs         = %s
      |  userClassPath        = %s
      |  sourcePath           = %s
      |  referencePath        = %s
      |}""".trim.stripMargin.format(
        scalaHome,
        ppcp(javaBootClassPath), ppcp(scalaBootClassPath),
        ppcp(javaExtDirs), ppcp(scalaExtDirs),
        ppcp(userClassPath), ppcp(sourcePath),
        ppcp(PathResolver.this.referencePath)
      )
  }

  def referencePath = join(Calculated.referencePath)
  def referencePathAsURLs = ClassPath toURLs referencePath
  def minimalPath = join(Seq(Calculated.scalaBootClassPath, Calculated.userClassPath))
  def minimalPathAsURLs = ClassPath toURLs minimalPath
}
