/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.tools

import java.util.concurrent.{ Callable, ExecutorService }
import scala.concurrent.duration.Duration
import scala.tools.nsc.util.Exceptional

package object partest {
  type File         = java.io.File
  type SFile        = scala.reflect.io.File
  type Directory    = scala.reflect.io.Directory
  type Path         = scala.reflect.io.Path
  type PathResolver = scala.tools.util.PathResolver
  type StringWriter = java.io.StringWriter

  val SFile        = scala.reflect.io.File
  val Directory    = scala.reflect.io.Directory
  val Path         = scala.reflect.io.Path
  val PathResolver = scala.tools.util.PathResolver
  val ClassPath    = scala.tools.nsc.util.ClassPath

  val space                       = "\u0020"
  val EOL                         = scala.compat.Platform.EOL
  def onull(s: String)            = if (s == null) "" else s
  def oempty(xs: String*)         = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String  = oempty(xs: _*) mkString space
  def nljoin(xs: String*): String = oempty(xs: _*) mkString EOL

  implicit val codec = scala.io.Codec.UTF8

  def setUncaughtHandler() = {
    Thread.setDefaultUncaughtExceptionHandler(
      new Thread.UncaughtExceptionHandler {
        def uncaughtException(thread: Thread, t: Throwable): Unit = {
          val t1 = Exceptional.rootCause(t)
          System.err.println(s"Uncaught exception on thread $thread: $t1")
          t1.printStackTrace()
        }
      }
    )
  }

  /** Sources have a numerical group, specified by name_7 and so on. */
  private val GroupPattern = """.*_(\d+)""".r

  implicit class `special string ops`(private val s: String) extends AnyVal {
    def linesIfNonEmpty: Iterator[String] = if (!s.isEmpty) s.linesIterator else Iterator.empty
  }

  implicit class FileOps(val f: File) {
    import scala.collection.JavaConverters._

    private def sf = SFile(f)

    // e.g. pos/t1234
    def withEnclosing: String = f.toPath.iterator.asScala.toList.takeRight(2).mkString("/")
    def testIdent = withEnclosing

    def mapInPlace(mapFn: String => String)(filterFn: String => Boolean = _ => true): Unit =
      writeAll(fileLines filter filterFn map (x => mapFn(x) + EOL): _*)

    def appendAll(strings: String*): Unit = sf.appendAll(strings: _*)
    def writeAll(strings: String*): Unit = sf.writeAll(strings: _*)
    def absolutePathSegments: List[String] = f.getAbsolutePath split """[/\\]+""" toList

    def isJava        = f.isFile && (sf hasExtension "java")
    def isScala       = f.isFile && (sf hasExtension "scala")
    def isJavaOrScala = isJava || isScala

    def extension = sf.extension
    def hasExtension(ext: String) = sf hasExtension ext
    def changeExtension(ext: String): File = (sf changeExtension ext).jfile

    /** The group number for this source file, or -1 for no group. */
    def group: Int =
      sf.stripExtension match {
        case GroupPattern(g) if g.toInt >= 0 => g.toInt
        case _                               => -1
      }

    def fileContents: String    = try sf.slurp() catch { case _: java.io.FileNotFoundException => "" }
    def fileLines: List[String] = fileContents.linesIfNonEmpty.toList
  }

  implicit class PathOps(p: Path) extends FileOps(p.jfile)

  implicit class Copier(val f: SFile) extends AnyVal {
    def copyTo(dest: Path): Unit = dest.toFile writeAll f.slurp(scala.io.Codec.UTF8)
  }

  implicit class LoaderOps(val loader: ClassLoader) extends AnyVal {
    import scala.util.control.Exception.catching
    /** Like ScalaClassLoader.create for the case where the result type is
     *  available to the current class loader, implying that the current
     *  loader is a parent of `loader`.
     */
    def instantiate[A >: Null](name: String): A = (
      catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      (loader loadClass name).getConstructor().newInstance().asInstanceOf[A] orNull
    )
  }

  implicit class ExecutorOps(val executor: ExecutorService) {
    def awaitTermination[A](wait: Duration)(failing: => A = ()): Option[A] = (
      if (executor awaitTermination (wait.length, wait.unit)) None
      else Some(failing)
    )
  }

  implicit def temporaryPath2File(x: Path): File = x.jfile
  implicit def stringPathToJavaFile(path: String): File = new File(path)

  implicit lazy val implicitConversions = scala.language.implicitConversions

  def fileSeparator = java.io.File.separator
  def pathSeparator = java.io.File.pathSeparator

  def words(s: String): List[String] = (s.trim split "\\s+").toList

  def timed[T](body: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val result = body
    val t2 = System.currentTimeMillis

    (result, t2 - t1)
  }

  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }

  def file2String(f: File): String = f.fileContents

  def basename(name: String): String = Path(name).stripExtension

  /** In order to allow for spaces in flags/options, this
   *  parses .flags, .javaopts, javacopts etc files as follows:
   *  If it is exactly one line, it is split (naively) on spaces.
   *  If it contains more than one line, each line is its own
   *  token, spaces and all.
   */
  def readOptionsFile(file: File): List[String] = {
    file.fileLines match {
      case x :: Nil   => words(x)
      case xs         => xs
    }
  }

  def findProgram(name: String): Option[File] = {
    val pathDirs = sys.env("PATH") match {
      case null => List("/usr/local/bin", "/usr/bin", "/bin")
      case path => path split "[:;]" filterNot (_ == "") toList
    }
    pathDirs.iterator map (d => new File(d, name)) find (_.canExecute)
  }

  def now = (new java.util.Date).toString
  def elapsedString(millis: Long): String = {
    val elapsedSecs = millis/1000
    val elapsedMins = elapsedSecs/60
    val elapsedHrs  = elapsedMins/60
    val dispMins    = elapsedMins - elapsedHrs  * 60
    val dispSecs    = elapsedSecs - elapsedMins * 60

    "%02d:%02d:%02d".format(elapsedHrs, dispMins, dispSecs)
  }

  def vmArgString = {
    import scala.collection.JavaConverters._
    val javaVmArguments =
      java.lang.management.ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList
    javaVmArguments.mkString(
      "Java VM started with arguments: '",
      " ",
      "'"
    )
  }

  def allPropertiesString = {
    import scala.collection.JavaConverters._
    System.getProperties.asScala.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString ""
  }
}
