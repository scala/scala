/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools

import java.nio.file.Files
import java.util.concurrent.{Callable, ExecutorService}

import scala.concurrent.duration.Duration
import scala.io.Codec
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions
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
  val EOL                         = System.lineSeparator()
  def onull(s: String)            = if (s == null) "" else s
  def oempty(xs: String*)         = xs filterNot (x => x == null || x == "")
  def ojoin(xs: String*): String  = oempty(xs: _*) mkString space
  def nljoin(xs: String*): String = oempty(xs: _*) mkString EOL

  implicit val codec: Codec = Codec.UTF8

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
  private object IntOf {
    def unapply(ds: String): Some[Int] = Some {
      try ds.toInt
      catch { case _: NumberFormatException => -1 }
    }
  }

  implicit class `special string ops`(private val s: String) extends AnyVal {
    def linesIfNonEmpty: Iterator[String] = if (!s.isEmpty) s.linesIterator else Iterator.empty
  }

  implicit class FileOps(val f: File) {
    private def sf = SFile(f)

    // e.g. pos/t1234.scala
    def withEnclosing: String = f.toPath.iterator.asScala.toList.takeRight(2).mkString("/")
    def testIdent = withEnclosing

    def mapInPlace(mapFn: String => String)(filterFn: String => Boolean = _ => true): Unit =
      writeAll(fileLines filter filterFn map (x => mapFn(x) + EOL): _*)

    def appendAll(strings: String*): Unit = sf.appendAll(strings: _*)
    def writeAll(strings: String*): Unit = sf.writeAll(strings: _*)
    def absolutePathSegments: List[String] = f.getAbsolutePath.split("""[/\\]+""").toList

    def isJava        = f.isFile && (sf hasExtension "java")
    def isScala       = f.isFile && (sf hasExtension "scala")
    def isJavaOrScala = isJava || isScala

    def extension = sf.extension
    def hasExtension(ext: String) = sf hasExtension ext
    def changeExtension(ext: String): File = (sf changeExtension ext).jfile

    /** The group number for this source file, or -1 for no group or out of range. */
    def group: Int =
      sf.stripExtension match {
        case GroupPattern(IntOf(g)) => g
        case _                      => -1
      }

    // Files.readString on jdk 11
    def fileContents: String    = if (Files.isReadable(f.toPath)) try sf.slurp() catch { case _: java.io.FileNotFoundException => "" } else ""
    def fileLines: List[String] = if (Files.isReadable(f.toPath)) Files.readAllLines(f.toPath).asScala.toList else Nil
  }

  implicit class PathOps(p: Path) extends FileOps(p.jfile)

  implicit class Copier(val f: SFile) extends AnyVal {
    def copyTo(dest: Path): Unit = dest.toFile writeAll f.slurp(Codec.UTF8)
  }

  implicit class LoaderOps(val loader: ClassLoader) extends AnyVal {
    import scala.util.control.Exception.catching
    /** Like ScalaClassLoader.create for the case where the result type is
     *  available to the current class loader, implying that the current
     *  loader is a parent of `loader`.
     */
    def instantiate[A >: Null](name: String): A = (
      catching(classOf[ClassNotFoundException], classOf[SecurityException]) opt
      loader.loadClass(name).getConstructor().newInstance().asInstanceOf[A]
    ).orNull
  }

  implicit class ExecutorOps(val executor: ExecutorService) {
    def awaitTermination[A](wait: Duration)(failing: => A = ()): Option[A] =
      if (executor.awaitTermination(wait.length, wait.unit)) None
      else Some(failing)
  }

  implicit def temporaryPath2File(x: Path): File = x.jfile
  implicit def stringPathToJavaFile(path: String): File = new File(path)

  def fileSeparator = java.io.File.separator
  def pathSeparator = java.io.File.pathSeparator

  def timed[T](body: => T): (T, Long) = {
    val t1 = System.currentTimeMillis
    val result = body
    val t2 = System.currentTimeMillis

    (result, t2 - t1)
  }

  def callable[T](body: => T): Callable[T] = new Callable[T] { override def call() = body }

  def basename(name: String): String = Path(name).stripExtension

  def findProgram(name: String): Option[File] = {
    val pathDirs = sys.env("PATH") match {
      case null => List("/usr/local/bin", "/usr/bin", "/bin")
      case path => path.split("[:;]").filterNot(_ == "").toList
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
    val javaVmArguments =
      java.lang.management.ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList
    javaVmArguments.mkString(
      "Java VM started with arguments: '",
      " ",
      "'"
    )
  }

  def allPropertiesString = {
    System.getProperties.asScala.toList.sorted map { case (k, v) => "%s -> %s\n".format(k, v) } mkString ""
  }

  def ifJavaAtLeast[A](version: String)(yesRun: => A) = new TestUnderJavaAtLeast(version, { yesRun })

  /** Debugger interest only below this line **/
  def isDebug                = sys.props.contains("partest.debug") || sys.env.contains("PARTEST_DEBUG")
  def debugSettings          = sys.props.getOrElse("partest.debug.settings", "")
  def log(msg: => Any): Unit = if (isDebug) Console.err.println(msg)
}
