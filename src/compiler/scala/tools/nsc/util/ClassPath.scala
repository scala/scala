/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package util

import io.{AbstractFile, Directory, File, Jar}
import java.net.MalformedURLException
import java.net.URL
import java.util.regex.PatternSyntaxException

import File.pathSeparator
import Jar.isJarOrZip

/**
  * A representation of the compiler's class- or sourcepath.
  */
trait ClassPath {
  import scala.tools.nsc.classpath._
  def asURLs: Seq[URL]

  /** Empty string represents root package */
  private[nsc] def packages(inPackage: String): Seq[PackageEntry]
  private[nsc] def classes(inPackage: String): Seq[ClassFileEntry]
  private[nsc] def sources(inPackage: String): Seq[SourceFileEntry]

  /** Allows to get entries for packages and classes merged with sources possibly in one pass. */
  private[nsc] def list(inPackage: String): ClassPathEntries

  /**
    * It returns both classes from class file and source files (as our base ClassRepresentation).
    * So note that it's not so strictly related to findClassFile.
    */
  def findClass(className: String): Option[ClassRepresentation] = {
    // A default implementation which should be overridden, if we can create the more efficient
    // solution for a given type of ClassPath
    val (pkg, simpleClassName) = PackageNameUtils.separatePkgAndClassNames(className)

    val foundClassFromClassFiles = classes(pkg).find(_.name == simpleClassName)
    def findClassInSources = sources(pkg).find(_.name == simpleClassName)

    foundClassFromClassFiles orElse findClassInSources
  }
  def findClassFile(className: String): Option[AbstractFile]

  def asClassPathStrings: Seq[String]

  /** The whole classpath in the form of one String.
    */
  def asClassPathString: String = ClassPath.join(asClassPathStrings: _*)
  // for compatibility purposes
  @deprecated("use asClassPathString instead of this one", "2.11.5")
  def asClasspathString: String = asClassPathString

  /** The whole sourcepath in the form of one String.
    */
  def asSourcePathString: String
}

object ClassPath {
  val RootPackage = ""

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /* Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list.filter(x => filt(x.name) && (x.isDirectory || isJarOrZip(x))).map(_.path).toList

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      try {
        val regexp = ("^" + pattern.replaceAllLiterally("""\*""", """.*""") + "$").r
        lsDir(Directory(pattern).parent, regexp.findFirstIn(_).isDefined)
      }
      catch { case _: PatternSyntaxException => List(pattern) }
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList.filterNot(_ == "").distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir.filter(_.isClassContainer).map(x => new java.io.File(dir.file, x.name).getPath).toList
    }
  }

  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

<<<<<<< HEAD
  def manifests: List[java.net.URL] = {
    import scala.collection.JavaConverters._
    val resources = Thread.currentThread().getContextClassLoader().getResources("META-INF/MANIFEST.MF")
    resources.asScala.filter(_.getProtocol == "jar").toList
=======
  /** A class modeling aspects of a ClassPath which should be
   *  propagated to any classpaths it creates.
   */
  abstract class ClassPathContext[T] {
    /** A filter which can be used to exclude entities from the classpath
     *  based on their name.
     */
    def isValidName(name: String): Boolean = true

    /** From the representation to its identifier.
     */
    def toBinaryName(rep: T): String

    /** Create a new classpath based on the abstract file.
     */
    def newClassPath(file: AbstractFile): ClassPath[T]

    /** Creators for sub classpaths which preserve this context.
     */
    def sourcesInPath(path: String): List[ClassPath[T]] =
      for (file <- expandPath(path, false) ; dir <- Option(AbstractFile getDirectory file)) yield
        new SourcePath[T](dir, this)

    def contentsOfDirsInPath(path: String): List[ClassPath[T]] =
      for (dir <- expandPath(path, false) ; name <- expandDir(dir) ; entry <- Option(AbstractFile getDirectory name)) yield
        newClassPath(entry)

    def classesAtAllURLS(path: String): List[ClassPath[T]] =
      (path split " ").toList flatMap classesAtURL

    def classesAtURL(spec: String) =
      for (url <- specToURL(spec).toList ; location <- Option(AbstractFile getURL url)) yield
        newClassPath(location)

    def classesInExpandedPath(path: String): IndexedSeq[ClassPath[T]] =
      classesInPathImpl(path, true).toIndexedSeq

    def classesInPath(path: String) = classesInPathImpl(path, false)

    // Internal
    private def classesInPathImpl(path: String, expand: Boolean) =
      for (file <- expandPath(path, expand) ; dir <- Option(AbstractFile getDirectory file)) yield
        newClassPath(dir)
  }

  class JavaContext extends ClassPathContext[AbstractFile] {
    def toBinaryName(rep: AbstractFile) = {
      val name = rep.name
      assert(endsClass(name), name)
      name.substring(0, name.length - 6)
    }
    def newClassPath(dir: AbstractFile) = new DirectoryClassPath(dir, this)
  }

  object DefaultJavaContext extends JavaContext {
    override def isValidName(name: String) = !isTraitImplementation(name) //TR: imho this doesn't really make sense but is unsafe to turn off (#4784)
>>>>>>> virt
  }

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class ClassPathContext

  @deprecated("shim for sbt's compiler interface", since = "2.12.0")
  sealed abstract class JavaContext
}

trait ClassRepresentation {
  def name: String
  def binary: Option[AbstractFile]
  def source: Option[AbstractFile]
}

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class DirectoryClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class MergedClassPath

@deprecated("shim for sbt's compiler interface", since = "2.12.0")
sealed abstract class JavaClassPath
