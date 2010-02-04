/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

import java.io.File
import java.net.URL
import java.util.StringTokenizer
import scala.util.Sorting

import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashSet => MutHashSet}
import scala.tools.nsc.io.AbstractFile

/** <p>
 *    This module provides star expansion of '-classpath' option arguments, behaves the same as
 *    java, see [http://java.sun.com/javase/6/docs/technotes/tools/windows/classpath.html]
 *  </p>
 *
 *  @author Stepan Koltsov
 */
object ClassPath {
  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    def isJar(name: String) = name.toLowerCase endsWith ".jar"

    /** Get all jars in directory */
    def lsJars(f: File, filt: String => Boolean = _ => true) = {
      val list = f.listFiles()
      if (list eq null) Nil
      else list.filter(f => f.isFile() && filt(f.getName) && isJar(f.getName())).map(_.getPath()).toList
    }

    val suffix = File.separator + "*"

    def basedir(s: String) =
      if (s contains File.separator) s.substring(0, s.lastIndexOf(File.separator))
      else "."

    if (pattern == "*") lsJars(new File("."))
    else if (pattern endsWith suffix) lsJars(new File(pattern dropRight 2))
    else if (pattern contains '*') {
      val regexp = ("^%s$" format pattern.replaceAll("""\*""", """.*""")).r
      lsJars(new File(basedir(pattern)), regexp findFirstIn _ isDefined)
    }
    else List(pattern)
  }

  /** Split path using platform-dependent path separator */
  private def splitPath(path: String): List[String] =
    path split File.pathSeparator toList

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) splitPath(path).flatMap(expandS(_))
    else splitPath(path)

  /** A useful name filter. */
  val noTraitImplFilter: String => Boolean = name => !(name endsWith "$class.class")

  import java.net.MalformedURLException
  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }
}

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPath[T] {
  type AnyClassRep = ClassPath[T]#ClassRep
  /**
   * The short name of the package (without prefix)
   */
  def name: String
  val classes: List[AnyClassRep]
  val packages: List[ClassPath[T]]
  val sourcepaths: List[AbstractFile]
  protected def nameOfBinaryRepresentation(binary: T): String = binary match {
    case f: AbstractFile  =>
      assert(f.name endsWith ".class", f.name)
      f.name dropRight 6
    case _                =>
      throw new FatalError("Unexpected binary class representation: " + binary)
  }

  /**
   * Represents classes which can be loaded with a ClassfileLoader/MSILTypeLoader
   * and / or a SourcefileLoader.
   */
  case class ClassRep(binary: Option[T], source: Option[AbstractFile]) {
    def name: String = binary match {
      case Some(x)  => nameOfBinaryRepresentation(x)
      case _        =>
        assert(source.isDefined)
        val nme = source.get.name
        if (nme.endsWith(".scala"))
          nme dropRight 6
        else if (nme.endsWith(".java"))
          nme dropRight 5
        else
          throw new FatalError("Unexpected source file ending: " + nme)
    }
  }

  /** A filter which can be used to exclude entities from the classpath
   *  based on their name.
   */
  def validName: String => Boolean

  /** Filters for assessing validity of various entities.
   */
  def validClassFile(name: String)  = (name endsWith ".class") && validName(name)
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.head != '.')
  def validSourceFile(name: String) = validSourceExtensions exists (name endsWith _)
  def validSourceExtensions         = List(".scala", ".java")

  /** Utility */
  protected final def dropExtension(name: String) = name take (name.lastIndexOf('.') - 1)

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def findClass(name: String): Option[AnyClassRep] = {
    val i = name.indexOf('.')
    if (i < 0) {
      classes.find(_.name == name)
    } else {
      val pkg = name take i
      val rest = name drop (i + 1)
      (packages find (_.name == pkg) flatMap (_ findClass rest)) map {
        case x: ClassRep  => x
        case x            => throw new FatalError("Unexpected ClassRep '%s' found searching for name '%s'".format(x, name))
      }
    }
  }
  def findAbstractFile(name: String): Option[AbstractFile] = {
    findClass(name) match {
      case Some(ClassRep(Some(x: AbstractFile), _)) => Some(x)
      case _                                        => None
    }
  }
}

/**
 * A Classpath containing source files
 */
class SourcePath[T](dir: AbstractFile, val validName: String => Boolean) extends ClassPath[T] {
  def name = dir.name

  lazy val classes: List[ClassRep] = dir partialMap {
    case f if !f.isDirectory && validSourceFile(f.name) => ClassRep(None, Some(f))
  } toList

  lazy val packages: List[SourcePath[T]] = dir partialMap {
    case f if f.isDirectory && validPackage(f.name) => new SourcePath[T](f, validName)
  } toList

  val sourcepaths: List[AbstractFile] = List(dir)

  override def toString() = "sourcepath: "+ dir.toString()
}

/**
 * A directory (or a .jar file) containing classfiles and packages
 */
class DirectoryClassPath(dir: AbstractFile, val validName: String => Boolean) extends ClassPath[AbstractFile] {
  def name = dir.name

  lazy val classes: List[ClassRep] = dir partialMap {
    case f if !f.isDirectory && validClassFile(f.name) => ClassRep(Some(f), None)
  } toList

  lazy val packages: List[DirectoryClassPath] = dir partialMap {
    case f if f.isDirectory && validPackage(f.name) => new DirectoryClassPath(f, validName)
  } toList

  val sourcepaths: List[AbstractFile] = Nil

  override def toString() = "directory classpath: "+ dir.toString()
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
abstract class MergedClassPath[T] extends ClassPath[T] {
  outer =>

  protected val entries: List[ClassPath[T]]

  def name = entries.head.name

  lazy val classes: List[AnyClassRep] = {
    val cls = new ListBuffer[AnyClassRep]
    for (e <- entries; c <- e.classes) {
      val name = c.name
      val idx = cls.indexWhere(cl => cl.name == name)
      if (idx >= 0) {
        val existing = cls(idx)
        if (existing.binary.isEmpty && c.binary.isDefined)
          cls(idx) = existing.copy(binary = c.binary)
        if (existing.source.isEmpty && c.source.isDefined)
          cls(idx) = existing.copy(source = c.source)
      } else {
        cls += c
      }
    }
    cls.toList
  }

  lazy val packages: List[ClassPath[T]] = {
    val pkg = new ListBuffer[ClassPath[T]]
    for (e <- entries; p <- e.packages) {
      val name = p.name
      val idx = pkg.indexWhere(pk => pk.name == name)
      if (idx >= 0) {
        pkg(idx) = addPackage(pkg(idx), p)
      } else {
        pkg += p
      }
    }
    pkg.toList
  }

  lazy val sourcepaths: List[AbstractFile] = entries.flatMap(_.sourcepaths)

  private def addPackage(to: ClassPath[T], pkg: ClassPath[T]) = to match {
    case cp: MergedClassPath[_] =>
      newMergedClassPath(cp.entries ::: List(pkg))
    case _ =>
      newMergedClassPath(List(to, pkg))
  }

  private def newMergedClassPath(entrs: List[ClassPath[T]]): MergedClassPath[T] =
    new MergedClassPath[T] {
      protected val entries = entrs
      override def validName = outer.validName
    }

  override def toString() = "merged classpath "+ entries.mkString("(", "\n", ")")
}

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(
  boot: String,
  ext: String,
  user: String,
  source: String,
  Xcodebase: String,
  val validName: String => Boolean = ClassPath.noTraitImplFilter
) extends MergedClassPath[AbstractFile] {

  protected val entries: List[ClassPath[AbstractFile]] = assembleEntries()

  private def assembleEntries(): List[ClassPath[AbstractFile]] = {
    import ClassPath._
    val etr = new ListBuffer[ClassPath[AbstractFile]]

    def addFilesInPath(path: String, expand: Boolean,
          ctr: AbstractFile => ClassPath[AbstractFile] = x => new DirectoryClassPath(x, validName)) {
      for (fileName <- expandPath(path, expandStar = expand)) {
        val file = AbstractFile.getDirectory(fileName)
        if (file ne null) etr += ctr(file)
      }
    }

    // 1. Boot classpath
    addFilesInPath(boot, false)

    // 2. Ext classpath
    for (fileName <- expandPath(ext, expandStar = false)) {
      val dir = AbstractFile.getDirectory(fileName)
      if (dir ne null) {
        for (file <- dir) {
          val name = file.name.toLowerCase
          if (name.endsWith(".jar") || name.endsWith(".zip") || file.isDirectory) {
            val archive = AbstractFile.getDirectory(new File(dir.file, name))
            if (archive ne null) etr += new DirectoryClassPath(archive, validName)
          }
        }
      }
    }

    // 3. User classpath
    addFilesInPath(user, true)

    // 4. Codebase entries (URLs)
    {
      for {
        spec <- Xcodebase.trim split " "
        url <- specToURL(spec)
        archive <- Option(AbstractFile getURL url)
      } {
        etr += new DirectoryClassPath(archive, validName)
      }
    }

    // 5. Source path
    if (source != "")
      addFilesInPath(source, false, x => new SourcePath[AbstractFile](x, validName))

    etr.toList
  }
}
