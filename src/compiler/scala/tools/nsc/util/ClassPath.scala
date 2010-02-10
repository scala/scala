/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package util

import java.io.{ File => JFile }
import java.net.URL

import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashSet => MutHashSet}
import io.{ File, Directory, Path, AbstractFile }
import scala.tools.util.StringOps.splitWhere
import Path.isJarOrZip

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
    val wildSuffix = File.separator + "*"

    /** Get all jars in directory */
    def lsJars(dir: Directory, filt: String => Boolean = _ => true) =
      dir.files partialMap { case f if filt(f.name) && (f hasExtension "jar") => f.path } toList

    def basedir(s: String) =
      if (s contains File.separator) s.substring(0, s.lastIndexOf(File.separator))
      else "."

    if (pattern == "*") lsJars(Directory("."))
    else if (pattern endsWith wildSuffix) lsJars(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      val regexp = ("^%s$" format pattern.replaceAll("""\*""", """.*""")).r
      lsJars(Directory(pattern).parent, regexp findFirstIn _ isDefined)
    }
    else List(pattern)
  }

  /** Split path using platform-dependent path separator */
  private def splitPath(path: String): List[String] =
    path split File.pathSeparator toList

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) splitPath(path) flatMap expandS
    else splitPath(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    val dir = Option(AbstractFile getDirectory extdir) getOrElse (return Nil)
    dir filter (_.isClassContainer) map (dir.sfile / _.name path) toList
  }

  /** A useful name filter. */
  def isTraitImplementation(name: String) = name endsWith "$class.class"

  import java.net.MalformedURLException
  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

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

    def classesInExpandedPath(path: String) = classesInPathImpl(path, true)
    def classesInPath(path: String) = classesInPathImpl(path, false)

    // Internal
    private def classesInPathImpl(path: String, expand: Boolean) =
      for (file <- expandPath(path, expand) ; dir <- Option(AbstractFile getDirectory file)) yield
        newClassPath(dir)
  }

  class JavaContext extends ClassPathContext[AbstractFile] {
    def toBinaryName(rep: AbstractFile) = {
      assert(rep.name endsWith ".class", rep.name)
      rep.name dropRight 6
    }
    def newClassPath(dir: AbstractFile) = new DirectoryClassPath(dir, this)
  }

  object DefaultJavaContext extends JavaContext {
    override def isValidName(name: String) = !isTraitImplementation(name)
  }

  /** From the source file to its identifier.
   */
  def toSourceName(f: AbstractFile): String = {
    val nme = f.name
    if (nme.endsWith(".scala"))
      nme dropRight 6
    else if (nme.endsWith(".java"))
      nme dropRight 5
    else
      throw new FatalError("Unexpected source file ending: " + nme)
  }
}
import ClassPath._

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPath[T] {
  type AnyClassRep = ClassPath[T]#ClassRep

  /**
   * The short name of the package (without prefix)
   */
  def name: String

  /** Info which should be propagated to any sub-classpaths.
   */
  def context: ClassPathContext[T]

  /** Lists of entities.
   */
  val classes: List[AnyClassRep]
  val packages: List[ClassPath[T]]
  val sourcepaths: List[AbstractFile]

  /**
   * Represents classes which can be loaded with a ClassfileLoader/MSILTypeLoader
   * and / or a SourcefileLoader.
   */
  case class ClassRep(binary: Option[T], source: Option[AbstractFile]) {
    def name: String = binary match {
      case Some(x)  => context.toBinaryName(x)
      case _        =>
        assert(source.isDefined)
        toSourceName(source.get)
    }
  }

  /** Filters for assessing validity of various entities.
   */
  def validClassFile(name: String)  = (name endsWith ".class") && context.isValidName(name)
  def validPackage(name: String)    = (name != "META-INF") && (name != "") && (name.head != '.')
  def validSourceFile(name: String) = validSourceExtensions exists (name endsWith _)
  def validSourceExtensions         = List(".scala", ".java")

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def findClass(name: String): Option[AnyClassRep] =
    splitWhere(name, _ == '.', true) match {
      case Some((pkg, rest)) =>
        val rep = packages find (_.name == pkg) flatMap (_ findClass rest)
        rep map {
          case x: ClassRep  => x
          case x            => throw new FatalError("Unexpected ClassRep '%s' found searching for name '%s'".format(x, name))
        }
      case _ =>
        classes find (_.name == name)
    }

  def findSourceFile(name: String): Option[AbstractFile] =
    findClass(name) match {
      case Some(ClassRep(Some(x: AbstractFile), _)) => Some(x)
      case _                                        => None
    }
}

/**
 * A Classpath containing source files
 */
class SourcePath[T](dir: AbstractFile, val context: ClassPathContext[T]) extends ClassPath[T] {
  def name = dir.name
  val sourcepaths: List[AbstractFile] = List(dir)

  lazy val classes: List[ClassRep] = dir partialMap {
    case f if !f.isDirectory && validSourceFile(f.name) => ClassRep(None, Some(f))
  } toList

  lazy val packages: List[SourcePath[T]] = dir partialMap {
    case f if f.isDirectory && validPackage(f.name) => new SourcePath[T](f, context)
  } toList


  override def toString() = "sourcepath: "+ dir.toString()
}

/**
 * A directory (or a .jar file) containing classfiles and packages
 */
class DirectoryClassPath(dir: AbstractFile, val context: ClassPathContext[AbstractFile]) extends ClassPath[AbstractFile] {
  def name = dir.name
  val sourcepaths: List[AbstractFile] = Nil

  lazy val classes: List[ClassRep] = dir partialMap {
    case f if !f.isDirectory && validClassFile(f.name) => ClassRep(Some(f), None)
  } toList

  lazy val packages: List[DirectoryClassPath] = dir partialMap {
    case f if f.isDirectory && validPackage(f.name) => new DirectoryClassPath(f, context)
  } toList


  override def toString() = "directory classpath: "+ dir.toString()
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
class MergedClassPath[T](
  protected val entries: List[ClassPath[T]],
  val context: ClassPathContext[T])
extends ClassPath[T] {

  def name = entries.head.name
  lazy val sourcepaths: List[AbstractFile] = entries flatMap (_.sourcepaths)

  lazy val classes: List[AnyClassRep] = {
    val cls = new ListBuffer[AnyClassRep]
    for (e <- entries; c <- e.classes) {
      val name = c.name
      val idx = cls.indexWhere(_.name == name)
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
      val idx = pkg.indexWhere(_.name == name)
      if (idx >= 0) {
        pkg(idx) = addPackage(pkg(idx), p)
      } else {
        pkg += p
      }
    }
    pkg.toList
  }

  private def addPackage(to: ClassPath[T], pkg: ClassPath[T]) = {
    val newEntries = to match {
      case cp: MergedClassPath[_] => cp.entries :+ pkg
      case _                      => List(to, pkg)
    }
    new MergedClassPath[T](newEntries, context)
  }

  override def toString() = "merged classpath "+ entries.mkString("(", "\n", ")")
}

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(
  containers: List[List[ClassPath[AbstractFile]]],
  context: JavaContext)
extends MergedClassPath[AbstractFile](containers.flatten, context) { }
