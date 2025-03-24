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

package scala
package reflect
package io

import scala.language.implicitConversions

import java.io.{RandomAccessFile, File => JFile}
import java.net.{URI, URL}
import scala.annotation.tailrec
import scala.util.Random.alphanumeric

/** An abstraction for filesystem paths.  The differences between
 *  Path, File, and Directory are primarily to communicate intent.
 *  Since the filesystem can change at any time, there is no way to
 *  reliably associate Files only with files and so on.  Any Path
 *  can be converted to a File or Directory (and thus gain access to
 *  the additional entity specific methods) by calling toFile or
 *  toDirectory, which has no effect on the filesystem.
 *
 *  Also available are createFile and createDirectory, which attempt
 *  to create the path in question.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object Path {
  def isExtensionJarOrZip(jfile: JFile): Boolean = isExtensionJarOrZip(jfile.getName)
  def isExtensionJarOrZip(name: String): Boolean =
    name.lastIndexOf('.') match {
      case i if i >= 0 =>
        val xt = name.substring(i + 1)
        xt.equalsIgnoreCase("jar") || xt.equalsIgnoreCase("zip")
      case _ => false
    }

  /** Lower case "extension", following the last dot. */
  def extension(name: String): String = {
    val i = name.lastIndexOf('.')
    if (i < 0) ""
    else name.substring(i + 1).toLowerCase
  }

  // not certain these won't be problematic, but looks good so far
  implicit def string2path(s: String): Path = apply(s)
  implicit def jfile2path(jfile: JFile): Path = apply(jfile)

  def onlyDirs(xs: Iterator[Path]): Iterator[Directory] = xs filter (_.isDirectory) map (_.toDirectory)
  def onlyDirs(xs: List[Path]): List[Directory] = xs filter (_.isDirectory) map (_.toDirectory)
  def onlyFiles(xs: Iterator[Path]): Iterator[File] = xs filter (_.isFile) map (_.toFile)

  def roots: List[Path] = java.io.File.listRoots().toList map Path.apply

  def apply(path: String): Path = apply(new JFile(path))
  def apply(jfile: JFile): Path = try {
    def isFile = {
      //if (settings.areStatisticsEnabled) Statistics.incCounter(IOStats.fileIsFileCount)
      jfile.isFile
    }

    def isDirectory = {
      //if (settings.areStatisticsEnabled) Statistics.incCounter(IOStats.fileIsDirectoryCount)
      jfile.isDirectory
    }

    if (isFile) new File(jfile)
    else if (isDirectory) new Directory(jfile)
    else new Path(jfile)
  } catch { case ex: SecurityException => new Path(jfile) }

  /** Avoiding any shell/path issues by only using alphanumerics. */
  private[io] def randomPrefix = alphanumeric take 6 mkString ""
  private[io] def fail(msg: String) = throw FileOperationException(msg)
}
import Path._

/** The Path constructor is private so we can enforce some
 *  semantics regarding how a Path might relate to the world.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class Path private[io] (val jfile: JFile) {
  val separator = java.io.File.separatorChar
  val separatorStr = java.io.File.separator

  // conversions
  def toFile: File = new File(jfile)
  def toDirectory: Directory = new Directory(jfile)
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())
  def toCanonical: Path = Path(jfile.getCanonicalPath())
  def toURI: URI = jfile.toURI()
  def toURL: URL = toURI.toURL()

  /** If this path is absolute, returns it: otherwise, returns an absolute
   *  path made up of root / this.
   */
  def toAbsoluteWithRoot(root: Path) = if (isAbsolute) this else root.toAbsolute / this

  /** Creates a new Path with the specified path appended.  Assumes
   *  the type of the new component implies the type of the result.
   */
  def /(child: Path): Path = if (isEmpty) child else new Path(new JFile(jfile, child.path))
  def /(child: Directory): Directory = /(child: Path).toDirectory
  def /(child: File): File = /(child: Path).toFile

  /** If this path is a directory, recursively iterate over its contents.
   *  The supplied condition is a filter which is applied to each element,
   *  with that branch of the tree being closed off if it is false.
   *  So for example if the condition is false for some subdirectory, nothing
   *  under that directory will be in the Iterator. If it's true, all files for
   *  which the condition holds and are directly in that subdirectory are in the
   *  Iterator, and all sub-subdirectories are recursively evaluated
   */
  def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (isFile) toFile walkFilter cond
    else if (isDirectory) toDirectory walkFilter cond
    else Iterator.empty

  /** Equivalent to walkFilter(_ => true).
   */
  def walk: Iterator[Path] = walkFilter(_ => true)

  // identity
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: Path = Path(jfile.getAbsolutePath())

  def resolve(other: Path) = if (other.isAbsolute || isEmpty) other else /(other)
  def relativize(other: Path) = {
    assert(isAbsolute == other.isAbsolute, "Paths not of same type: "+this+", "+other)

    @tailrec
    def createRelativePath(baseSegs: List[String], otherSegs: List[String]) : String = {
      (baseSegs, otherSegs) match {
        case (b :: bs, o :: os) if b == o => createRelativePath(bs, os)
        case (bs, os) => ((".."+separator)*bs.length)+os.mkString(separatorStr)
      }
    }

    Path(createRelativePath(segments, other.segments))
  }

  def segments: List[String] = (path split separator).toList filterNot (_.length == 0)

  /**
   * @return The path of the parent directory, or root if path is already root
   */
  def parent: Directory = path match {
    case "" | "." => Directory("..")
    case _        =>
      // the only solution <-- a comment which could have used elaboration
      if (segments.nonEmpty && segments.last == "..")
        (path / "..").toDirectory
      else jfile.getParent match {
        case null =>
          if (isAbsolute) toDirectory // it should be a root. BTW, don't need to worry about relative pathed root
          else Directory(".")         // a dir under pwd
        case x    =>
          Directory(x)
      }
  }
  def parents: List[Directory] = {
    val p = parent
    if (p isSame this) Nil else p :: p.parents
  }
  // if name ends with an extension (e.g. "foo.jpg") returns the extension ("jpg"), otherwise ""
  def extension: String = {
    var i = name.length - 1
    while (i >= 0 && name.charAt(i) != '.')
      i -= 1

    if (i < 0) ""
    else name.substring(i + 1)
  }
  // compares against extensions in a CASE INSENSITIVE way.
  def hasExtension(ext: String, exts: String*) = {
    val lower = extension.toLowerCase
    ext.toLowerCase == lower || exts.exists(_.toLowerCase == lower)
  }
  // returns the filename without the extension.
  def stripExtension: String = name stripSuffix ("." + extension)
  // returns the Path with the extension.
  def addExtension(ext: String): Path = Path(path + "." + ext)
  // changes the existing extension out for a new one, or adds it
  // if the current path has none.
  def changeExtension(ext: String): Path = (
    if (extension == "") addExtension(ext)
    else Path(path.stripSuffix(extension) + ext)
  )

  // conditionally execute
  def ifFile[T](f: File => T): Option[T] = if (isFile) Some(f(toFile)) else None
  def ifDirectory[T](f: Directory => T): Option[T] = if (isDirectory) Some(f(toDirectory)) else None

  // Boolean tests
  def canRead = jfile.canRead()
  def canWrite = jfile.canWrite()
  def exists = {
    //if (settings.areStatisticsEnabled) Statistics.incCounter(IOStats.fileExistsCount)
    try jfile.exists() catch { case ex: SecurityException => false }
  }

  def isFile = {
    //if (settings.areStatisticsEnabled) Statistics.incCounter(IOStats.fileIsFileCount)
    try jfile.isFile() catch { case ex: SecurityException => false }
  }
  def isDirectory = {
    //if (settings.areStatisticsEnabled) Statistics.incCounter(IOStats.fileIsDirectoryCount)
    try jfile.isDirectory() catch { case ex: SecurityException => jfile.getPath == "." }
  }
  def isAbsolute = jfile.isAbsolute()
  def isEmpty = path.length == 0

  // Information
  def lastModified = jfile.lastModified()
  def length = jfile.length()

  // Boolean path comparisons
  def endsWith(other: Path) = segments endsWith other.segments
  def isSame(other: Path) = toCanonical == other.toCanonical
  def isFresher(other: Path) = lastModified > other.lastModified

  // creations
  def createDirectory(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (!res && failIfExists && exists) fail(s"Directory '$name' already exists.")
    else if (isDirectory) toDirectory
    else new Directory(jfile)
  }
  def createFile(failIfExists: Boolean = false): File = {
    val res = jfile.createNewFile()
    if (!res && failIfExists && exists) fail(s"File '$name' already exists.")
    else if (isFile) toFile
    else new File(jfile)
  }

  // deletions
  def delete() = jfile.delete()

  /** Deletes the path recursively. Returns false on failure.
   *  Use with caution!
   */
  def deleteRecursively(): Boolean = deleteRecursively(jfile)
  private def deleteRecursively(f: JFile): Boolean = {
    if (f.isDirectory) f.listFiles match {
      case null =>
      case xs   => xs foreach deleteRecursively
    }
    f.delete()
  }

  def truncate() =
    isFile && {
      val raf = new RandomAccessFile(jfile, "rw")
      raf setLength 0
      raf.close()
      length == 0
    }

  override def toString() = path
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode() = path.hashCode()
}
