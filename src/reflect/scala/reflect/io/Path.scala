/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.reflect
package io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, RandomAccessFile }
import java.io.{ File => JFile }
import java.net.{ URI, URL }
import scala.util.Random.alphanumeric
import scala.language.implicitConversions

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
 *  @author  Paul Phillips
 *  @since   2.8
 *  
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object Path {
  def isExtensionJarOrZip(jfile: JFile): Boolean = isExtensionJarOrZip(jfile.getName)
  def isExtensionJarOrZip(name: String): Boolean = {
    val ext = extension(name)
    ext == "jar" || ext == "zip"
  }
  def extension(name: String): String = {
    var i = name.length - 1
    while (i >= 0 && name.charAt(i) != '.')
      i -= 1

    if (i < 0) ""
    else name.substring(i + 1).toLowerCase
  }

  // not certain these won't be problematic, but looks good so far
  implicit def string2path(s: String): Path = apply(s)
  implicit def jfile2path(jfile: JFile): Path = apply(jfile)

  // java 7 style, we don't use it yet
  // object AccessMode extends Enumeration {
  //   val EXECUTE, READ, WRITE = Value
  // }
  // def checkAccess(modes: AccessMode*): Boolean = {
  //   modes foreach {
  //     case EXECUTE  => throw new Exception("Unsupported") // can't check in java 5
  //     case READ     => if (!jfile.canRead()) return false
  //     case WRITE    => if (!jfile.canWrite()) return false
  //   }
  //   true
  // }

  def onlyDirs(xs: Iterator[Path]): Iterator[Directory] = xs filter (_.isDirectory) map (_.toDirectory)
  def onlyDirs(xs: List[Path]): List[Directory] = xs filter (_.isDirectory) map (_.toDirectory)
  def onlyFiles(xs: Iterator[Path]): Iterator[File] = xs filter (_.isFile) map (_.toFile)
  def onlyFiles(xs: List[Path]): List[File] = xs filter (_.isFile) map (_.toFile)

  def roots: List[Path] = java.io.File.listRoots().toList map Path.apply

  def apply(segments: Seq[String]): Path = apply(segments mkString java.io.File.separator)
  def apply(path: String): Path = apply(new JFile(path))
  def apply(jfile: JFile): Path =
    if (jfile.isFile) new File(jfile)
    else if (jfile.isDirectory) new Directory(jfile)
    else new Path(jfile)

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

  // Validation: this verifies that the type of this object and the
  // contents of the filesystem are in agreement.  All objects are
  // valid except File objects whose path points to a directory and
  // Directory objects whose path points to a file.
  def isValid: Boolean = true

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

  /** If this path is a container, recursively iterate over its contents.
   *  The supplied condition is a filter which is applied to each element,
   *  with that branch of the tree being closed off if it is true.  So for
   *  example if the condition is true for some subdirectory, nothing
   *  under that directory will be in the Iterator; but otherwise each
   *  file and subdirectory underneath it will appear.
   */
  def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (isFile) toFile walkFilter cond
    else if (isDirectory) toDirectory walkFilter cond
    else Iterator.empty

  /** Equivalent to walkFilter(_ => false).
   */
  def walk: Iterator[Path] = walkFilter(_ => true)

  // identity
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: Path = Path(jfile.getAbsolutePath())
  def isRootPath: Boolean = roots exists (_ isSame this)

  def resolve(other: Path) = if (other.isAbsolute || isEmpty) other else /(other)
  def relativize(other: Path) = {
    assert(isAbsolute == other.isAbsolute, "Paths not of same type: "+this+", "+other)

    def createRelativePath(baseSegs: List[String], otherSegs: List[String]) : String = {
      (baseSegs, otherSegs) match {
        case (b :: bs, o :: os) if b == o => createRelativePath(bs, os)
        case (bs, os) => ((".."+separator)*bs.length)+os.mkString(separatorStr)
      }
    }

    Path(createRelativePath(segments, other.segments))
  }

  // derived from identity
  def root: Option[Path] = roots find (this startsWith _)
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
  // def extension: String = (name lastIndexOf '.') match {
  //   case -1   => ""
  //   case idx  => name drop (idx + 1)
  // }
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
  def exists = jfile.exists()
  def notExists = try !jfile.exists() catch { case ex: SecurityException => false }

  def isFile = jfile.isFile()
  def isDirectory = jfile.isDirectory()
  def isAbsolute = jfile.isAbsolute()
  def isHidden = jfile.isHidden()
  def isEmpty = path.length == 0

  // Information
  def lastModified = jfile.lastModified()
  def lastModified_=(time: Long) = jfile setLastModified time // should use setXXX function?
  def length = jfile.length()

  // Boolean path comparisons
  def endsWith(other: Path) = segments endsWith other.segments
  def startsWith(other: Path) = segments startsWith other.segments
  def isSame(other: Path) = toCanonical == other.toCanonical
  def isFresher(other: Path) = lastModified > other.lastModified

  // creations
  def createDirectory(force: Boolean = true, failIfExists: Boolean = false): Directory = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (!res && failIfExists && exists) fail("Directory '%s' already exists." format name)
    else if (isDirectory) toDirectory
    else new Directory(jfile)
  }
  def createFile(failIfExists: Boolean = false): File = {
    val res = jfile.createNewFile()
    if (!res && failIfExists && exists) fail("File '%s' already exists." format name)
    else if (isFile) toFile
    else new File(jfile)
  }

  // deletions
  def delete() = jfile.delete()
  def deleteIfExists() = if (jfile.exists()) delete() else false

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

  def touch(modTime: Long = System.currentTimeMillis) = {
    createFile()
    if (isFile)
      lastModified = modTime
  }

  // todo
  // def copyTo(target: Path, options ...): Boolean
  // def moveTo(target: Path, options ...): Boolean

  override def toString() = path
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode() = path.hashCode()
}
