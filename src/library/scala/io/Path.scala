/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, File => JFile }
import java.net.{ URI, URL }
import collection.{ Sequence, Traversable }
import collection.immutable.{ StringVector => SV }
import PartialFunction._
import util.Random.nextASCIIString

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
 */

object Path
{
  // not sure whether this will be problematic
  implicit def string2path(s: String): Path = apply(s)
  implicit def jfile2path(jfile: JFile): Path = apply(jfile)

  // java 7 style, we don't use it yet
  // object AccessMode extends Enumeration("AccessMode") {
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

  def roots: List[Path] = JFile.listRoots().toList map Path.apply

  def apply(path: String): Path = apply(new JFile(path))
  def apply(jfile: JFile): Path = {
    if (jfile.isFile) new File(jfile)
    else if (jfile.isDirectory) new Directory(jfile)
    else new Path(jfile)
  }
  private[io] def randomPrefix = nextASCIIString(6)
  private[io] def fail(msg: String) = throw FileOperationException(msg)
}
import Path._

/** The Path constructor is private so we can enforce some
 *  semantics regarding how a Path might relate to the world.
 */
class Path private[io] (val jfile: JFile)
{
  val separator = JFile.separatorChar

  // Validation: this verifies that the type of this object and the
  // contents of the filesystem are in agreement.  All objects are
  // valid except File objects whose path points to a directory and
  // Directory objects whose path points to a file.
  def isValid: Boolean = true

  // conversions
  def toFile: File = new File(jfile)
  def toDirectory: Directory = new Directory(jfile)
  def toAbsolute: Path = if (isAbsolute) this else Path(jfile.getAbsolutePath())
  def toURI: URI = jfile.toURI()
  def toURL: URL = toURI.toURL()

  /** Creates a new Path with the specified path appended.  Assumes
   *  the type of the new component implies the type of the result.
   */
  def /(child: Path): Path = new Path(new JFile(jfile, child.path))
  def /(child: Directory): Directory = /(child: Path).toDirectory
  def /(child: File): File = /(child: Path).toFile

  // identity
  def name: String = jfile.getName()
  def path: String = jfile.getPath()
  def normalize: Path = Path(jfile.getCanonicalPath())
  // todo -
  // def resolve(other: Path): Path
  // def relativize(other: Path): Path

  // derived from identity
  def root: Option[Path] = roots find (this startsWith _)
  def segments: List[String] = (path split separator).toList filterNot (_.isEmpty)
  def parent: Option[Path] = Option(jfile.getParent()) map Path.apply
  def parents: List[Path] = parent match {
    case None     => Nil
    case Some(p)  => p :: p.parents
  }
  // if name ends with an extension (e.g. "foo.jpg") returns the extension ("jpg")
  def extension: Option[String] =
    condOpt(SV.lastIndexWhere(name, _ == '.')) {
      case idx if idx != -1 => SV.drop(name, idx + 1)
    }
  // Alternative approach:
  // (Option fromReturnValue SV.lastIndexWhere(name, _ == '.') map (x => SV.drop(name, x + 1))

  // Boolean tests
  def canRead = jfile.canRead()
  def canWrite = jfile.canWrite()
  def exists = jfile.exists()
  def notExists = try !jfile.exists() catch { case ex: SecurityException => false }

  def isFile = jfile.isFile()
  def isDirectory = jfile.isDirectory()
  def isAbsolute = jfile.isAbsolute()
  def isHidden = jfile.isHidden()

  // Information
  def lastModified = jfile.lastModified()
  def length = jfile.length()

  // Boolean path comparisons
  def endsWith(other: Path) = segments endsWith other.segments
  def startsWith(other: Path) = segments startsWith other.segments
  def isSame(other: Path) = toAbsolute == other.toAbsolute
  def isFresher(other: Path) = lastModified > other.lastModified

  // creations
  def create(): Boolean = true
  def createDirectory(force: Boolean = true): Directory = {
    val res = if (force) jfile.mkdirs() else jfile.mkdir()
    if (res) new Directory(jfile)
    else fail("Failed to create new directory.")
  }
  def createFile(): File =
    if (jfile.createNewFile()) new File(jfile)
    else fail("Failed to create new file.")

  // deletions
  def delete() = jfile.delete()
  def deleteIfExists() = if (jfile.exists()) delete() else false

  // todo
  // def copyTo(target: Path, options ...): Boolean
  // def moveTo(target: Path, options ...): Boolean

  override def toString() = "Path(%s)".format(path)
  override def equals(other: Any) = other match {
    case x: Path  => path == x.path
    case _        => false
  }
  override def hashCode() = path.hashCode()
}
