/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

import java.io.{ FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter, File => JFile }
import collection.Traversable

object File
{
  final val extensionRegex = """^.*\.([^.]+)$""".r
  implicit def fileWrapper(file: JFile): File = new File(file)

  def apply(fileName: String) = new File(new JFile(fileName))
  def apply(file: JFile)      = new File(file)
}
import File._

/** An abstraction for files.  For now it is little more than a wrapper
 *  around java.io.File.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class File(val file: JFile) extends collection.Iterable[File]
{
  /** If file is a directory, an iterator over its contents.
   *  If not, an empty iterator.
   */
  def iterator: Iterator[File] =
    if (file.isDirectory) file.listFiles.iterator map (x => new File(x))
    else Iterator.empty

  /** Convenience function for iterating over the lines in the file.
   */
  def lines(implicit codec: Codec = Codec.default): Iterator[String] = toSource(codec).getLines()

  /** Convenience function to import entire file into a String.
   */
  def slurp(implicit codec: Codec = Codec.default) = toSource(codec).mkString

  /** Deletes the file or directory recursively. Returns false if it failed.
   *  Use with caution!
   */
  def deleteRecursively(): Boolean = deleteRecursively(file)
  private def deleteRecursively(f: JFile): Boolean = {
    if (f.isDirectory) file.listFiles match {
      case null =>
      case xs   => xs foreach deleteRecursively
    }
    f.delete()
  }

  /** Obtains an InputStream. */
  def inputStream() = new FileInputStream(file)

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false) = new FileOutputStream(file, append)

  /** Obtains an InputStreamReader wrapped around a FileInputStream.
   */
  def reader(implicit codec: Codec = Codec.default) =
    new InputStreamReader(inputStream, codec.charSet)

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  def writer(append: Boolean = false)(implicit codec: Codec = Codec.default) =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  /** Wraps a BufferedReader around the result of reader().
   */
  def bufferedReader(implicit codec: Codec = Codec.default) =
    new BufferedReader(reader(codec))

  /** Wraps a BufferedWriter around the result of writer().
   */
  def bufferedWriter(append: Boolean = false)(implicit codec: Codec = Codec.default) =
    new BufferedWriter(writer(append)(codec))

  /** Writes all the Strings in the given iterator to the file. */
  def writeAll(xs: Traversable[String], append: Boolean = false)(implicit codec: Codec = Codec.default): Unit = {
    val out = bufferedWriter(append)(codec)
    xs foreach (out write _)
    out close
  }

  /** Attempts to return the file extension. */
  def extension = file.getName match {
    case extensionRegex(x)  => Some(x)
    case _                  => None
  }

  /** Creates a Source from this file. */
  def toSource(implicit codec: Codec = Codec.default): Source =
    (Source fromFile file)(codec)

  /** Creates a new File with the specified path appended. */
  def /(child: String) = new File(new JFile(file, child))

  override def toString() = file.toString
  override def equals(other: Any) = other match {
    case x: File    => this.file == x.file
    case _          => false
  }
  override def hashCode = file.hashCode
}
