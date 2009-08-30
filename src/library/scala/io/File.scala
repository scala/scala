/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, File => JFile }
import collection.Traversable

object File
{
  def apply(path: Path) = path.toFile

  // Create a temporary file
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null) =
    apply(JFile.createTempFile(prefix, suffix, dir))
}
import Path._

/** An abstraction for files.  For character data, a Codec
 *  can be supplied at either creation time or when a method
 *  involving character data is called (with the latter taking
 *  precdence if supplied.) If neither is available, the value
 *  of scala.io.Codec.default is used.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class File(jfile: JFile)(implicit val creationCodec: Codec = null)
extends Path(jfile)
{
  private def getCodec(): Codec =
    if (creationCodec == null) Codec.default else creationCodec

  override def toDirectory: Directory = new Directory(jfile)
  override def toFile: File = this
  override def create(): Boolean = jfile.createNewFile()
  override def isValid = jfile.isFile() || !jfile.exists()

  /** Convenience functions for iterating over the bytes in a file.
   */
  def bytesAsInts(): Iterator[Int] = {
    val in = bufferedInput()
    Iterator continually in.read() takeWhile (_ != -1)
  }

  def bytes(): Iterator[Byte] = bytesAsInts() map (_.toByte)
  def chars(codec: Codec = getCodec()) = (Source fromFile jfile)(codec)

  /** Convenience function for iterating over the lines in the file.
   */
  def lines(codec: Codec = getCodec()): Iterator[String] = chars(codec).getLines()

  /** Convenience function to import entire file into a String.
   */
  def slurp(codec: Codec = getCodec()) = chars(codec).mkString

  /** Obtains an InputStream. */
  def inputStream() = new FileInputStream(jfile)
  def bufferedInput() = new BufferedInputStream(inputStream())

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false) = new FileOutputStream(jfile, append)
  def bufferedOutput(append: Boolean = false) = new BufferedOutputStream(outputStream(append))

  /** Obtains an InputStreamReader wrapped around a FileInputStream.
   */
  def reader(codec: Codec = getCodec()) =
    new InputStreamReader(inputStream, codec.charSet)

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  def writer(append: Boolean = false, codec: Codec = getCodec()) =
    new OutputStreamWriter(outputStream(append), codec.charSet)

  /** Wraps a BufferedReader around the result of reader().
   */
  def bufferedReader(codec: Codec = getCodec()) = new BufferedReader(reader(codec))

  /** Wraps a BufferedWriter around the result of writer().
   */
  def bufferedWriter(append: Boolean = false, codec: Codec = getCodec()) =
    new BufferedWriter(writer(append, codec))

  /** Writes all the Strings in the given iterator to the file. */
  def writeAll(xs: Traversable[String], append: Boolean = false, codec: Codec = getCodec()): Unit = {
    val out = bufferedWriter(append, codec)
    try xs foreach (out write _)
    finally out close
  }

  override def toString() = "File(%s)".format(path)
}
