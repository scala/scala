/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc
package io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, IOException, File => JFile }
import java.nio.channels.FileChannel
import collection.Traversable
import scala.io.Codec

object File
{
  def pathSeparator = JFile.pathSeparator

  def apply(path: Path)(implicit codec: Codec = null) =
    if (codec != null) new File(path.jfile)(codec)
    else path.toFile

  // Create a temporary file
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null) =
    apply(JFile.createTempFile(prefix, suffix, dir))

  import java.nio.channels.Channel
  type Closeable = { def close(): Unit }
  def closeQuietly(target: Closeable) {
    try target.close() catch { case e: IOException => }
  }
}
import File._
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
with Streamable.Chars
{
  def withCodec(codec: Codec): File = new File(jfile)(codec)
  override def toDirectory: Directory = new Directory(jfile)
  override def toFile: File = this

  override def isValid = jfile.isFile() || !jfile.exists()
  override def length = super[Path].length

  /** Obtains an InputStream. */
  def inputStream() = new FileInputStream(jfile)

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false) = new FileOutputStream(jfile, append)
  def bufferedOutput(append: Boolean = false) = new BufferedOutputStream(outputStream(append))

  /** Obtains an OutputStreamWriter wrapped around a FileOutputStream.
   *  This should behave like a less broken version of java.io.FileWriter,
   *  in that unlike the java version you can specify the encoding.
   */
  def writer(append: Boolean = false, codec: Codec = getCodec()) =
    new OutputStreamWriter(outputStream(append), codec.charSet)

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

  def copyFile(destPath: Path, preserveFileDate: Boolean = false) = {
    val FIFTY_MB = 1024 * 1024 * 50
    val dest = destPath.toFile
    if (!isValid) fail("Source %s is not a valid file." format name)
    if (this.normalize == dest.normalize) fail("Source and destination are the same.")
    if (!dest.parent.map(_.exists).getOrElse(false)) fail("Destination cannot be created.")
    if (dest.exists && !dest.canWrite) fail("Destination exists but is not writable.")
    if (dest.isDirectory) fail("Destination exists but is a directory.")

    lazy val in_s = inputStream()
    lazy val out_s = dest.outputStream()
    lazy val in = in_s.getChannel()
    lazy val out = out_s.getChannel()

    try {
      val size = in.size()
      var pos, count = 0L
      while (pos < size) {
        count = (size - pos) min FIFTY_MB
        pos += out.transferFrom(in, pos, count)
      }
    }
    finally List[Closeable](out, out_s, in, in_s) foreach closeQuietly

    if (this.length != dest.length)
      fail("Failed to completely copy %s to %s".format(name, dest.name))

    if (preserveFileDate)
      dest.lastModified = this.lastModified

    ()
  }

  override def toString() = "File(%s)".format(path)
}
