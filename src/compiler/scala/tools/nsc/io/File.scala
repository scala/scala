/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.nsc
package io

import java.io.{
  FileInputStream, FileOutputStream, BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter,
  BufferedInputStream, BufferedOutputStream, IOException, PrintStream, File => JFile }
import java.nio.channels.{ Channel, FileChannel }
import scala.io.Codec

object File {
  def pathSeparator = JFile.pathSeparator
  def separator = JFile.separator

  def apply(path: Path)(implicit codec: Codec = null) =
    if (codec != null) new File(path.jfile)(codec)
    else path.toFile

  // Create a temporary file
  def makeTemp(prefix: String = Path.randomPrefix, suffix: String = null, dir: JFile = null) =
    apply(JFile.createTempFile(prefix, suffix, dir))

  type Closeable = { def close(): Unit }
  def closeQuietly(target: Closeable) {
    try target.close() catch { case e: IOException => }
  }

  // this is a workaround for http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6503430
  // we are using a static initializer to statically initialize a java class so we don't
  // trigger java.lang.InternalErrors later when using it concurrently.
  {
    val tmp = JFile.createTempFile("bug6503430", null, null)
    val in = new FileInputStream(tmp).getChannel()
    val out = new FileOutputStream(tmp, true).getChannel()
    out.transferFrom(in, 0, 0)
    ()
  }
}
import File._
import Path._

/** An abstraction for files.  For character data, a Codec
 *  can be supplied at either creation time or when a method
 *  involving character data is called (with the latter taking
 *  precedence if supplied.) If neither is available, the value
 *  of scala.io.Codec.default is used.
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
class File(jfile: JFile)(implicit val creationCodec: Codec = null)
extends Path(jfile)
with Streamable.Chars {
  def withCodec(codec: Codec): File = new File(jfile)(codec)
  override def addExtension(ext: String): File = super.addExtension(ext).toFile
  override def toAbsolute: File = if (isAbsolute) this else super.toAbsolute.toFile
  override def toDirectory: Directory = new Directory(jfile)
  override def toFile: File = this
  override def normalize: File = super.normalize.toFile
  override def isValid = jfile.isFile() || !jfile.exists()
  override def length = super[Path].length
  override def walkFilter(cond: Path => Boolean): Iterator[Path] =
    if (cond(this)) Iterator.single(this) else Iterator.empty

  /** Obtains an InputStream. */
  def inputStream() = new FileInputStream(jfile)

  /** Obtains a OutputStream. */
  def outputStream(append: Boolean = false) = new FileOutputStream(jfile, append)
  def bufferedOutput(append: Boolean = false) = new BufferedOutputStream(outputStream(append))
  def printStream(append: Boolean = false) = new PrintStream(outputStream(append), true)

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

  /** Creates a new file and writes all the Strings to it. */
  def writeAll(strings: String*): Unit = {
    val out = bufferedWriter()
    try strings foreach (out write _)
    finally out close
  }

  def appendAll(strings: String*): Unit = {
    val out = bufferedWriter(append = true)
    try strings foreach (out write _)
    finally out close
  }

  def copyTo(destPath: Path, preserveFileDate: Boolean = false): Boolean = {
    val CHUNK = 1024 * 1024 * 16  // 16 MB
    val dest = destPath.toFile
    if (!isValid) fail("Source %s is not a valid file." format name)
    if (this.normalize == dest.normalize) fail("Source and destination are the same.")
    if (!dest.parent.exists) fail("Destination cannot be created.")
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
        count = (size - pos) min CHUNK
        pos += out.transferFrom(in, pos, count)
      }
    }
    finally List[Closeable](out, out_s, in, in_s) foreach closeQuietly

    if (this.length != dest.length)
      fail("Failed to completely copy %s to %s".format(name, dest.name))

    if (preserveFileDate)
      dest.lastModified = this.lastModified

    true
  }

  /** Reflection since we're into the java 6+ API.
   */
  def setExecutable(executable: Boolean, ownerOnly: Boolean = true): Boolean = {
    type JBoolean = java.lang.Boolean
    val method =
      try classOf[JFile].getMethod("setExecutable", classOf[Boolean], classOf[Boolean])
      catch { case _: NoSuchMethodException => return false }

    try method.invoke(jfile, executable: JBoolean, ownerOnly: JBoolean).asInstanceOf[JBoolean].booleanValue
    catch { case _: Exception => false }
  }
}
