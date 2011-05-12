/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package io

import java.net.{ URI, URL }
import java.io.{ BufferedInputStream, InputStream, PrintStream }
import java.io.{ BufferedReader, InputStreamReader, Closeable => JCloseable }
import scala.io.{ Codec, BufferedSource, Source }
import collection.mutable.ArrayBuffer
import Path.fail

/** Traits for objects which can be represented as Streams.
 *
 *  @author Paul Phillips
 *  @since  2.8
 */

object Streamable {
  /** For reading a stream of unknown length.  Doesn't buffer the
   *  input stream: call the two argument version.
   */
  def toByteArray(in: InputStream): Array[Byte] = {
    val bytes   = new ArrayBuffer[Byte]()
    val buf     = new Array[Byte](4096)

    def loop(): Array[Byte] = {
      val bytesRead = in.read(buf)
      if (bytesRead < 0) sys.error("read error")
      else if (bytesRead == 0) bytes.toArray
      else {
        bytes ++= buf.slice(0, bytesRead)
        loop()
      }
    }
    try loop()
    finally in.close()
  }
  /** This method aspires to be the fastest way to read
   *  a stream of known length into memory.
   */
  def toByteArray(input: InputStream, len: Int): Array[Byte] = {
    val in = new BufferedInputStream(input)
    if (len < 0)
      return toByteArray(in)

    val arr = new Array[Byte](len)
    var offset = 0

    def loop() {
      if (offset < len) {
        val read = in.read(arr, offset, len - offset)
        if (read >= 0) {
          offset += read
          loop()
        }
      }
    }
    try loop()
    finally in.close()

    if (offset == arr.length) arr
    else sys.error("Could not read entire source (%d of %d bytes)".format(offset, len))
  }

  /** Traits which can be viewed as a sequence of bytes.  Source types
   *  which know their length should override def length: Long for more
   *  efficient method implementations.
   */
  trait Bytes {
    def inputStream(): InputStream

    def length: Long = -1
    def bufferedInput() = new BufferedInputStream(inputStream())
    def bytes(): Iterator[Byte] = bytesAsInts() map (_.toByte)
    def bytesAsInts(): Iterator[Int] = {
      val in = bufferedInput()
      Iterator continually in.read() takeWhile (_ != -1)
    }

    def toByteArray() = {
      require(length <= Int.MaxValue, length + " larger than Int.MaxValue")
      Streamable.toByteArray(inputStream(), length.toInt)
    }
  }

  /** For objects which can be viewed as Chars.
   */
  trait Chars extends Bytes {
    /** Calls to methods requiring byte<->char transformations should be offered
     *  in a form which allows specifying the codec.  When it is not specified,
     *  the one discovered at creation time will be used, which will always find the
     *  one in scala.io.Codec if no other is available.  This can be overridden
     *  to use a different default.
     */
    def creationCodec: Codec = implicitly[Codec]

    def chars(): BufferedSource = chars(creationCodec)
    def chars(codec: Codec): BufferedSource = Source.fromInputStream(inputStream())(codec)

    def lines(): Iterator[String] = lines(creationCodec)
    def lines(codec: Codec): Iterator[String] = chars(codec).getLines()

    /** Obtains an InputStreamReader wrapped around a FileInputStream.
     */
    def reader(): InputStreamReader = reader(creationCodec)
    def reader(codec: Codec): InputStreamReader = new InputStreamReader(inputStream, codec.charSet)

    /** Wraps a BufferedReader around the result of reader().
     */
    def bufferedReader(): BufferedReader = bufferedReader(creationCodec)
    def bufferedReader(codec: Codec) = new BufferedReader(reader(codec))

    /** Creates a BufferedReader and applies the closure, automatically closing it on completion.
     */
    def applyReader[T](f: BufferedReader => T): T = {
      val in = bufferedReader()
      try f(in)
      finally in.close()
    }

    /** Convenience function to import entire file into a String.
     */
    def slurp(): String = slurp(creationCodec)
    def slurp(codec: Codec) = chars(codec).mkString
  }

  /** Call a function on something Closeable, finally closing it. */
  def closing[T <: JCloseable, U](stream: T)(f: T => U): U =
    try f(stream)
    finally stream.close()

  def bytes(is: => InputStream): Array[Byte] =
    new Bytes { def inputStream() = is } toByteArray

  def slurp(is: => InputStream)(implicit codec: Codec): String =
    new Chars { def inputStream() = is } slurp codec

  def slurp(url: URL)(implicit codec: Codec): String =
    slurp(url.openStream())
}
