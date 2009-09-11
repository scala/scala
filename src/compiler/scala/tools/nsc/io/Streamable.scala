/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 */

package scala.tools.nsc
package io

import java.net.{ URI, URL }
import java.io.{ BufferedInputStream, InputStream, PrintStream, File => JFile }
import java.io.{ BufferedReader, InputStreamReader }
import scala.io.{ Codec, Source }

import collection.mutable.ArrayBuffer
import Path.fail

/** Traits for objects which can be represented as Streams.
 *
 *  @author Paul Phillips
 *  @since  2.8
 */

object Streamable
{
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

    /** This method aspires to be the fastest way to read
     *  a stream of known length into memory.
     */
    def toByteArray(): Array[Byte] = {
      // if we don't know the length, fall back on relative inefficiency
      if (length == -1L)
        return (new ArrayBuffer[Byte]() ++ bytes()).toArray

      val arr = new Array[Byte](length.toInt)
      val len = arr.length
      lazy val in = bufferedInput()
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
      else fail("Could not read entire source (%d of %d bytes)".format(offset, len))
    }
  }

  /** For objects which can be viewed as Chars.  The abstract creationCodec
   *  can safely be defined as null and will subsequently be ignored.
   */
  trait Chars extends Bytes {
    def creationCodec: Codec
    private def failNoCodec() = fail("This method requires a Codec to be chosen explicitly.")

    /** The general algorithm for any call to a method involving byte<->char
     *  transformations is: if a codec is supplied (explicitly or implicitly),
     *  use that; otherwise if a codec was defined when the object was created,
     *  use that; otherwise, use Codec.default.
     *
     *  Note that getCodec takes a codec argument rather than having methods
     *  always default to getCodec() and use the argument otherwise, so that
     *  method implementations can, where desired, identify the case where no
     *  codec was ever explicitly supplied.  If allowDefault = false, an
     *  exception will be thrown rather than falling back on Codec.default.
     */
    def getCodec(givenCodec: Codec = null, allowDefault: Boolean = true) =
      if (givenCodec != null) givenCodec
      else if (creationCodec != null) creationCodec
      else if (allowDefault) Codec.default
      else failNoCodec()

    def chars(codec: Codec = getCodec()): Source = (Source fromInputStream inputStream())(codec)
    def lines(codec: Codec = getCodec()): Iterator[String] = chars(codec).getLines()

    /** Obtains an InputStreamReader wrapped around a FileInputStream.
     */
    def reader(codec: Codec = getCodec()) = new InputStreamReader(inputStream, codec.charSet)

    /** Wraps a BufferedReader around the result of reader().
     */
    def bufferedReader(codec: Codec = getCodec()) = new BufferedReader(reader(codec))

    /** Convenience function to import entire file into a String.
     */
    def slurp(codec: Codec = getCodec()) = chars(codec).mkString
  }
}