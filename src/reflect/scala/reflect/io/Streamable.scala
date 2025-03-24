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

import java.net.URL
import java.io.{BufferedInputStream, InputStream}
import java.io.{BufferedReader, InputStreamReader, Closeable => JCloseable}
import scala.io.{ Codec, BufferedSource, Source }
import scala.collection.mutable.ArrayBuffer
import Path.fail
import scala.annotation.tailrec

/** Traits for objects which can be represented as Streams.
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
object Streamable {
  /** Traits which can be viewed as a sequence of bytes.  Source types
   *  which know their length should override def length: Long for more
   *  efficient method implementations.
   *
   *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
   *
   *  Note that this code was not written with resource management in mind.
   *  Several methods (such as `chars` and `lines`) create InputStreams they
   *  don't close
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
        return (new ArrayBuffer[Byte]() ++= bytes()).toArray

      val arr = new Array[Byte](length.toInt)
      val len = arr.length
      lazy val in = bufferedInput()
      var offset = 0

      @tailrec
      def loop(): Unit = {
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

  /** For objects which can be viewed as Chars.
   *
   * ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
   */
  trait Chars extends Bytes {
    /** Calls to methods requiring byte<->char transformations should be offered
     *  in a form which allows specifying the codec.  When it is not specified,
     *  the one discovered at creation time will be used, which will always find the
     *  one in scala.io.Codec if no other is available.  This can be overridden
     *  to use a different default.
     */
    def creationCodec: Codec = implicitly[Codec]

    /** Caller is responsible for closing the returned BufferedSource. */
    def chars(codec: Codec): BufferedSource = Source.fromInputStream(inputStream())(codec)

    /** Beware! Leaks an InputStream which will not be closed until it gets finalized. */
    def lines(): Iterator[String] = lines(creationCodec)

    /** Beware! Leaks an InputStream which will not be closed until it gets finalized. */
    def lines(codec: Codec): Iterator[String] = chars(codec).getLines()

    /** Obtains an InputStreamReader wrapped around a FileInputStream.
     */
    def reader(codec: Codec): InputStreamReader = new InputStreamReader(inputStream(), codec.charSet)

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
    def slurp(codec: Codec) = {
      val src = chars(codec)
      try src.mkString finally src.close()  // Always Be Closing
    }
  }

  /** Call a function on something Closeable, finally closing it. */
  def closing[T <: JCloseable, U](stream: T)(f: T => U): U =
    try f(stream)
    finally stream.close()

  def bytes(is: => InputStream): Array[Byte] =
    (new Bytes {
      def inputStream() = is
    }).toByteArray()

  def slurp(is: => InputStream)(implicit codec: Codec): String =
    new Chars { def inputStream() = is } slurp codec

  def slurp(url: URL)(implicit codec: Codec): String =
    slurp(url.openStream())
}
