/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io

import java.io.{ InputStream, Reader, BufferedReader, InputStreamReader, IOException }
import java.nio.charset.{ Charset, CharsetDecoder, CodingErrorAction, CharacterCodingException, MalformedInputException }
import java.nio.channels.Channels
import Source._

object BufferedSource
{
  /** Reads data from <code>inputStream</code> with a buffered reader,
   *  using encoding in implicit parameter <code>codec</code>.
   *
   *  @param  inputStream  the input stream from which to read
   *  @param  bufferSize   buffer size (defaults to BufferedSource.DefaultBufSize)
   *  @param  reset        a () => Source which resets the stream (defaults to Source.NoReset)
   *  @param  codec        (implicit) a scala.io.Codec specifying behavior (defaults to BufferedSource.defaultCodec)
   *  @return              the buffered source
   */
  def fromInputStream(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: () => Source = null
  )(implicit codec: Codec = Codec.default) =
  {
    if (reset == null) new BufferedSource(inputStream, bufferSize, codec)
    else {
      def _reset = reset
      new BufferedSource(inputStream, bufferSize, codec) {
        override def reset = _reset()
      }
    }
  }
}

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir
 *  @version 1.0, 19/08/2004
 */
class BufferedSource(
  inputStream: InputStream,
  bufferSize: Int,
  codec: Codec)
extends Source
{
  val decoder = codec.decoder
  decoder.reset
  decoder onMalformedInput codec.malformedAction
  val reader = new BufferedReader(new InputStreamReader(inputStream, decoder), bufferSize)

  override val iter = new Iterator[Char] {
    private def getc(): Char =
      try     { reader.read().toChar }
      catch   { case e: CharacterCodingException => codec receivedMalformedInput e }

    var buf_char = getc
    def hasNext = reader.ready()
    def next = {
      val c = buf_char
      if (hasNext) buf_char = getc
      c
    }
  }
  def close: Unit     = reader.close
  def reset(): Source = NoReset()
}

