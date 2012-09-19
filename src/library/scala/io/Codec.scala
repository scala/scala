/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package io

import scala.language.implicitConversions
import scala.annotation.migration
import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder, CharacterCodingException, CodingErrorAction => Action, StandardCharsets }

/** A class for character encoding/decoding preferences. */
class Codec(val charSet: Charset) {
  type Configure[T] = (T => T, Boolean)
  type Handler      = CharacterCodingException => Int

  // these variables allow configuring the Codec object, and then
  // all decoders and encoders retrieved from it will use these settings.
  private[this] var _onMalformedInput: Action         = null
  private[this] var _onUnmappableCharacter: Action    = null
  private[this] var _encodingReplacement: Array[Byte] = null
  private[this] var _decodingReplacement: String      = null
  private[this] var _onCodingException: Handler       = e => throw e

  /** The name of the Codec. */
  override def toString = name

  // these methods can be chained to configure the variables above
  def onMalformedInput(newAction: Action): this.type              = { _onMalformedInput = newAction ; this }
  def onUnmappableCharacter(newAction: Action): this.type         = { _onUnmappableCharacter = newAction ; this }
  def decodingReplaceWith(newReplacement: String): this.type      = { _decodingReplacement = newReplacement ; this }
  def encodingReplaceWith(newReplacement: Array[Byte]): this.type = { _encodingReplacement = newReplacement ; this }
  def onCodingException(handler: Handler): this.type              = { _onCodingException = handler ; this }

  def name = charSet.name
  def encoder: CharsetEncoder = {
    val enc = charSet.newEncoder()
    if (_onMalformedInput ne null) enc onMalformedInput _onMalformedInput
    if (_onUnmappableCharacter ne null) enc onUnmappableCharacter _onUnmappableCharacter
    if (_encodingReplacement ne null) enc replaceWith _encodingReplacement
    enc
  }
  def decoder: CharsetDecoder = {
    val dec = charSet.newDecoder()
    if (_onMalformedInput ne null) dec onMalformedInput _onMalformedInput
    if (_onUnmappableCharacter ne null) dec onUnmappableCharacter _onUnmappableCharacter
    if (_decodingReplacement ne null) dec replaceWith _decodingReplacement
    dec
  }

  def wrap(body: => Int): Int =
    try body catch { case e: CharacterCodingException => _onCodingException(e) }
}

trait LowPriorityCodecImplicits {
  self: Codec.type =>

  /** The Codec of Last Resort. */
  implicit lazy val fallbackSystemCodec: Codec = defaultCharsetCodec
}

object Codec extends LowPriorityCodecImplicits {
  @deprecated("this method is misnamed and will be removed, use `Codec(StandardCharsets.ISO_8859_1) instead`", "2.12.0")
  final val ISO8859: Codec = new Codec(StandardCharsets.ISO_8859_1)
  final val UTF8: Codec    = new Codec(StandardCharsets.UTF_8)

  /* Optimistically these two possible defaults will be the same thing.
   *  In practice this is not necessarily true, and in fact Sun classifies
   *  the fact that you can influence anything at all via -Dfile.encoding
   *  as an accident, with any anomalies considered "not a bug".
   */
  /** Always returns a UTF8 codec. */
  def defaultCharsetCodec = UTF8
  @deprecated("use `Codec(scala.util.Properties.encodingString)` to create a codec influenced by the `-Dfile.encoding` JVM flag", "2.12.0")
  def fileEncodingCodec   = apply(scala.util.Properties.encodingString)
  def default             = UTF8

  def apply(encoding: String): Codec        = new Codec(Charset forName encoding)
  def apply(charSet: Charset): Codec        = new Codec(charSet)
  def apply(decoder: CharsetDecoder): Codec = {
    val _decoder = decoder
    new Codec(decoder.charset()) { override def decoder = _decoder }
  }

  @migration("This method was previously misnamed `toUTF8`. Converts from Array[Byte] to Array[Char].", "2.9.0")
  def fromUTF8(bytes: Array[Byte]): Array[Char] = fromUTF8(bytes, 0, bytes.length)
  def fromUTF8(bytes: Array[Byte], offset: Int, len: Int): Array[Char] = {
    val bbuffer = java.nio.ByteBuffer.wrap(bytes, offset, len)
    val cbuffer = UTF8.charSet decode bbuffer
    val chars   = new Array[Char](cbuffer.remaining())
    cbuffer get chars

    chars
  }

  @migration("This method was previously misnamed `fromUTF8`. Converts from character sequence to Array[Byte].", "2.9.0")
  def toUTF8(cs: CharSequence): Array[Byte] = {
    val cbuffer = java.nio.CharBuffer.wrap(cs, 0, cs.length)
    val bbuffer = UTF8.charSet encode cbuffer
    val bytes = new Array[Byte](bbuffer.remaining())
    bbuffer get bytes

    bytes
  }
  def toUTF8(chars: Array[Char], offset: Int, len: Int): Array[Byte] = {
    val cbuffer = java.nio.CharBuffer.wrap(chars, offset, len)
    val bbuffer = UTF8.charSet encode cbuffer
    val bytes = new Array[Byte](bbuffer.remaining())
    bbuffer get bytes

    bytes
  }

  implicit def string2codec(s: String): Codec           = apply(s)
  implicit def charset2codec(c: Charset): Codec         = apply(c)
  implicit def decoder2codec(cd: CharsetDecoder): Codec = apply(cd)
}
