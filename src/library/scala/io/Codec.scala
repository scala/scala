/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.io

import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder, CharacterCodingException, CodingErrorAction => Action }
import annotation.migration

// Some notes about encodings for use in refining this implementation.
//
// Emails: encoding recorded in header, e.g. Content-Type: charset= "iso-8859-1"
// HTML: optional content-type meta tag.
//   <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
// XML: optional encoding parameter.
//   <?xml version="1.0" encoding="ISO8859-1" ?>
//
// MacRoman vs. UTF-8: see http://jira.codehaus.org/browse/JRUBY-3576
// -Dfile.encoding: see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4375816

/** A class for character encoding/decoding preferences.
 *
 */
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

  // these methods can be chained to configure the variables above
  def onMalformedInput(newAction: Action): this.type = { _onMalformedInput = newAction ; this }
  def onUnmappableCharacter(newAction: Action): this.type = { _onUnmappableCharacter = newAction ; this }
  def decodingReplaceWith(newReplacement: String): this.type = { _decodingReplacement = newReplacement ; this }
  def encodingReplaceWith(newReplacement: Array[Byte]): this.type = { _encodingReplacement = newReplacement ; this }
  def onCodingException(handler: Handler): this.type = { _onCodingException = handler ; this }

  def name = charSet.name
  def encoder =
    applyFunctions[CharsetEncoder](charSet.newEncoder(),
      (_ onMalformedInput _onMalformedInput, _onMalformedInput != null),
      (_ onUnmappableCharacter _onUnmappableCharacter, _onUnmappableCharacter != null),
      (_ replaceWith _encodingReplacement, _encodingReplacement != null)
    )

  def decoder =
    applyFunctions[CharsetDecoder](charSet.newDecoder(),
      (_ onMalformedInput _onMalformedInput, _onMalformedInput != null),
      (_ onUnmappableCharacter _onUnmappableCharacter, _onUnmappableCharacter != null),
      (_ replaceWith _decodingReplacement, _decodingReplacement != null)
    )

  def wrap(body: => Int): Int =
    try body catch { case e: CharacterCodingException => _onCodingException(e) }

  // call a series of side effecting methods on an object, finally returning the object
  private def applyFunctions[T](x: T, fs: Configure[T]*) =
    fs.foldLeft(x)((x, pair) => pair match {
      case (f, cond) => if (cond) f(x) else x
    })
}

trait LowPriorityCodecImplicits {
  self: Codec.type =>

  /** The Codec of Last Resort. */
  implicit def fallbackSystemCodec: Codec = defaultCharsetCodec
}

object Codec extends LowPriorityCodecImplicits {
  final val ISO8859: Codec = new Codec(Charset forName "ISO-8859-1")
  final val UTF8: Codec    = new Codec(Charset forName "UTF-8")

  /** Optimistically these two possible defaults will be the same thing.
   *  In practice this is not necessarily true, and in fact Sun classifies
   *  the fact that you can influence anything at all via -Dfile.encoding
   *  as an accident, with any anomalies considered "not a bug".
   */
  def defaultCharsetCodec                   = apply(Charset.defaultCharset)
  def fileEncodingCodec                     = apply(util.Properties.encodingString)
  def default                               = defaultCharsetCodec

  def apply(encoding: String): Codec        = new Codec(Charset forName encoding)
  def apply(charSet: Charset): Codec        = new Codec(charSet)
  def apply(decoder: CharsetDecoder): Codec = {
    val _decoder = decoder
    new Codec(decoder.charset()) { override def decoder = _decoder }
  }

  @migration("This method was previously misnamed `toUTF8`. Converts from Array[Byte] to Array[Char].", "2.9.0")
  def fromUTF8(bytes: Array[Byte]): Array[Char] = {
    val bbuffer = java.nio.ByteBuffer wrap bytes
    val cbuffer = UTF8.charSet decode bbuffer
    val chars = new Array[Char](cbuffer.remaining())
    cbuffer get chars

    chars
  }

  @migration("This method was previously misnamed `fromUTF8`. Converts from character sequence to Array[Byte].", "2.9.0")
  def toUTF8(cs: CharSequence): Array[Byte] = {
    val cbuffer = java.nio.CharBuffer wrap cs
    val bbuffer = UTF8.charSet encode cbuffer
    val bytes = new Array[Byte](bbuffer.remaining())
    bbuffer get bytes

    bytes
  }

  implicit def string2codec(s: String) = apply(s)
  implicit def charset2codec(c: Charset) = apply(c)
  implicit def decoder2codec(cd: CharsetDecoder) = apply(cd)
}
