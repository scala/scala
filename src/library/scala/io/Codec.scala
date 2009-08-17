/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

import java.nio.charset.{ Charset, CharsetDecoder, CharsetEncoder, CharacterCodingException, CodingErrorAction => Action }

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
class Codec(val charSet: Charset)
{
  type Configure[T] = (T => T, Boolean)

  type Handler = CharacterCodingException => Int
  private[this] var _onMalformedInput: Action       = null
  private[this] var _onUnmappableCharacter: Action  = null
  // private[this] var _replacement: Array[Byte]       = null

  private[this] var _onCodingException: Handler = e => throw e

  def name = charSet.name
  def encoder =
    applyFunctions[CharsetEncoder](charSet.newEncoder(),
      (_ onMalformedInput _onMalformedInput, _onMalformedInput != null),
      (_ onUnmappableCharacter _onUnmappableCharacter, _onUnmappableCharacter != null)
      // (_ replaceWith _replacement, _replacement != null)
    )

  def decoder =
    applyFunctions[CharsetDecoder](charSet.newDecoder(),
      (_ onMalformedInput _onMalformedInput, _onMalformedInput != null),
      (_ onUnmappableCharacter _onUnmappableCharacter, _onUnmappableCharacter != null)
      // (_ replaceWith _replacement, _replacement != null)
    )

  def wrap(body: => Int): Int =
    try body catch { case e: CharacterCodingException => _onCodingException(e) }

  // by default we ignore bad characters.
  // this behavior can be altered by overriding these two methods
  def onMalformedInput(newAction: Action): this.type = { _onMalformedInput = newAction ; this }
  def onUnmappableCharacter(newAction: Action): this.type = { _onUnmappableCharacter = newAction ; this }
  // def replaceWith(newReplacement: String): this.type = { _replacement = newReplacement ; this }
  def onCodingException(handler: Handler): this.type = { _onCodingException = handler ; this }

  // call a series of side effecting methods on an object, finally returning the object
  def applyFunctions[T](x: T, fs: Configure[T]*) =
    fs.foldLeft(x)((x, pair) => pair match {
      case (f, cond) => if (cond) f(x) else x
    })
}

object Codec {
  def default                               = apply(Charset.defaultCharset)
  def apply(encoding: String): Codec        = new Codec(Charset forName encoding)
  def apply(charSet: Charset): Codec        = new Codec(charSet)
  def apply(decoder: CharsetDecoder): Codec = {
    val _decoder = decoder
    new Codec(decoder.charset()) { override def decoder = _decoder }
  }

  implicit def string2codec(s: String) = apply(s)
  implicit def charset2codec(c: Charset) = apply(c)
  implicit def decoder2codec(cd: CharsetDecoder) = apply(cd)
}