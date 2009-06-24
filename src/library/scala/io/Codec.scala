/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

import java.nio.charset.{ Charset, CharsetDecoder, CodingErrorAction }

/** A class for character encoding/decoding preferences.
 *
 */
class Codec(charSet: Charset) {
  def name = charSet.name
  def decoder = charSet.newDecoder()

  // by default we replace bad chars with the decoder's replacement value (e.g. "?")
  // this behavior can be altered by overriding these two methods
  def malformedAction(): CodingErrorAction = CodingErrorAction.IGNORE
  def receivedMalformedInput(e: Exception): Char = decoder.replacement()(0)
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