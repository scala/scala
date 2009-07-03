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
  def name = charSet.name
  def decoder = charSet.newDecoder()
  def encoder = charSet.newEncoder()

  // by default we ignore bad characters.
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