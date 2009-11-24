/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.io

/**
 *  @author  Martin Odersky
 *  @version 1.0, 04/10/2004
 */
object UTF8Codec
{
  final val UNI_REPLACEMENT_CHAR: Int = 0x0000FFFD
  final val UNI_REPLACEMENT_BYTES = Array[Byte](-17, -65, -67)

  // Note, from http://unicode.org/faq/utf_bom.html#utf8-5
  //
  // A different issue arises if an unpaired surrogate is encountered when converting
  // ill-formed UTF-16 data. By represented such an unpaired surrogate on its own as a
  // 3-byte sequence, the resulting UTF-8 data stream would become ill-formed.
  // While it faithfully reflects the nature of the input, Unicode conformance
  // requires that encoding form conversion always results in valid data stream.
  // Therefore a converter must treat this as an error.
  //
  // Some useful locations:
  //    http://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt

  @deprecated("""Use new String(Array(ch), 0, 1).getBytes("UTF-8") instead""")
  def encode(ch: Int): Array[Byte] =
    if ((Character getType ch) == Character.SURROGATE.toInt) UNI_REPLACEMENT_BYTES
    else try new String(Array(ch), 0, 1) getBytes "UTF-8" catch {
      case _: IllegalArgumentException  => UNI_REPLACEMENT_BYTES
    }

  @deprecated("Use Codec.fromUTF8 instead")
  def encode(src: Array[Char], from: Int, dst: Array[Byte], to: Int, len: Int): Int = {
    val bytes = Codec fromUTF8 src.slice(from, from + len)
    Array.copy(bytes, 0, dst, to, bytes.length)
    bytes.length
  }

  @deprecated("Use Codec.fromUTF8 instead")
  def encode(s: String, dst: Array[Byte], to: Int): Int =
    encode(s.toArray, 0, dst, to, s.length)

  @deprecated("Use Codec.fromUTF8 instead")
  def encode(s: String): Array[Byte] = Codec fromUTF8 s

  @deprecated("Use Codec.toUTF8 instead")
  def decode(src: Array[Byte], from: Int, dst: Array[Char], to: Int, len: Int): Int = {
    val chars = Codec toUTF8 src.slice(from, from + len)
    Array.copy(chars, 0, dst, to, chars.length)
    chars.length
  }

  @deprecated("Use Codec.toUTF8 instead")
  def decode(src: Array[Byte], from: Int, len: Int): String =
    Codec toUTF8 src.slice(from, from + len) mkString
}
