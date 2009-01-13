/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.tools.util

/** This object provides methods for encoding/decoding UTF-8 characters.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object UTF8Codec {

  def encode(src: Array[Char], from: Int, dst: Array[Byte], to: Int, len: Int): Int = {
    var i = from
    var j = to
    val end = from + len
    while (i < end) {
      val ch = src(i)
      i += 1
      if (ch < 128) {
        dst(j) = ch.toByte
        j += 1
      }
      else if (ch <= 0x3FF) {
        dst(j)   = (0xC0 | (ch >> 6)).toByte
        dst(j+1) = (0x80 | (ch & 0x3F)).toByte
        j += 2
      } else {
        dst(j)   = (0xE0 | (ch >> 12)).toByte
        dst(j+1) = (0x80 | ((ch >> 6) & 0x3F)).toByte
        dst(j+2) = (0x80 | (ch & 0x3F)).toByte
        j += 3
      }
    }
    j
  }

  def encode(s: String, dst: Array[Byte], to: Int): Int =
    encode(s.toCharArray(), 0, dst, to, s.length())

  def encode(s: String): Array[Byte] = {
    val dst = new Array[Byte](s.length() * 3)
    val len = encode(s, dst, 0)
    dst.subArray(0, len)
  }

  def decode(src: Array[Byte], from: Int,
             dst: Array[Char], to: Int, len: Int): Int =
  {
    var i = from
    var j = to
    val end = from + len
    while (i < end) {
      var b = src(i) & 0xFF
      i += 1
      if (b >= 0xE0) {
        b = ((b & 0x0F) << 12) | (src(i) & 0x3F) << 6
        b = b | (src(i+1) & 0x3F)
        i += 2
      } else if (b >= 0xC0) {
        b = ((b & 0x1F) << 6) | (src(i) & 0x3F)
        i += 1
      }
      dst(j) = b.toChar
      j += 1
    }
    j
  }

  def decode(src: Array[Byte], from: Int, len: Int): String = {
    val cs = new Array[Char](len)
    new String(cs, 0, decode(src, 0, cs, 0, len))
  }

}
