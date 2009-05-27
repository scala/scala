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
object UTF8Codec {

  final val UNI_REPLACEMENT_CHAR: Int = 0x0000FFFD

  /** convert a codepoint to utf-8 bytes
   * @author buraq
   * @param ch codepoint
   */
  def encode(ch1: Int): Array[Byte] = {
    var ch = ch1
    val byteMask = 0xBF
    val byteMark = 0x80
    var bytesToWrite = 0
    val firstByteMark = List[Byte](0x00.toByte, 0x00.toByte, 0xC0.toByte, 0xE0.toByte, 0xF0.toByte, 0xF8.toByte, 0xFC.toByte)

    if      (ch < 0x80)        { bytesToWrite = 1 }
    else if (ch < 0x800)       { bytesToWrite = 2 }
    else if (ch < 0x10000)     { bytesToWrite = 3 }
    else if (ch <= 0x0010FFFF) { bytesToWrite = 4 }
    else return encode(UNI_REPLACEMENT_CHAR)

    val res = new Array[Byte](bytesToWrite)

    var bw = bytesToWrite
    if (bw >= 4) {
      res(3) = ((ch | byteMark) & byteMask).toByte; ch = ch >> 6; bw -= 1
    }
    if (bw >= 3) {
      res(2) = ((ch | byteMark) & byteMask).toByte; ch = ch >> 6; bw -= 1
    }
    if (bw >= 2) {
      res(1) = ((ch | byteMark) & byteMask).toByte; ch = ch >> 6; bw -= 1
    }
    if (bw >= 1) {
      res(0) = (ch | firstByteMark(bytesToWrite)).toByte
    }
    res
  }

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
    dst.slice(0, len)
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
