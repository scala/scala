/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2006, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id: UTF8Codec.scala 7116 2006-04-11 15:36:05Z mihaylov $


package scala.io;


object UTF8Codec {

  def encode(src: Array[Char], from: Int, dst: Array[Byte], to: Int, len: Int): Int = {
    var i = from;
    var j = to;
    val end = from + len;
    while (i < end) {
      val ch = src(i);
      i = i + 1;
      if (ch < 128) {
        dst(j) = ch.toByte;
        j = j + 1;
      }
      else if (ch <= 0x3FF) {
        dst(j)   = (0xC0 | (ch >> 6)).toByte;
        dst(j+1) = (0x80 | (ch & 0x3F)).toByte;
        j = j + 2;
      } else {
        dst(j)   = (0xE0 | (ch >> 12)).toByte;
        dst(j+1) = (0x80 | ((ch >> 6) & 0x3F)).toByte;
        dst(j+2) = (0x80 | (ch & 0x3F)).toByte;
        j = j + 3;
      }
    }
    j
  }

  def encode(s: String, dst: Array[Byte], to: Int): Int =
    encode(s.toCharArray(), 0, dst, to, s.length());


  def encode(s: String): Array[Byte] = {
    val dst = new Array[Byte](s.length() * 3);
    val len = encode(s, dst, 0);
    dst.subArray(0, len)
  }

  def decode(src: Array[Byte], from: Int,
             dst: Array[Char], to: Int, len: Int): Int =
  {
    var i = from;
    var j = to;
    val end = from + len;
    while (i < end) {
      var b = src(i) & 0xFF;
      i = i + 1;
      if (b >= 0xE0) {
        b = ((b & 0x0F) << 12) | (src(i) & 0x3F) << 6;
        b = b | (src(i+1) & 0x3F);
        i = i + 2;
      } else if (b >= 0xC0) {
        b = ((b & 0x1F) << 6) | (src(i) & 0x3F);
        i = i + 1;
      }
      dst(j) = b.toChar;
      j = j + 1;
    }
    j
  }

  def decode(src: Array[Byte], from: Int, len: Int): String = {
    val cs = new Array[Char](len);
    String.copyValueOf(cs, 0, decode(src, 0, cs, 0, len));
  }

}
