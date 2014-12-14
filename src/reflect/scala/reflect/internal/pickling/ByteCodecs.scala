/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala
package reflect.internal.pickling

object ByteCodecs {

  def avoidZero(src: Array[Byte]): Array[Byte] = {
    var i = 0
    val srclen = src.length
    var count = 0
    while (i < srclen) {
      if (src(i) == 0x7f) count += 1
      i += 1
    }
    val dst = new Array[Byte](srclen + count)
    i = 0
    var j = 0
    while (i < srclen) {
      val in = src(i)
      if (in == 0x7f) {
        dst(j) = (0xc0).toByte
        dst(j + 1) = (0x80).toByte
        j += 2
      } else {
        dst(j) = (in + 1).toByte
        j += 1
      }
      i += 1
    }
    dst
  }

  def regenerateZero(src: Array[Byte]): Int = {
    var i = 0
    val srclen = src.length
    var j = 0
    while (i < srclen) {
      val in: Int = src(i) & 0xff
      if (in == 0xc0 && (src(i + 1) & 0xff) == 0x80) {
        src(j) = 0x7f
        i += 2
      } else if (in == 0) {
        src(j) = 0x7f
        i += 1
      } else {
        src(j) = (in - 1).toByte
        i += 1
      }
      j += 1
    }
    j
  }

  def encode8to7(src: Array[Byte]): Array[Byte] = {
    val srclen = src.length
    val dstlen = (srclen * 8 + 6) / 7
    val dst = new Array[Byte](dstlen)
    var i = 0
    var j = 0
    while (i + 6 < srclen) {
      var in: Int = src(i) & 0xff
      dst(j) = (in & 0x7f).toByte
      var out: Int = in >>> 7
      in = src(i + 1) & 0xff
      dst(j + 1) = (out | (in << 1) & 0x7f).toByte
      out = in >>> 6
      in = src(i + 2) & 0xff
      dst(j + 2) = (out | (in << 2) & 0x7f).toByte
      out = in >>> 5
      in = src(i + 3) & 0xff
      dst(j + 3) = (out | (in << 3) & 0x7f).toByte
      out = in >>> 4
      in = src(i + 4) & 0xff
      dst(j + 4) = (out | (in << 4) & 0x7f).toByte
      out = in >>> 3
      in = src(i + 5) & 0xff
      dst(j + 5) = (out | (in << 5) & 0x7f).toByte
      out = in >>> 2
      in = src(i + 6) & 0xff
      dst(j + 6) = (out | (in << 6) & 0x7f).toByte
      out = in >>> 1
      dst(j + 7) = out.toByte
      i += 7
      j += 8
    }
    if (i < srclen) {
      var in: Int = src(i) & 0xff
      dst(j) = (in & 0x7f).toByte; j += 1
      var out: Int = in >>> 7
      if (i + 1 < srclen) {
        in = src(i + 1) & 0xff
        dst(j) = (out | (in << 1) & 0x7f).toByte; j += 1
        out = in >>> 6
        if (i + 2 < srclen) {
          in = src(i + 2) & 0xff
          dst(j) = (out | (in << 2) & 0x7f).toByte; j += 1
          out = in >>> 5
          if (i + 3 < srclen) {
            in = src(i + 3) & 0xff
            dst(j) = (out | (in << 3) & 0x7f).toByte; j += 1
            out = in >>> 4
            if (i + 4 < srclen) {
              in = src(i + 4) & 0xff
              dst(j) = (out | (in << 4) & 0x7f).toByte; j += 1
              out = in >>> 3
              if (i + 5 < srclen) {
                in = src(i + 5) & 0xff
                dst(j) = (out | (in << 5) & 0x7f).toByte; j += 1
                out = in >>> 2
              }
            }
          }
        }
      }
      if (j < dstlen) dst(j) = out.toByte
    }
    dst
  }

  def decode7to8(src: Array[Byte], srclen: Int): Int = {
    var i = 0
    var j = 0
    val dstlen = (srclen * 7 + 7) / 8
    while (i + 7 < srclen) {
      var out: Int = src(i).toInt
      var in: Byte = src(i + 1)
      src(j) = (out | (in & 0x01) << 7).toByte
      out = in >>> 1
      in = src(i + 2)
      src(j + 1) = (out | (in & 0x03) << 6).toByte
      out = in >>> 2
      in = src(i + 3)
      src(j + 2) = (out | (in & 0x07) << 5).toByte
      out = in >>> 3
      in = src(i + 4)
      src(j + 3) = (out | (in & 0x0f) << 4).toByte
      out = in >>> 4
      in = src(i + 5)
      src(j + 4) = (out | (in & 0x1f) << 3).toByte
      out = in >>> 5
      in = src(i + 6)
      src(j + 5) = (out | (in & 0x3f) << 2).toByte
      out = in >>> 6
      in = src(i + 7)
      src(j + 6) = (out | in << 1).toByte
      i += 8
      j += 7
    }
    if (i < srclen) {
      var out: Int = src(i).toInt
      if (i + 1 < srclen) {
        var in: Byte = src(i + 1)
        src(j) = (out | (in & 0x01) << 7).toByte; j += 1
        out = in >>> 1
        if (i + 2 < srclen) {
          in = src(i + 2)
          src(j) = (out | (in & 0x03) << 6).toByte; j += 1
          out = in >>> 2
          if (i + 3 < srclen) {
            in = src(i + 3)
            src(j) = (out | (in & 0x07) << 5).toByte; j += 1
            out = in >>> 3
            if (i + 4 < srclen) {
              in = src(i + 4)
              src(j) = (out | (in & 0x0f) << 4).toByte; j += 1
              out = in >>> 4
              if (i + 5 < srclen) {
                in = src(i + 5)
                src(j) = (out | (in & 0x1f) << 3).toByte; j += 1
                out = in >>> 5
                if (i + 6 < srclen) {
                  in = src(i + 6)
                  src(j) = (out | (in & 0x3f) << 2).toByte; j += 1
                  out = in >>> 6
                }
              }
            }
          }
        }
      }
      if (j < dstlen) src(j) = out.toByte
    }
    dstlen
  }

  def encode(xs: Array[Byte]): Array[Byte] = avoidZero(encode8to7(xs))

  /**
   * Destructively decodes array xs and returns the length of the decoded array.
   *
   * Sometimes returns (length+1) of the decoded array. Example:
   *
   *   scala> val enc = scala.reflect.internal.pickling.ByteCodecs.encode(Array(1,2,3))
   *   enc: Array[Byte] = Array(2, 5, 13, 1)
   *
   *   scala> scala.reflect.internal.pickling.ByteCodecs.decode(enc)
   *   res43: Int = 4
   *
   *   scala> enc
   *   res44: Array[Byte] = Array(1, 2, 3, 0)
   *
   * However, this does not always happen.
   */
  def decode(xs: Array[Byte]): Int = {
    val len = regenerateZero(xs)
    decode7to8(xs, len)
  }
}
