/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.reflect.generic

object ByteCodecs {

  def bump(src: Array[Byte], len: Int, delta: Int) {
    var i = 0
    while (i < len) {
      val in: Int = src(i) & 0xff
      src(i) = (in + delta).toByte
      i += 1
    }
  }

  def encodeZeroes(src: Array[Byte]): Array[Byte] = {
    var i = 0
    val srclen = src.length
    var count = 0
    while (i < srclen) {
      if (src(i) == 0) count += 1
      i += 1
    }
    val dst = new Array[Byte](srclen + count)
    i = 0
    var j = 0
    while (i < srclen) {
      val in = src(i)
      if (in == 0) {
        dst(j) = (0xc0).toByte
        dst(j + 1) = (0x80).toByte
        j += 2
      } else {
        dst(j) = in
        j += 1
      }
      i += 1
    }
    dst
  }

  def decodeZeroes(src: Array[Byte]): Int = {
    var i = 0
    val srclen = src.length
    var j = 0
    while (i < srclen) {
      val in: Int = src(i) & 0xff
      if (in == 0xc0 && (src(i + 1) & 0xff) == 0x80) {
        src(j) = 0
        i += 2
      } else {
        src(j) = in.toByte
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

  def decode7to8(src: Array[Byte], srclen: Int, dstlen: Int) {
    var i = 0
    var j = 0
    while (i + 7 < srclen) {
      var out: Int = src(i)
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
      var out: Int = src(i)
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
  }

  def encode(xs: Array[Byte]): Array[Byte] = {
    val ys = xs.clone()
    bump(ys, ys.length, 1)
    encodeZeroes(encode8to7(ys))
  }

  /** Destructively decode array xs */
  def decode(xs: Array[Byte], dstlen: Int) {
    val len = decodeZeroes(xs)
    decode7to8(xs, len, dstlen)
    bump(xs, dstlen, -1)
  }

// test & debug, preliminary ----------------------------------

  def testBump(xs: Array[Byte]) {
    val xs0 = xs.clone()
    bump(xs, xs.length, 1)
    bump(xs, xs.length, -1)
    assert(xs.deep == xs0.deep, xs0.deep)
  }

  def testZeroes(xs: Array[Byte]) {
    val ys = encodeZeroes(xs)
    val len = decodeZeroes(ys)
    assert(len == xs.length && ys.take(len).deep == xs.deep,
           "testZeroes("+xs.deep+") failed; len = "+len+", result = "+ys.take(len).deep)
  }

  def test8to7(xs: Array[Byte]) {
    val ys = encode8to7(xs)
    decode7to8(ys, ys.length, xs.length)
    assert(ys.take(xs.length).deep == xs.deep,
           "test8to7("+xs.deep+") failed, result = "+ys.take(xs.length).deep)
  }

  def testAll(xs: Array[Byte]) {
    val ys = encode(xs)
    decode(ys, xs.length)
    assert(ys.take(xs.length).deep == xs.deep,
           "testAll("+xs.deep+") failed, result = "+ys.take(xs.length).deep)
  }

  def test(inputs: Array[Byte]*) {
    for (input <- inputs) {
      testBump(input)
      test8to7(input)
      testAll(input)
    }
  }

  def main(args: Array[String]) {
    test(
      Array(1, 2, 3),
      Array(1, 2, 3, 4, 5, 6, 7),
      Array(1, -2, 0, -3, -5, -6, -7),
      Array(1, 3, -1, -128, 0, 0, -128, 1, 2, 3))
    val rand = new scala.util.Random()
    for (i <- 1 until 50000) {
      if (i % 100 == 0) println("tested "+i)
      var xs = new Array[Byte](i)
      rand.nextBytes(xs)
      test(xs)
    }
    println("tested OK")
  }
}








