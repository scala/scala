package org.scalacheck
package rng

import scala.annotation.tailrec
import scala.util.Try

/**
 * Simple RNG by Bob Jenkins:
 *
 * http://burtleburtle.net/bob/rand/smallprng.html
 */
sealed abstract class Seed extends Serializable {
  protected val a: Long
  protected val b: Long
  protected val c: Long
  protected val d: Long

  /**
   * Generate a Base-64 representation of this seed.
   *
   * Given a seed, this method will return a String with 44
   * characters, according to the web-safe Base-64 specification
   * (i.e. using minus (-) and underscore (_) in addition to
   * alphanumeric characters).
   *
   * The 256-bit seed is serialized as a little-endian array of 64-bit
   * Long values. Strings produced by this method are guaranteed to be
   * parseable by the Seed.fromBase64 method.
   */
  def toBase64: String = {
    def enc(x: Long): Char = Seed.Alphabet((x & 0x3f).toInt)
    val chars = new Array[Char](44)
    def encode(x: Long, shift: Int, i: Int, rest: List[Long]): String =
      if (shift < 58) {
        chars(i) = enc(x >>> shift)
        encode(x, shift + 6, i + 1, rest)
      } else rest match {
        case Nil =>
          chars(i) = enc(x >>> 60)
          chars(i + 1) = '='
          new String(chars)
        case y :: ys =>
          val sh = 64 - shift
          chars(i) = enc((x >>> shift) | (y << sh))
          encode(y, (6 - sh) % 6, i + 1, ys)
      }
    encode(a, 0, 0, b :: c :: d :: Nil)
  }

  /** Generate the next seed in the RNG's sequence. */
  def next: Seed = {
    import java.lang.Long.rotateLeft
    val e = a - rotateLeft(b, 7)
    val a1 = b ^ rotateLeft(c, 13)
    val b1 = c + rotateLeft(d, 37)
    val c1 = d + e
    val d1 = e + a
    Seed(a1, b1, c1, d1)
  }

  /** Reseed the RNG using the given Long value. */
  def reseed(n: Long): Seed = {
    val n0 = ((n >>> 32) & 0xffffffff)
    val n1 = (n & 0xffffffff)
    var i = 0
    var seed: Seed = Seed(a ^ n0, b ^ n1, c, d)
    while(i < 16) { seed = seed.next; i += 1 }
    seed
  }

  /**
   * This is a quick way of deterministically sliding this RNG to a
   * different part of the PRNG sequence.
   *
   * We use this as an easy way to "split" the RNG off into a new part
   * of the sequence. We want to do this in situations where we've
   * already called .next several times, and we want to avoid
   * repeating those numbers while preserving determinism.
   */
  def slide: Seed = {
    val (n, s) = long
    s.reseed(n)
  }

  /**
   * Generates a Long value.
   *
   * The values will be uniformly distributed. */
  def long: (Long, Seed) = (d, next)

  /**
   * Generates a Double value.
   *
   * The values will be uniformly distributed, and will be contained
   * in the interval [0.0, 1.0). */
  def double: (Double, Seed) = ((d >>> 11) * 1.1102230246251565e-16, next)
}

object Seed {

  private case class apply(a: Long, b: Long, c: Long, d: Long) extends Seed {
    override def toString: String = s"""Seed.fromBase64("$toBase64")"""
  }

  /** Generate a deterministic seed. */
  def apply(s: Long): Seed = {
    var i = 0
    var seed: Seed = Seed(0xf1ea5eed, s, s, s)
    while (i < 20) { seed = seed.next; i += 1 }
    seed
  }

  /**
   * Generate a seed directly from four Long values.
   *
   * Warning: unlike Seed.apply(Long), this method just directly
   * constructs a seed from the four Long values. Prefer using
   * `Seed(Long)` if you aren't sure whether these will be good seed
   * values.
   */
  def fromLongs(a: Long, b: Long, c: Long, d: Long): Seed =
    apply(a, b, c, d)

  /**
   * Alphabet of characters used by the `toBase64` method.
   *
   * Since we're using the web-safe Base-64 specification, we are
   * using minus (-) and underscore(_) in addition to the alphanumeric
   * characters.
   */
  private[scalacheck] final val Alphabet: Array[Char] =
    ((0 until 26).map(i => ('A' + i).toChar) ++
      (0 until 26).map(i => ('a' + i).toChar) ++
      (0 until 10).map(i => ('0' + i).toChar) ++
      Vector('-', '_')).toArray

  /**
   * Parse a Base-64 encoded seed, returning a Seed value.
   *
   * This method requires the exact format produced by `toBase64`
   * (i.e. a 44-character string using the web-safe Base-64
   * alphabet). Other encodings must produce precisely the same
   * outputs to be supported.
   *
   * This method will throw an IllegalArgumentException if parsing
   * fails.
   */
  def fromBase64(s: String): Try[Seed] = {
    def fail(s: String): Nothing = throw new IllegalArgumentException(s)

    def dec(c: Char): Long =
      if      ('A' <= c && c <= 'Z') (c - 'A').toLong
      else if ('a' <= c && c <= 'z') ((c - 'a') + 26).toLong
      else if ('0' <= c && c <= '9') ((c - '0') + 52).toLong
      else if (c == '-') 62L
      else if (c == '_') 63L
      else fail(s"illegal Base64 character: $c")

    val longs = new Array[Long](4)
    @tailrec def decode(x: Long, shift: Int, i: Int, j: Int): Seed =
      if (i >= 43) {
        Seed.fromLongs(longs(0), longs(1), longs(2), longs(3))
      } else {
        val b = dec(s.charAt(i))
        if (shift < 58) {
          decode(x | (b << shift), shift + 6, i + 1, j)
        } else {
          longs(j) = x | (b << shift)
          val sh = 64 - shift
          decode(b >>> sh, 6 - sh, i + 1, j + 1)
        }
      }

    Try {
      if (s.length != 44) fail(s"wrong Base64 length: $s")
      if (s.charAt(43) != '=') fail(s"wrong Base64 format: $s")
      if (s.charAt(42) == '=') fail(s"wrong Base64 format: $s")
      decode(0L, 0, 0, 0)
    }
  }

  /** Generate a random seed. */
  def random(): Seed = apply(scala.util.Random.nextLong)

}
