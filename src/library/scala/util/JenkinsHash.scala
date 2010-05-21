/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util

import java.nio.ByteBuffer

/**
 *  Original algorithm due to Bob Jenkins.
 *    http://burtleburtle.net/bob/c/lookup3.c
 *  Scala version partially adapted from java version by Gray Watson.
 *    http://256.com/sources/jenkins_hash_java/JenkinsHash.java
 *
 *  This is based on the 1996 version, not the 2006 version, and
 *  could most likely stand some improvement; the collision rate is
 *  negligible in my tests, but performance merits investigation.
 *
 *  @author  Paul Phillips
 */

object JenkinsHash {
  final val MAX_VALUE = 0xFFFFFFFFL

  private def bytesProvided(v: Any) = v match {
    case x: Byte      => 1
    case x: Short     => 2
    case x: Int       => 4
    case x: Long      => 8
    case x: Float     => 4
    case x: Double    => 8
    case x: Boolean   => 1
    case x: Char      => 2
    case x: Unit      => 0
    case _            => 4
  }

  private def putAnyVal(bb: ByteBuffer, v: AnyVal) = v match {
    case x: Byte      => bb put x
    case x: Short     => bb putShort x
    case x: Int       => bb putInt x
    case x: Long      => bb putLong x
    case x: Float     => bb putFloat x
    case x: Double    => bb putDouble x
    case x: Boolean   => bb.put(if (x) Byte.MaxValue else Byte.MinValue)
    case x: Char      => bb putChar x
    case x: Unit      =>
  }

  /** Not entirely sure how else one might do this these days, since
   *  matching on x: AnyVal is a compile time error.
   */
  private def classifyAny(x: Any): (Option[AnyVal], Option[AnyRef]) = x match {
    case x: Byte      => (Some(x), None)
    case x: Short     => (Some(x), None)
    case x: Int       => (Some(x), None)
    case x: Long      => (Some(x), None)
    case x: Float     => (Some(x), None)
    case x: Double    => (Some(x), None)
    case x: Boolean   => (Some(x), None)
    case x: Char      => (Some(x), None)
    case x: Unit      => (Some(x), None)
    case x: AnyRef    => (None, Some(x))
  }

  private def partitionValuesAndRefs(xs: Seq[Any]): (Seq[AnyVal], Seq[AnyRef]) = {
    val (avs, ars) = xs map classifyAny unzip

    (avs.flatten, ars.flatten)
  }

  private def hashAnyValSeq(xs: Seq[AnyVal]): Int = {
    val arr = new Array[Byte](xs map bytesProvided sum)
    val bb = ByteBuffer wrap arr
    xs foreach (x => putAnyVal(bb, x))

    hash(bb.array()).toInt
  }

  /**
   * Convert a byte into a long value without making it negative.
   */
  private def byteToLong(b: Byte): Long = {
    val res = b & 0x7F
    if ((b & 0x80) != 0L) res + 128
    else res
  }

  /**
   * Do addition and turn into 4 bytes.
   */
  private def add(x1: Long, x2: Long) = (x1 + x2) & MAX_VALUE

  /**
   * Do subtraction and turn into 4 bytes.
   */
  private def subtract(x1: Long, x2: Long) = (x1 - x2) & MAX_VALUE

  /**
   * Left shift val by shift bits and turn in 4 bytes.
   */
  private def xor(x1: Long, x2: Long) = (x1 ^ x2) & MAX_VALUE

  /**
   * Left shift val by shift bits.  Cut down to 4 bytes.
   */
  private def leftShift(x: Long, shift: Int) = (x << shift) & MAX_VALUE

  /**
   * Convert 4 bytes from the buffer at offset into a long value.
   */
  private def fourByteToLong(bytes: Array[Byte], offset: Int) =
    0 to 3 map (i => byteToLong(bytes(offset + i)) << (i * 8)) sum

  /**
   *  Hash a sequence of anything into a 32-bit value.  Descendants
   *  of AnyVal are broken down into individual bytes and mixed with
   *  some vigor, and this is summed with the hashCodes provided by
   *  the descendants of AnyRef.
   */
  def hashSeq(xs: Seq[Any]): Int = {
    val (values, refs) = partitionValuesAndRefs(xs)
    val refsSum = refs map (x => if (x == null) 0 else x.##) sum

    hashAnyValSeq(values) + refsSum
  }

  /**
   * Hash a variable-length key into a 32-bit value.  Every bit of the
   * key affects every bit of the return value.  Every 1-bit and 2-bit
   * delta achieves avalanche.  The best hash table sizes are powers of 2.
   *
   * @param buffer Byte array that we are hashing on.
   * @param initialValue Initial value of the hash if we are continuing from
   * a previous run.  0 if none.
   * @return Hash value for the buffer.
   */
  def hash(buffer: Array[Byte], initialValue: Long = 0L): Long = {
    var a, b  = 0x09e3779b9L
    var c     = initialValue

    def hashMix(): Long = {
      a = subtract(a, b); a = subtract(a, c); a = xor(a, c >> 13);
      b = subtract(b, c); b = subtract(b, a); b = xor(b, leftShift(a, 8));
      c = subtract(c, a); c = subtract(c, b); c = xor(c, (b >> 13));
      a = subtract(a, b); a = subtract(a, c); a = xor(a, (c >> 12));
      b = subtract(b, c); b = subtract(b, a); b = xor(b, leftShift(a, 16));
      c = subtract(c, a); c = subtract(c, b); c = xor(c, (b >> 5));
      a = subtract(a, b); a = subtract(a, c); a = xor(a, (c >> 3));
      b = subtract(b, c); b = subtract(b, a); b = xor(b, leftShift(a, 10));
      c = subtract(c, a); c = subtract(c, b); c = xor(c, (b >> 15));

      c
    }

    def mixTwelve(pos: Int) = {
      a = add(a, fourByteToLong(buffer, pos));
      b = add(b, fourByteToLong(buffer, pos + 4));
      c = add(c, fourByteToLong(buffer, pos + 8));
      hashMix()
    }

    // mix in blocks of 12
    var pos: Int = buffer.length
    while (pos >= 12) {
      pos -= 12
      mixTwelve(pos)
    }
    c += buffer.length

    // mix any leftover bytes (0-11 remaining)
    if (pos > 10) c = add(c, leftShift(byteToLong(buffer(10)), 24))
    if (pos > 9) c = add(c, leftShift(byteToLong(buffer(9)), 16))
    if (pos > 8) c = add(c, leftShift(byteToLong(buffer(8)), 8))
    if (pos > 7) b = add(b, leftShift(byteToLong(buffer(7)), 24))
    if (pos > 6) b = add(b, leftShift(byteToLong(buffer(6)), 16))
    if (pos > 5) b = add(b, leftShift(byteToLong(buffer(5)), 8))
    if (pos > 4) b = add(b, byteToLong(buffer(4)))
    if (pos > 3) a = add(a, leftShift(byteToLong(buffer(3)), 24))
    if (pos > 2) a = add(a, leftShift(byteToLong(buffer(2)), 16))
    if (pos > 1) a = add(a, leftShift(byteToLong(buffer(1)), 8))
    if (pos > 0) a = add(a, byteToLong(buffer(0)))

    // final mix and result
    hashMix()
  }
}
