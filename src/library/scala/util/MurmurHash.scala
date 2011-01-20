/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util

/** An implementation of MurmurHash 2.0.
 *  http://sites.google.com/site/murmurhash/
 *
 *  It is here primarily for use by case classes.
 *
 *  @author  archontophoenix
 *  @version 2.9
 *  @since   2.9
 */

object MurmurHash {
  private def bytesProvided (v: Any) = v match {
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

  private def highInt (v: Any): Int = {
    val l = v match {
      case x: Long   => x
      case x: Double => java.lang.Double.doubleToRawLongBits(x)
      case _         => throw new IllegalArgumentException("No highInt: " + v)
    }

    l >>> 32 toInt
  }

  final val m = 0x5bd1e995
  final val r = 24

  private def step (hh: Int, kk: Int): Int = {
    var h = hh
    var k = kk
    k *= m
    k ^= k >>> r
    k *= m
    h *= m
    h ^ k
  }

  def product(p: Product): Int = product(p, 0)
  def product(p: Product, seed: Int): Int = {
    // Initialize the hash to a 'random' value
    var n = p.productArity
    var h = seed ^ n
    var partial = 0
    var nextPartialByte = 0
    while (n > 0) {
      n -= 1
      val el: Any = p.productElement(n)
      bytesProvided(el) match {
        case 0 =>
        case 1 =>
          partial |= el.## << (nextPartialByte * 8)
          if (nextPartialByte < 3)
            nextPartialByte += 1
          else {
            h = step(h,partial)
            partial = 0
            nextPartialByte = 0
          }
        case 2 =>
          val i = el.##
          partial |= i << (nextPartialByte * 8)
          if (nextPartialByte < 2)
            nextPartialByte += 2
          else {
            h = step(h,partial)
            nextPartialByte -= 2
            partial = if (nextPartialByte == 0) 0 else i >>> 8
          }
        case 4 =>
          h = step(h, el.##)
        case 8 =>
          h = step(h, el.##)
          h = step(h, highInt(el))
      }
    }
    // Handle the last few bytes of the input array
    if (nextPartialByte > 0) {
      h ^= partial
      h *= m
    }
    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    h ^= h >>> 13
    h *= m
    h ^ (h >>> 15)
  }

  def string(s: String): Int = {
    // Initialize the hash to a 'random' value
    var n = s.length
    var h = n
    var nn = n & ~ 1
    while (nn > 0) {
      nn -= 2
      h = step(h, (s(nn + 1) << 16) | s(nn))
    }
    // Handle the last few bytes of the input array
    if ((n & 1) != 0) {
      h ^= s(n - 1)
      h *= m
    }
    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    h ^= h >>> 13
    h *= m
    h ^ (h >>> 15)
  }
}
