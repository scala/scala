/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package util

import scala.annotation.migration
import scala.collection.mutable.ArrayBuffer
import scala.collection.BuildFrom
import scala.collection.immutable.{ List, LazyList }
import scala.language.{implicitConversions, higherKinds}

/**
 *  @author Stephane Micheloud
 *
 */
class Random(val self: java.util.Random) extends AnyRef with Serializable {
  /** Creates a new random number generator using a single long seed. */
  def this(seed: Long) = this(new java.util.Random(seed))

  /** Creates a new random number generator using a single integer seed. */
  def this(seed: Int) = this(seed.toLong)

  /** Creates a new random number generator. */
  def this() = this(new java.util.Random())

  /** Returns the next pseudorandom, uniformly distributed boolean value
   *  from this random number generator's sequence.
   */
  def nextBoolean(): Boolean = self.nextBoolean()

  /** Generates random bytes and places them into a user-supplied byte
   *  array.
   */
  def nextBytes(bytes: Array[Byte]) { self.nextBytes(bytes) }

  /** Returns the next pseudorandom, uniformly distributed double value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextDouble(): Double = self.nextDouble()

  /** Returns the next pseudorandom, uniformly distributed double value
   *  between min (inclusive) and max (exclusive) from this random number generator's sequence.
   */
  def between(minInclusive: Double, maxExclusive: Double): Double = {
    require(minInclusive < maxExclusive, "Invalid bounds")

    val next = nextDouble() * (maxExclusive - minInclusive) + minInclusive
    if (next < maxExclusive) next
    else Math.nextAfter(maxExclusive, Double.NegativeInfinity)
  }

  /** Returns the next pseudorandom, uniformly distributed float value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextFloat(): Float = self.nextFloat()

  /** Returns the next pseudorandom, uniformly distributed float value
   *  between min (inclusive) and max (exclusive) from this random number generator's sequence.
   */
  def between(minInclusive: Float, maxExclusive: Float): Float = {
    require(minInclusive < maxExclusive, "Invalid bounds")

    val next = nextFloat() * (maxExclusive - minInclusive) + minInclusive
    if (next < maxExclusive) next
    else Math.nextAfter(maxExclusive, Float.NegativeInfinity)
  }

  /** Returns the next pseudorandom, Gaussian ("normally") distributed
   *  double value with mean 0.0 and standard deviation 1.0 from this
   *  random number generator's sequence.
   */
  def nextGaussian(): Double = self.nextGaussian()

  /** Returns the next pseudorandom, uniformly distributed int value
   *  from this random number generator's sequence.
   */
  def nextInt(): Int = self.nextInt()

  /** Returns a pseudorandom, uniformly distributed int value between 0
   *  (inclusive) and the specified value (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def nextInt(n: Int): Int = self.nextInt(n)

  /** Returns a pseudorandom, uniformly distributed int value between min
   *  (inclusive) and the specified value max (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def between(minInclusive: Int, maxExclusive: Int): Int = {
    require(minInclusive < maxExclusive, "Invalid bounds")

    val difference = maxExclusive - minInclusive
    if (difference >= 0) {
      nextInt(difference) + minInclusive
    } else {
      /* The interval size here is greater than Int.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      def loop(): Int = {
        val n = nextInt()
        if (n >= minInclusive && n < maxExclusive) n
        else loop()
      }
      loop()
    }
  }

  /** Returns the next pseudorandom, uniformly distributed long value
   *  from this random number generator's sequence.
   */
  def nextLong(): Long = self.nextLong()

  /** Returns a pseudorandom, uniformly distributed long value between 0
   *  (inclusive) and the specified value (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def nextLong(n: Long): Long = {
    require(n > 0, "n must be positive")

    /*
     * Divide n by two until small enough for nextInt. On each
     * iteration (at most 31 of them but usually much less),
     * randomly choose both whether to include high bit in result
     * (offset) and whether to continue with the lower vs upper
     * half (which makes a difference only if odd).
     */

    var offset = 0L
    var _n = n

    while (_n >= Integer.MAX_VALUE) {
      val bits = nextInt(2)
      val halfn = _n >>> 1
      val nextn =
        if ((bits & 2) == 0) halfn
        else _n - halfn
      if ((bits & 1) == 0)
        offset += _n - nextn
      _n = nextn
    }
    offset + nextInt(_n.toInt)
  }

  /** Returns a pseudorandom, uniformly distributed long value between min
   *  (inclusive) and the specified value max (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def between(minInclusive: Long, maxExclusive: Long): Long = {
    require(minInclusive < maxExclusive, "Invalid bounds")

    val difference = maxExclusive - minInclusive
    if (difference >= 0) {
      nextLong(difference) + minInclusive
    } else {
      /* The interval size here is greater than Long.MaxValue,
       * so the loop will exit with a probability of at least 1/2.
       */
      def loop(): Long = {
        val n = nextLong()
        if (n >= minInclusive && n < maxExclusive) n
        else loop()
      }
      loop()
    }
  }

  /** Returns a pseudorandomly generated String.  This routine does
   *  not take any measures to preserve the randomness of the distribution
   *  in the face of factors like unicode's variable-length encoding,
   *  so please don't use this for anything important.  It's primarily
   *  intended for generating test data.
   *
   *  @param  length    the desired length of the String
   *  @return           the String
   */
  def nextString(length: Int) = {
    def safeChar() = {
      val surrogateStart: Int = 0xD800
      val res = nextInt(surrogateStart - 1) + 1
      res.toChar
    }

    List.fill(length)(safeChar()).mkString
  }

  /** Returns the next pseudorandom, uniformly distributed value
   *  from the ASCII range 33-126.
   */
  def nextPrintableChar(): Char = {
    val low  = 33
    val high = 127
    (self.nextInt(high - low) + low).toChar
  }

  def setSeed(seed: Long) { self.setSeed(seed) }

  /** Returns a new collection of the same type in a randomly chosen order.
   *
   *  @return         the shuffled collection
   */
  def shuffle[T, CC[X] <: IterableOnce[X]](xs: CC[T])(implicit bf: BuildFrom[CC[T], T, CC[T]]): CC[T] = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = nextInt(n)
      swap(n - 1, k)
    }

    (bf.newBuilder(xs) ++= buf).result()
  }

  /** Returns a LazyList of pseudorandomly chosen alphanumeric characters,
   *  equally chosen from A-Z, a-z, and 0-9.
   *
   *  @since 2.8
   */
  @migration("`alphanumeric` returns a LazyList instead of a Stream", "2.13.0")
  def alphanumeric: LazyList[Char] = {
    def nextAlphaNum: Char = {
      val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      chars charAt (self nextInt chars.length)
    }

    LazyList continually nextAlphaNum
  }

}

/** The object `Random` offers a default implementation
 *  of scala.util.Random and random-related convenience methods.
 *
 *  @since 2.8
 */
object Random extends Random {

  implicit def javaRandomToRandom(r: java.util.Random): Random = new Random(r)

}
