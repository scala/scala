/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package util

import scala.annotation.{migration, tailrec}
import scala.collection.mutable.ArrayBuffer
import scala.collection.BuildFrom
import scala.collection.immutable.LazyList
import scala.language.implicitConversions

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
  def nextBytes(bytes: Array[Byte]): Unit = { self.nextBytes(bytes) }
  
  /** Generates `n` random bytes and returns them in a new array. */
  def nextBytes(n: Int): Array[Byte] = {
    val bytes = new Array[Byte](0 max n)
    self.nextBytes(bytes)
    bytes
  }
  
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
      @tailrec
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
      @tailrec
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
  def nextString(length: Int): String = {
    def safeChar(): Char = {
      val surrogateStart: Int = 0xD800
      val res = nextInt(surrogateStart - 1) + 1
      res.toChar
    }
    if (length <= 0) {
      ""
    } else {
      val arr = new Array[Char](length)
      var i = 0
      while (i < length) {
        arr(i) = safeChar()
        i += 1
      }
      new String(arr)
    }
  }

  /** Returns the next pseudorandom, uniformly distributed value
   *  from the ASCII range 33-126.
   */
  def nextPrintableChar(): Char = {
    val low  = 33
    val high = 127
    (self.nextInt(high - low) + low).toChar
  }

  def setSeed(seed: Long): Unit = { self.setSeed(seed) }

  /** Returns a new collection of the same type in a randomly chosen order.
   *
   *  @return         the shuffled collection
   */
  def shuffle[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): C = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int): Unit = {
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
 */
object Random extends Random {

  implicit def javaRandomToRandom(r: java.util.Random): Random = new Random(r)

}
