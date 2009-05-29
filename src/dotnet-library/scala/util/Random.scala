/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Random.scala 14498 2008-04-04 12:12:27Z washburn $


package scala.util

/**
 *  @author Stephane Micheloud
 */
class Random(self0: System.Random) {
  private var rnd = self0 // see setSeed(seed)
  def self = rnd

  /** Creates a new random number generator using a single long seed. */
  def this(seed: Long) = this(new System.Random(seed.toInt))

  /** Creates a new random number generator using a single integer seed. */
  def this(seed: Int) = this(new System.Random(seed))

  /** Creates a new random number generator. */
  def this() = this(new System.Random(System.Environment.TickCount))

  /** Returns the next pseudorandom, uniformly distributed boolean value
   *  from this random number generator's sequence.
   */
  def nextBoolean(): Boolean = (nextInt() & 0x1) == 1

  /** Generates random bytes and places them into a user-supplied byte
   *  array.
   */
  def nextBytes(bytes: Array[Byte]) { rnd.NextBytes(bytes) }

  /** Returns the next pseudorandom, uniformly distributed double value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextDouble(): Double = rnd.NextDouble()

  /** Returns the next pseudorandom, uniformly distributed float value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextFloat(): Float = nextDouble().toFloat

  /** Returns the next pseudorandom, Gaussian ("normally") distributed
   *  double value with mean 0.0 and standard deviation 1.0 from this
   *  random number generator's sequence.
   *  This implements the Box-Muller transformation on the pseudorandom
   *  `nextDouble` method of this class, and should be equivalent to
   *  the implementation of `nextGaussian` in the Java API.
   */
  def nextGaussian(): Double = synchronized {
    if (nextGaussianAvailable) {
      nextGaussianAvailable = false
      nextGaussianCache
    } else {
      var u: Double = 0.0
      var v: Double = 0.0
      var s: Double = 0.0
      do {
        u = 2 * nextDouble() - 1   // [-1, +1]
        v = 2 * nextDouble() - 1   // [-1, +1]
        s = Math.pow(u, 2) + Math.pow(v, 2)
      } while (s >= 1 || s == 0);
      val mul = Math.sqrt((-2 * Math.log(s)) / s)
      nextGaussianCache = u * mul
      nextGaussianAvailable = true
      v * mul
    }
  }

  private var nextGaussianAvailable: Boolean = false
  private var nextGaussianCache: Double = _

  /** Returns the next pseudorandom, uniformly distributed int value
   *  from this random number generator's sequence.
   */
  def nextInt(): Int = rnd.Next()

  /** Returns a pseudorandom, uniformly distributed int value between 0
   *  (inclusive) and the specified value (exclusive), drawn from this
   *  random number generator's sequence.
   */
  def nextInt(n: Int): Int = rnd.Next(0, n)

  /** Returns the next pseudorandom, uniformly distributed long value
   *  from this random number generator's sequence.
   */
  def nextLong(): Long = nextInt().toLong // 2x nextInt() ?!

  def setSeed(seed: Long) { rnd = new System.Random(seed.toInt) }

}
