/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util

/**
 *  @author Stephane Micheloud
 *
 */
class Random(val self: java.util.Random) {

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

  /** Returns the next pseudorandom, uniformly distributed float value
   *  between 0.0 and 1.0 from this random number generator's sequence.
   */
  def nextFloat(): Float = self.nextFloat()

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

  /** Returns the next pseudorandom, uniformly distributed long value
   *  from this random number generator's sequence.
   */
  def nextLong(): Long = self.nextLong()

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

  /** Returns a pseudorandomly generated String drawing upon
   *  only ASCII characters between 33 and 126.
   */
  def nextASCIIString(length: Int) = {
    val (min, max) = (33, 126)
    def nextDigit = nextInt(max - min) + min

    new String(Array.fill(length)(nextDigit.toByte), "ASCII")
  }

  def setSeed(seed: Long) { self.setSeed(seed) }
}

/** The object <code>Random</code> offers a default implementation
 *  of scala.util.Random and random-related convenience methods.
 *
 *  @since 2.8
 */
object Random extends Random
{
  import collection.Seq

  /** Returns a new sequence in random order.
   *  @param  seq   the sequence to shuffle
   *  @return       the shuffled sequence
   */
  def shuffle[T: ClassManifest](seq: Seq[T]): Seq[T] = {
    // It would be better if this preserved the shape of its container, but I have
    // again been defeated by the lack of higher-kinded type inference.  I can
    // only make it work that way if it's called like
    //   shuffle[Int,List](List.range(0,100))
    // which nicely defeats the "convenience" portion of "convenience method".
    val buf = seq.toArray

    def swap(i1: Int, i2: Int) {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = nextInt(n)
      swap(n - 1, k)
    }

    buf.toSeq
  }

  /** I was consumed by weeping when I discovered how easy this
   *  is to implement in SeqLike rather than trying to
   *  accomplish the inference from the outside.  For reference
   *  here is the shape-preserving implementation.
   */
  // def shuffle: This = {
  //   import scala.util.Random.nextInt
  //   val buf = thisCollection.toIndexedSeq
  //
  //   def swap(i1: Int, i2: Int) {
  //     val tmp = buf(i1)
  //     buf(i1) = buf(i2)
  //     buf(i2) = tmp
  //   }
  //
  //   for (n <- buf.length to 2 by -1) {
  //     val k = nextInt(n)
  //     swap(n - 1, k)
  //   }
  //
  //   newBuilder ++= buf result
  // }
}
