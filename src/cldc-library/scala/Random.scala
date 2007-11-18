/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** (see http://java.sun.com/javame/reference/apis/jsr030/)
 *
 *  @author Stephane Micheloud
 */
class Random(val self: java.util.Random) {

  /** Creates a new random number generator using a single long seed. */
  def this(seed: Long) = this(new java.util.Random(seed))

  /** Creates a new random number generator using a single integer seed. */
  def this(seed: Int) = this(seed.toLong)

  /** Creates a new random number generator. */
  def this() = this(compat.Platform.currentTime)

  /** Returns the next pseudorandom, uniformly distributed int value
   *  from this random number generator's sequence.
   */
  def nextInt(): Int = self.nextInt()

  /** Returns the next pseudorandom, uniformly distributed long value
   *  from this random number generator's sequence.
   */
  def nextLong(): Long = self.nextLong()

  def setSeed(seed: Long) { self.setSeed(seed) }

}
