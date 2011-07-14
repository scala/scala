/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest

import scala.collection.mutable

trait Statistics {
  /** Only collected when --stats is given. */
  lazy val testStatistics = new mutable.HashMap[String, Long]

  /** Given function and block of code, evaluates code block,
   *  calls function with milliseconds elapsed, and returns block result.
   */
  def timed[T](f: Long => Unit)(body: => T): T = {
    val start = System.currentTimeMillis
    val result = body
    val end = System.currentTimeMillis

    f(end - start)
    result
  }
  /** Times body and returns both values.
   */
  def timed2[T](body: => T): (Long, T) = {
    var milliSeconds = 0L
    val result = timed(x => milliSeconds = x)(body)

    (milliSeconds, result)
  }

  def resultsToStatistics(results: Iterable[(_, Int)]): (Int, Int) =
    (results partition (_._2 == 0)) match {
      case (winners, losers) => (winners.size, losers.size)
    }

  def recordTestTiming(name: String, milliseconds: Long) =
    synchronized { testStatistics(name) = milliseconds }

  def showTestStatistics() {
    testStatistics.toList sortBy (-_._2) foreach { case (k, v) => println("%s: %.2f seconds".format(k, (v.toDouble / 1000))) }
  }
}
