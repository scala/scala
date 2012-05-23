/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

import scala.concurrent.util.Duration
import java.util.concurrent.TimeUnit

case class Timeout(duration: Duration) {
  def this(timeout: Long) = this(Duration(timeout, TimeUnit.MILLISECONDS))
  def this(length: Long, unit: TimeUnit) = this(Duration(length, unit))
}

object Timeout {

  /**
   * A timeout with zero duration, will cause most requests to always timeout.
   */
  val zero = new Timeout(Duration.Zero)

  /**
   * A Timeout with infinite duration. Will never timeout. Use extreme caution with this
   * as it may cause memory leaks, blocked threads, or may not even be supported by
   * the receiver, which would result in an exception.
   */
  val never = new Timeout(Duration.Inf)

  def apply(timeout: Long) = new Timeout(timeout)
  def apply(length: Long, unit: TimeUnit) = new Timeout(length, unit)

  implicit def durationToTimeout(duration: Duration) = new Timeout(duration)
  implicit def intToTimeout(timeout: Int) = new Timeout(timeout)
  implicit def longToTimeout(timeout: Long) = new Timeout(timeout)
}
