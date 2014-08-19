/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent.duration

/**
 * This class stores a deadline, as obtained via `Deadline.now` or the
 * duration DSL:
 *
 * {{{
 * import scala.concurrent.duration._
 * 3.seconds.fromNow
 * }}}
 *
 * Its main purpose is to manage repeated attempts to achieve something (like
 * awaiting a condition) by offering the methods `hasTimeLeft` and `timeLeft`.  All
 * durations are measured according to `System.nanoTime` aka wall-time; this
 * does not take into account changes to the system clock (such as leap
 * seconds).
 */
case class Deadline private (time: FiniteDuration) extends Ordered[Deadline] {
  /**
   * Return a deadline advanced (i.e., moved into the future) by the given duration.
   */
  def +(other: FiniteDuration): Deadline = copy(time = time + other)
  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  def -(other: FiniteDuration): Deadline = copy(time = time - other)
  /**
   * Calculate time difference between this and the other deadline, where the result is directed (i.e., may be negative).
   */
  def -(other: Deadline): FiniteDuration = time - other.time
  /**
   * Calculate time difference between this duration and now; the result is negative if the deadline has passed.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def timeLeft: FiniteDuration = this - Deadline.now
  /**
   * Determine whether the deadline still lies in the future at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def hasTimeLeft(): Boolean = !isOverdue()
  /**
   * Determine whether the deadline lies in the past at the point where this method is called.
   *
   * '''''Note that on some systems this operation is costly because it entails a system call.'''''
   * Check `System.nanoTime` for your platform.
   */
  def isOverdue(): Boolean = (time.toNanos - System.nanoTime()) < 0
  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  def compare(other: Deadline) = time compare other.time
}

object Deadline {
  /**
   * Construct a deadline due exactly at the point where this method is called. Useful for then
   * advancing it to obtain a future deadline, or for sampling the current time exactly once and
   * then comparing it to multiple deadlines (using subtraction).
   */
  def now: Deadline = Deadline(Duration(System.nanoTime, NANOSECONDS))

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  implicit object DeadlineIsOrdered extends Ordering[Deadline] {
    def compare(a: Deadline, b: Deadline) = a compare b
  }

}
