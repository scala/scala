/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.concurrent.duration

import annotation.nowarn

/** A deadline, as a duration of time remaining, calculated from the present.
 *
 *  Deadlines are obtained via `Deadline.now` or the duration DSL:
 *
 *  {{{
 *  import scala.concurrent.duration._
 *  3.seconds.fromNow
 *  }}}
 *
 *  The purpose of a deadline is to support managed repeated attempts,
 *  such as when awaiting a condition, by offering the methods `hasTimeLeft` and `timeLeft`.
 *
 *  All durations are measured according to `System.nanoTime`;
 *  this does not take into account changes to the system clock, such as leap seconds.
 */
case class Deadline private (time: FiniteDuration) extends Ordered[Deadline] {
  /**
   * Return a deadline advanced (i.e., moved into the future) by the given duration.
   */
  @nowarn("cat=deprecation")
  def +(other: FiniteDuration): Deadline = copy(time = time + other)
  /**
   * Return a deadline moved backwards (i.e., towards the past) by the given duration.
   */
  @nowarn("cat=deprecation")
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
  def compare(other: Deadline): Int = time compare other.time

  // expose public copy for backward compatibility under -Xsource:3
  @deprecated("use now or FiniteDuration#fromNow", since="2.13.13")
  def copy(time: FiniteDuration = this.time): Deadline = Deadline(time)
}

object Deadline {
  /**
   * Construct a deadline due exactly at the point where this method is called. Useful for then
   * advancing it to obtain a future deadline, or for sampling the current time exactly once and
   * then comparing it to multiple deadlines (using subtraction).
   */
  @nowarn("cat=deprecation")
  def now: Deadline = Deadline(Duration(System.nanoTime, NANOSECONDS))

  /**
   * The natural ordering for deadline is determined by the natural order of the underlying (finite) duration.
   */
  implicit object DeadlineIsOrdered extends Ordering[Deadline] {
    def compare(a: Deadline, b: Deadline): Int = a compare b
  }

  // expose public apply for backward compatibility under -Xsource:3
  @deprecated("use now or FiniteDuration#fromNow", since="2.13.13")
  def apply(time: FiniteDuration): Deadline = new Deadline(time)
}
