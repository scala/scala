/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.concurrent

import scala.concurrent.util.Duration

/** A service for scheduling tasks and thunks for one-time, or periodic execution.
 */
trait Scheduler {

  /** Schedules a thunk for repeated execution with an initial delay and a frequency.
   *
   *  @param delay      the initial delay after which the thunk should be executed
   *                    the first time
   *  @param frequency  the frequency with which the thunk should be executed,
   *                    as a time period between subsequent executions
   */
  def schedule(delay: Duration, frequency: Duration)(thunk: => Unit): Cancellable

  /** Schedules a task for execution after a given delay.
   *
   *  @param delay  the duration after which the task should be executed
   *  @param task   the task that is scheduled for execution
   *  @return       a `Cancellable` that may be used to cancel the execution
   *                of the task
   */
  def scheduleOnce(delay: Duration, task: Runnable): Cancellable

  /** Schedules a thunk for execution after a given delay.
   *
   *  @param delay  the duration after which the thunk should be executed
   *  @param task   the thunk that is scheduled for execution
   *  @return       a `Cancellable` that may be used to cancel the execution
   *                of the thunk
   */
  def scheduleOnce(delay: Duration)(task: => Unit): Cancellable

}



trait Cancellable {

  /** Cancels the underlying task.
   */
  def cancel(): Unit

}
