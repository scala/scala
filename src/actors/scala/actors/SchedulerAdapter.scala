/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors

/** The <code>SchedulerAdapter</code> trait is used to adapt
 *  the behavior of the standard <code>Scheduler</code> object.
 *
 *  Providing an implementation for the
 *  <code>execute(f: => Unit)</code> method is sufficient to
 *  obtain a concrete <code>IScheduler</code> class.
 *
 *  @version 0.9.18
 *  @author Philipp Haller
 */
trait SchedulerAdapter extends IScheduler {

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit =
    execute { task.run() }

  /** Notifies the scheduler about activity of the
   *  executing actor.
   *
   *  @param  a  the active actor
   */
  def tick(a: Actor): Unit =
    Scheduler tick a

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit =
    Scheduler.shutdown()

  def onLockup(handler: () => Unit) {}

  def onLockup(millis: Int)(handler: () => Unit) {}

  def printActorDump {}

}
