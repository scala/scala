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
 *  obtain a concrete class that extends <code>IScheduler</code>.
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

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit =
    Scheduler.shutdown()

  /** The <code>ActorGC</code> instance that keeps track of the
   *  live actor objects that are managed by <code>this</code>
   *  scheduler.
   */
  val actorGC: ActorGC = new ActorGC

  def onLockup(handler: () => Unit) {}

  def onLockup(millis: Int)(handler: () => Unit) {}

  def printActorDump {}

}
