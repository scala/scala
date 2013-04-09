/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors

/**
 * A common interface for all schedulers used to execute actor tasks.
 *
 * Subclasses of `Actor` that override its `scheduler` member must provide
 * an `IScheduler` implementation.
 *
 * @author Philipp Haller
 */
@deprecated("Use the akka.actor package instead. For migration from the scala.actors package refer to the Actors Migration Guide.", "2.11.0")
trait IScheduler {

  /** Submits a closure for execution.
   *
   *  @param  fun  the closure to be executed
   */
  def execute(fun: => Unit): Unit

  /** Submits a `Runnable` for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit

  def executeFromActor(task: Runnable): Unit =
    execute(task)

  /** Shuts down the scheduler. */
  def shutdown(): Unit

  /** When the scheduler is active, it can execute tasks.
   *
   * @return `'''true'''`, if the scheduler is active, otherwise false.
   */
  def isActive: Boolean

  /** Registers a newly created actor with this scheduler.
   *
   *  @param  a  the actor to be registered
   */
  def newActor(a: TrackedReactor): Unit

  /** Unregisters an actor from this scheduler, because it
   *  has terminated.
   *
   *  @param  a  the actor to be registered
   */
  def terminated(a: TrackedReactor): Unit

  /** Registers a closure to be executed when the specified
   *  actor terminates.
   *
   *  @param  a  the actor
   *  @param  f  the closure to be registered
   */
  def onTerminate(a: TrackedReactor)(f: => Unit): Unit

  def managedBlock(blocker: scala.concurrent.ManagedBlocker): Unit

}
