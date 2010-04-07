/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

/**
 * The <code>IScheduler</code> trait provides a common interface
 * for all schedulers used to execute actor tasks.
 *
 * Subclasses of <code>Actor</code> that override its
 * <code>scheduler</code> member must provide
 * an <code>IScheduler</code> implementation.
 *
 * @author Philipp Haller
 */
trait IScheduler {

  /** Submits a closure for execution.
   *
   *  @param  fun  the closure to be executed
   */
  def execute(fun: => Unit): Unit

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit

  def executeFromActor(task: Runnable): Unit =
    execute(task)

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit

  /** When the scheduler is active, it can execute tasks.
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

  @deprecated("this member is going to be removed in a future release")
  def tick(a: Actor) {}

  @deprecated("this member is going to be removed in a future release")
  def onLockup(handler: () => Unit) {}

  @deprecated("this member is going to be removed in a future release")
  def onLockup(millis: Int)(handler: () => Unit) {}

  @deprecated("this member is going to be removed in a future release")
  def printActorDump {}

}
