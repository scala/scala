/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
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
 * an implementation of the <code>IScheduler</code>
 * trait.
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
  def newActor(a: OutputChannelActor): Unit

  /** Unregisters an actor from this scheduler, because it
   *  has terminated.
   *
   *  @param  a  the actor to be registered
   */
  def terminated(a: OutputChannelActor): Unit

  /** Registers a closure to be executed when the specified
   *  actor terminates.
   *
   *  @param  a  the actor
   *  @param  f  the closure to be registered
   */
  def onTerminate(a: OutputChannelActor)(f: => Unit): Unit
}
