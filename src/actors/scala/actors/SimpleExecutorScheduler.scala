/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import scala.collection.mutable.HashMap
import java.util.concurrent.{ExecutorService, RejectedExecutionException}

/**
 * The <code>SimpleExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s. It
 * does not start an additional thread. Also, the underlying
 * <code>ExecutorService</code> is not shut down automatically;
 * instead, the <code>ExecutorService</code> must be shut down either
 * directly or by shutting down the
 * <code>SimpleExecutorScheduler</code> instance.
 *
 * @author Philipp Haller
 */
class SimpleExecutorScheduler(protected var executor: ExecutorService) extends IScheduler {

  /* Maintains at most one closure per actor that is executed
   * when the actor terminates.
   */
  protected val termHandlers = new HashMap[Actor, () => Unit]

  /* This constructor (and the var above) is currently only used to work
   * around a bug in scaladoc, which cannot deal with early initializers
   * (to be used in subclasses such as DefaultExecutorScheduler) properly.
   */
  def this() {
    this(null)
  }

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable) {
    try {
      executor execute task
    } catch {
      case ree: RejectedExecutionException =>
        // run task on current thread
        task.run()
    }
  }

  /** Submits a closure for execution.
   *
   *  @param  block  the closure to be executed
   */
  def execute(block: => Unit) {
    val task = new Runnable {
      def run() { block }
    }
    execute(task)
  }

  /** Shuts down the scheduler.
   */
  def shutdown() {
    executor.shutdown()
  }

  /** The scheduler is active if the underlying <code>ExecutorService</code>
   *  has not been shut down.
   */
  def isActive =
    (executor ne null) && !executor.isShutdown()

  def newActor(a: Actor) {}

  def terminated(a: Actor) {
    // obtain termination handler (if any)
    val todo = synchronized {
      termHandlers.get(a) match {
        case Some(handler) =>
          termHandlers -= a
          () => handler
        case None =>
          () => { /* do nothing */ }
      }
    }

    // invoke termination handler (if any)
    todo()
  }

  /** Registers a closure to be executed when the specified
   *  actor terminates.
   *
   *  @param  a  the actor
   *  @param  f  the closure to be registered
   */
  def onTerminate(a: Actor)(block: => Unit) = synchronized {
    termHandlers += (a -> (() => block))
  }
}
