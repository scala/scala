/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors
package scheduler

import java.util.concurrent.Callable
import scala.concurrent.ThreadPoolRunner

/**
 * The <code>ExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s.
 *
 * @author Philipp Haller
 */
private[scheduler] trait ExecutorScheduler extends IScheduler with ThreadPoolRunner {

  def execute(task: Runnable) {
    super[ThreadPoolRunner].execute(task.asInstanceOf[Task[Unit]])
  }

  private class RunCallable(fun: => Unit) extends Callable[Unit] with Runnable {
    def call() { fun }
    def run() { fun }
  }

  /** Submits a closure for execution.
   *
   *  @param  fun  the closure to be executed
   */
  override def execute(fun: => Unit) {
    super[ThreadPoolRunner].execute((new RunCallable(fun)).asInstanceOf[Task[Unit]])
  }

  /** This method is called when the scheduler shuts down.
   */
  def onShutdown(): Unit =
    executor.shutdown()

  /** The scheduler is active if the underlying <code>ExecutorService</code>
   *  has not been shut down.
   */
  def isActive =
    (executor ne null) && !executor.isShutdown

}
