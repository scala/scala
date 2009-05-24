/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.concurrent.{ExecutorService, RejectedExecutionException}

/**
 * The <code>ExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s.
 *
 * @author Philipp Haller
 */
class ExecutorScheduler(var executor: ExecutorService) extends SchedulerService {

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
        Debug.info("caught "+ree)
    }
  }

  /** This method is called when the <code>SchedulerService</code>
   *  shuts down.
   */
  def onShutdown(): Unit =
    executor.shutdown()
}
