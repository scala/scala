/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import java.util.concurrent.ExecutorService

/**
 * The <code>ExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s.
 *
 * @author Philipp Haller
 */
class ExecutorScheduler(executor: ExecutorService) extends SchedulerService {

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit =
    executor execute task

  /** This method is called when the <code>SchedulerService</code>
   *  shuts down.
   */
  def onShutdown(): Unit =
    executor.shutdown()
}
