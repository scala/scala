/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors
package scheduler

import scala.concurrent.ThreadPoolRunner

/**
 * The <code>ExecutorScheduler</code> class uses an
 * <code>ExecutorService</code> to execute <code>Actor</code>s.
 *
 * @author Philipp Haller
 */
trait ExecutorScheduler extends IScheduler with ThreadPoolRunner[Unit] {

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
