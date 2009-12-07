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

import scala.util.control.ControlException
import java.lang.{Runnable, Thread, InterruptedException}

/**
 * The abstract <code>SchedulerService</code> class allows subclasses
 * to implement a custom <code>onShutdown</code> method, which is
 * invoked when the runtime system has detected that all actors have
 * been terminated.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
abstract class SchedulerService(daemon: Boolean) extends Thread with IScheduler with ActorGC {

  setDaemon(daemon)

  def this() =
    this(false)

  private var terminating = false

  protected val CHECK_FREQ = 100

  def onShutdown(): Unit

  override def run() {
    try {
      while (true) {
        this.synchronized {
          try {
            wait(CHECK_FREQ)
          } catch {
            case _: InterruptedException =>
          }
          if (terminating)
            throw new QuitException

          gc()

          if (allTerminated)
            throw new QuitException
        }
      }
    } catch {
      case _: QuitException =>
        Debug.info(this+": initiating shutdown...")
        // invoke shutdown hook
        onShutdown()
        // allow thread to exit
    }
  }

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }
}

/**
 * The <code>QuitException</code> class is used to manage control flow
 * of certain schedulers and worker threads.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
private[actors] class QuitException extends Throwable with ControlException
