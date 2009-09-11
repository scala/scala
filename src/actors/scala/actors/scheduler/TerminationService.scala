/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.actors
package scheduler

import java.lang.{Runnable, Thread, InterruptedException}

/**
 * The <code>TerminationService</code> class starts a new thread
 * that is used to check regularly if the scheduler can be
 * shut down, because all started actors are known to
 * have terminated.
 *
 * @author Philipp Haller
 */
abstract class TerminationService(terminate: Boolean)
  extends Thread with IScheduler with TerminationMonitor {

  private var terminating = false

  protected val CHECK_FREQ = 50

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

          if (terminate && allTerminated)
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
