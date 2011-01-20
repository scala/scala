/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package scheduler

import java.lang.{Thread, InterruptedException}

/**
 * The <code>TerminationService</code> class starts a new thread
 * that is used to check regularly if the scheduler can be
 * shut down, because all started actors are known to
 * have terminated.
 *
 * @author Philipp Haller
 */
private[scheduler] trait TerminationService extends TerminationMonitor {
  _: Thread with IScheduler =>

  private var terminating = false

  /** Indicates whether the scheduler should terminate when all
   *  actors have terminated.
   */
  protected val terminate = true

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

          if (terminating || (terminate && allActorsTerminated))
            throw new QuitControl

          gc()
        }
      }
    } catch {
      case _: QuitControl =>
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
