/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.scheduler

import java.util.concurrent.ThreadPoolExecutor
import scala.actors.Debug
import scala.concurrent.ManagedBlocker

/**
 * The <code>ThreadPoolScheduler</code> class uses a
 * <code>ThreadPoolExecutor</code> to execute <code>Actor</code>s.
 *
 * A <code>ThreadPoolScheduler</code> attempts to shut down
 * the underlying <code>ThreadPoolExecutor</code> only if
 * <code>terminate</code> is set to true.
 *
 * Otherwise, the <code>ThreadPoolExecutor</code> must be shut down
 * either directly or by shutting down the
 * <code>ThreadPoolScheduler</code> instance.
 *
 * @author Philipp Haller
 */
class ThreadPoolScheduler(protected var executor: ThreadPoolExecutor,
                          protected val terminate: Boolean,
                          protected val daemon: Boolean)
  extends Thread with ExecutorScheduler with TerminationMonitor {

  setDaemon(daemon)

  private var terminating = false // guarded by this
  protected val CHECK_FREQ = 10

  /* This constructor (and the var above) is currently only used to work
   * around a bug in scaladoc, which cannot deal with early initializers
   * (to be used in subclasses such as DefaultThreadPoolScheduler)
   * properly.
   */
  def this(d: Boolean) {
    this(null, true, d)
  }

  override def run() {
    try {
      while (true) {
        this.synchronized {
          try {
            wait(CHECK_FREQ)
          } catch {
            case _: InterruptedException =>
          }

          if (terminating || (terminate && allTerminated))
            throw new QuitException

          gc()
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
