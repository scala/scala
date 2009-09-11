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

import java.util.concurrent.ThreadPoolExecutor
import scala.concurrent.ManagedBlocker

/**
 * The <code>ThreadPoolScheduler</code> class uses an
 * <code>ThreadPoolExecutor</code> to execute <code>Actor</code>s.
 *
 * A <code>ThreadPoolScheduler</code> attempts to shut down
 * the underlying <code>ExecutorService</code> only if
 * <code>terminate</code> is set to true.
 *
 * Otherwise, the <code>ExecutorService</code> must be shut down either
 * directly or by shutting down the
 * <code>ThreadPoolScheduler</code> instance.
 *
 * @author Philipp Haller
 */
class ThreadPoolScheduler(protected var executor: ThreadPoolExecutor,
                          protected var terminate: Boolean)
  extends Thread with TerminationMonitor with ExecutorScheduler {

  private var terminating = false
  protected val CHECK_FREQ = 10

  /* This constructor (and the var above) is currently only used to work
   * around a bug in scaladoc, which cannot deal with early initializers
   * (to be used in subclasses such as DefaultExecutorScheduler) properly.
   */
  def this() {
    this(null, true)
  }

  override def managedBlock(blocker: ManagedBlocker) {
    val coreSize = executor.getCorePoolSize()
    if (coreSize < ThreadPoolConfig.maxPoolSize && (executor.getActiveCount() >= coreSize - 1)) {
      executor.setCorePoolSize(coreSize + 1)
    }
    blocker.block()
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

          if (terminating)
            throw new QuitException

          if (terminate && allTerminated)
            throw new QuitException

          val coreSize = executor.getCorePoolSize()
          if (coreSize < ThreadPoolConfig.maxPoolSize && (executor.getActiveCount() >= coreSize - 1)) {
            executor.setCorePoolSize(coreSize + 1)
          }
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
