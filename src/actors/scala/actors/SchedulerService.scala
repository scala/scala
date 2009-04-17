/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.actors

import java.lang.{Runnable, Thread, InterruptedException}

/**
 * The abstract <code>SchedulerService</code> class allows
 * subclasses to implement a custom <code>onShutdown</code>
 * method, which is invoked when the runtime system has detected
 * that all actors have been terminated.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
abstract class SchedulerService(daemon: Boolean) extends Thread with IScheduler {
  setDaemon(daemon)

  def this() =
    this(false)

  /** The <code>ActorGC</code> instance that keeps track of the
   *  live actor objects that are managed by <code>this</code>
   *  scheduler.
   */
  val actorGC = new ActorGC

  private var terminating = false

  def printActorDump {}

  protected val CHECK_FREQ = 100

  def onLockup(handler: () => Unit) =
    lockupHandler = handler

  def onLockup(millis: Int)(handler: () => Unit) = {
    //LOCKUP_CHECK_FREQ = millis / CHECK_FREQ
    lockupHandler = handler
  }

  private var lockupHandler: () => Unit = null

  def onShutdown(): Unit

  override def run() {
    try {
      while (!terminating) {
        this.synchronized {
          try {
            wait(CHECK_FREQ)
          } catch {
            case _: InterruptedException =>
              if (terminating) throw new QuitException
          }

          actorGC.gc()

          if (actorGC.allTerminated) {
            throw new QuitException
          }
        } // sync
      } // while (!terminating)
    } catch {
      case _: QuitException =>
        Debug.info(this+": initiating shutdown...")
        // invoke shutdown hook
        onShutdown()
        // allow thread to exit
    }
  }

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit =
    execute { task.run() }

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }

}
