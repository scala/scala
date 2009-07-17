package scala.actors

import java.lang.Thread.State
import forkjoin._

class ForkJoinScheduler extends Thread with IScheduler with TerminationMonitor {

  private val pool = {
    val p = new ForkJoinPool()
    // enable locally FIFO scheduling mode
    p.setAsyncMode(true)
    Debug.info(this+": parallelism "+p.getParallelism())
    Debug.info(this+": max pool size "+p.getMaximumPoolSize())
    p
  }

  private var terminating = false

  private val CHECK_FREQ = 50

  override def managedBlock(blocker: ManagedBlocker) {
    ForkJoinPool.managedBlock(blocker, true)
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

          if (allTerminated) {
            //Debug.info(this+": all actors terminated")
            throw new QuitException
          }
        }
      }
    } catch {
      case _: QuitException =>
        Debug.info(this+": initiating shutdown...")
        while (!pool.isQuiescent()) {
          try {
            Thread.sleep(10)
          } catch {
            case ignore: InterruptedException =>
          }
        }
        pool.shutdown()
        // allow thread to exit
    }
  }

  def execute(task: Runnable) {
    pool.execute(task)
  }

  def executeFromActor(task: Runnable) {
    val recAction = new RecursiveAction {
      def compute() = task.run()
    }
    recAction.fork()
  }

  /** Submits a closure for execution.
   *
   *  @param  fun  the closure to be executed
   */
  def execute(fun: => Unit): Unit =
    execute(new Runnable {
      def run() { fun }
    })

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit = synchronized {
    terminating = true
  }

  def isActive =
    !pool.isShutdown()

}
