package scala.actors

import java.lang.Thread.State
import forkjoin._

class ForkJoinScheduler extends Thread with IScheduler with TerminationMonitor {

  private val pool = {
    val p = new ForkJoinPool()
    Debug.info(this+": parallelism "+p.getParallelism())
    Debug.info(this+": max pool size "+p.getMaximumPoolSize())
    p
  }

  private var terminating = false

  private val CHECK_FREQ = 50

  private def allWorkersWaiting: Boolean =
    pool.workers.forall(t => {
      if (t == null)
        true
      else {
        val s = t.getState()
        s == State.WAITING || s == State.TIMED_WAITING
      }
    })

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

          if (!pool.isQuiescent && allWorkersWaiting) {
            //Debug.info(this+": all workers blocked")
            val par = pool.getParallelism()
            //Debug.info(this+": parallelism "+par)
            //Debug.info(this+": max pool size "+pool.getMaximumPoolSize())
            if (par < pool.getMaximumPoolSize()) {
              pool.setParallelism(par + 1)
            }
          }
        }
      }
    } catch {
      case _: QuitException =>
        Debug.info(this+": initiating shutdown...")
        pool.shutdown()
        // allow thread to exit
    }
  }

  def execute(task: Runnable) {
    val recAction = new RecursiveAction {
      def compute() = task.run()
    }
    val thread = Thread.currentThread()
    if (thread.isInstanceOf[ForkJoinWorkerThread])
      recAction.fork()
    else
      pool.execute(task)
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
