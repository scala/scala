/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.actors
package scheduler

import scala.collection.mutable.Queue

/**
 * This scheduler executes actor tasks on the current thread.
 *
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {

  private val tasks = new Queue[Runnable]

  /** The maximum number of nested tasks that are run
   *  without unwinding the call stack.
   */
  protected val maxNesting = 10

  private var curNest = 0
  private var isShutdown = false

  def execute(task: Runnable) {
    if (curNest < maxNesting) {
      curNest += 1
      task.run()
    } else {
      curNest = 0
      tasks += task
    }
  }

  def execute(fun: => Unit): Unit =
    execute(new Runnable {
      def run() { fun }
    })

  def shutdown() {
    isShutdown = false
    while (!tasks.isEmpty) {
      val task = tasks.dequeue()
      task.run()
    }
    isShutdown = true
  }

  def newActor(actor: TrackedReactor) {}
  def terminated(actor: TrackedReactor) {}

  // TODO: run termination handlers at end of shutdown.
  def onTerminate(actor: TrackedReactor)(f: => Unit) {}

  def isActive =
    !isShutdown

  def managedBlock(blocker: scala.concurrent.ManagedBlocker) {
    blocker.block()
  }
}
