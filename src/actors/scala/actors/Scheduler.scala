/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

import compat.Platform

import java.lang.{Runnable, Thread, InterruptedException}

import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer, Buffer, HashMap, Queue, Stack, HashSet}

/**
 * The <code>Scheduler</code> object is used by
 * <code>Actor</code> to execute tasks of an execution of an actor.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
object Scheduler extends IScheduler {

  Debug.info("initializing "+this+"...")

  private var sched: IScheduler = {
    val s = new FJTaskScheduler2
    s.start()
    s
  }

  def impl = sched
  def impl_= (scheduler: IScheduler) = {
    sched = scheduler
  }

  private var tasks: LinkedQueue = null
  private var pendingCount = 0

  /* Assumes <code>sched</code> holds an instance
   * of <code>FJTaskScheduler2</code>.
   */
  def snapshot(): Unit =
    if (sched.isInstanceOf[FJTaskScheduler2]) {
      val fjts = sched.asInstanceOf[FJTaskScheduler2]
      tasks = fjts.snapshot()
      pendingCount = actorGC.getPendingCount
      fjts.shutdown()
    } else
      error("snapshot operation not supported.")

  /* Creates an instance of class <code>FJTaskScheduler2</code>
   * and submits <code>tasks</code> for execution.
   */
  def restart(): Unit = synchronized {
    sched = {
      val s = new FJTaskScheduler2
      actorGC.setPendingCount(pendingCount)
      s.start()
      s
    }
    //Actor.timer = new java.util.Timer
    while (!tasks.isEmpty()) {
      sched.execute(tasks.take().asInstanceOf[FJTask])
    }
    tasks = null
  }

  def execute(task: Runnable) {
    val t = currentThread
    if (t.isInstanceOf[FJTaskRunner]) {
      val tr = t.asInstanceOf[FJTaskRunner]
      tr.push(new FJTask {
        def run() { task.run() }
      })
    } else
      sched execute task
  }

  def execute(fun: => Unit) {
    val t = currentThread
    if (t.isInstanceOf[FJTaskRunner]) {
      val tr = t.asInstanceOf[FJTaskRunner]
      tr.push(new FJTask {
        def run() { fun }
      })
    } else
      sched execute { fun }
  }

  def shutdown() = sched.shutdown()

  /** The <code>ActorGC</code> instance that keeps track of the
   *  live actor objects that are managed by <code>this</code>
   *  scheduler.
   */
  def actorGC: ActorGC = sched.actorGC

  def onLockup(handler: () => Unit) = sched.onLockup(handler)
  def onLockup(millis: Int)(handler: () => Unit) = sched.onLockup(millis)(handler)
  def printActorDump = sched.printActorDump
}


/**
 * The <code>IScheduler</code> trait provides a common interface
 * for all schedulers used to execute actor tasks.
 *
 * Subclasses of <code>Actor</code> that override its
 * <code>scheduler</code> member value must provide
 * an implementation of the <code>IScheduler</code>
 * trait.
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
trait IScheduler {

  /** Submits a closure for execution.
   *
   *  @param  fun  the closure to be executed
   */
  def execute(fun: => Unit): Unit

  /** Submits a <code>Runnable</code> for execution.
   *
   *  @param  task  the task to be executed
   */
  def execute(task: Runnable): Unit

  /** Shuts down the scheduler.
   */
  def shutdown(): Unit

  /** The <code>ActorGC</code> instance that keeps track of the
   *  live actor objects that are managed by <code>this</code>
   *  <code>IScheduler</code> instance.
   */
  def actorGC: ActorGC

  def onLockup(handler: () => Unit): Unit
  def onLockup(millis: Int)(handler: () => Unit): Unit
  def printActorDump: Unit

  val QUIT_TASK = new Reaction(null) {
    override def run(): Unit = {}
    override def toString() = "QUIT_TASK"
  }
}


/**
 * This scheduler executes the tasks of an actor on a single
 * thread (the current thread).
 *
 * @version 0.9.18
 * @author Philipp Haller
 */
class SingleThreadedScheduler extends IScheduler {

  def execute(task: Runnable) {
    task.run()
  }

  def execute(fun: => Unit): Unit =
    execute(new Runnable {
      def run() { fun }
    })

  def shutdown() {}

  /** The <code>ActorGC</code> instance that keeps track of the
   *  live actor objects that are managed by <code>this</code>
   *  scheduler.
   */
  val actorGC: ActorGC = new ActorGC

  def onLockup(handler: () => Unit) {}
  def onLockup(millis: Int)(handler: () => Unit) {}
  def printActorDump {}
}


/**
 * The <code>QuitException</code> class is used to manage control flow
 * of certain schedulers and worker threads.
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
private[actors] class QuitException extends Throwable {
  /*
   For efficiency reasons we do not fill in
   the execution stack trace.
   */
  override def fillInStackTrace(): Throwable = this
}
