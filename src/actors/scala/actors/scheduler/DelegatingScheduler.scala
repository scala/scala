/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.actors
package scheduler

import scala.concurrent.ManagedBlocker

/**
 * @author Erik Engbrecht
 */
private[actors] trait DelegatingScheduler extends IScheduler {
  protected def makeNewScheduler(): IScheduler

  protected var sched: IScheduler = null

  final def impl = synchronized {
    if ((sched eq null) || (!sched.isActive))
      sched = makeNewScheduler()
    sched
  }

  final def impl_= (scheduler: IScheduler): Unit = synchronized {
    //TODO: if there is already a scheduler, should it be shutdown?
    sched = scheduler
  }

  /**
   * Always active because it will just make a new scheduler if required
   */
  def isActive: Boolean = true

  def execute(fun: => Unit) = impl.execute(fun)

  def execute(task: Runnable) = impl.execute(task)

  override def executeFromActor(task: Runnable) = impl.executeFromActor(task)

  def shutdown(): Unit = synchronized {
    if (sched ne null) {
      sched.shutdown()
      sched = null
    }
  }

  def newActor(actor: TrackedReactor) = synchronized {
    val createNew = if (sched eq null)
      true
    else sched.synchronized {
      if (!sched.isActive)
        true
      else {
        sched.newActor(actor)
        false
      }
    }
    if (createNew) {
      sched = makeNewScheduler()
      sched.newActor(actor)
    }
  }

  def terminated(actor: TrackedReactor) = impl.terminated(actor)

  def onTerminate(actor: TrackedReactor)(f: => Unit) = impl.onTerminate(actor)(f)

  override def managedBlock(blocker: ManagedBlocker): Unit =
    impl.managedBlock(blocker)
}
