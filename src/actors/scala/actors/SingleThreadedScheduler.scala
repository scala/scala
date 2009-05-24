/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors

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

  def newActor(actor: Actor) {}
  def terminated(actor: Actor) {}
  def onTerminate(actor: Actor)(f: => Unit) {}

  def isActive = true
}
