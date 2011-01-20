/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import scala.actors._

// Dummy to keep ant from recompiling on every run.
trait SwingActor { }

/*object SwingActor {
  /**
   * Similar to Actor.actor, but creates an instance of a SwingActor.
   */
  def apply(body: => Unit): Actor =
    new SwingActor { def act() = body }.start()
}

/**
 * An actor that runs on the Swing event dispatching thread (EDT).
 */
abstract class SwingActor extends Actor {
  override val scheduler = new SchedulerAdapter {
    def execute(op: =>Unit) = Swing onEDT op
    def onTerminate(a: Actor)(op: => Unit) {}
    def terminated(a: Actor) {}
  }
}*/
