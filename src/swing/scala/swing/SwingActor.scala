package scala.swing

import scala.actors._

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
