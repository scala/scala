package scala.swing

import javax.swing._
import event.Event

/**
 * Convenience class with utility methods for GUI applications.
 */
class GUIApplication {

  /**
   * Called before the GUI is created. Override to customize.
   */
  def init() {}

  /**
   * Initializes the framework and runs the given program.
   */
  def run(prog: => Unit) = Swing.onEDT { init(); prog }
}
