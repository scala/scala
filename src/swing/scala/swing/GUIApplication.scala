/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import event.Event
import javax.swing._

/**
 * Convenience class with utility methods for GUI applications.
 */
@deprecated("Use SwingApplication instead") class GUIApplication {

  /**
   * Called before the GUI is created. Override to customize.
   */
  def init() {}

  /**
   * Initializes the framework and runs the given program.
   */
  def run(prog: => Unit) = Swing.onEDT { init(); prog }
}
