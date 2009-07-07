/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


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
