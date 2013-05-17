/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

/**
 * A frame that can be used for main application windows. Shuts down the
 * framework and quits the application when closed.
 */
class MainFrame(gc: java.awt.GraphicsConfiguration = null) extends Frame(gc) {
  override def closeOperation() { sys.exit(0) }
}
