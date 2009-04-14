package scala.swing

import event._

/**
 * A frame that can be used for main application windows. Shuts down the
 * framework and quits the application when closed.
 */
class MainFrame extends Frame {
  override def closeOperation { System.exit(0); }
}
