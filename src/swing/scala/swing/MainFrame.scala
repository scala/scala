package scala.swing

import event._

/**
 * A frame that can be used for main application windows. Quits the
 * application on close.
 */
class MainFrame extends Frame {
  reactions += {
    case WindowClosing(_) => System.exit(1)
  }
}
