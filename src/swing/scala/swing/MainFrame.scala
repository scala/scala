package scala.swing

import event._

class MainFrame extends Frame {
  reactions += {
    case WindowClosing(_) => System.exit(1)
  }
}
