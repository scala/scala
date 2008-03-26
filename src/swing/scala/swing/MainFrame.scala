package swing

import event._

class MainFrame(peer: javax.swing.JFrame) extends Frame(peer) {
  def this() = this(new javax.swing.JFrame)
  reactions += {
    case WindowClosing(_) => System.exit(1)
  }
}
