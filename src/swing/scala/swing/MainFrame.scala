package swing;

import javax.swing._;
import event._;

class MainFrame(jframe: JFrame) extends Frame(jframe) {
  def this() = this(new JFrame("Untitled Frame"))
  reactions += {
    case WindowClosing(_) => System.exit(1)
  }
}
