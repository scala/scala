package scala.actors.gui;

import javax.swing._;
import scala.actors.gui.event._;

class MainFrame(jframe: JFrame) extends Frame(jframe) {
  def this() = this(new JFrame("Untitled Frame"))

  addHandler {
    case WindowClosing(_) => System.exit(1)
  }

  subscribe(this)
}
