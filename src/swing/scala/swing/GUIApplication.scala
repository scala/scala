package scala.swing

import javax.swing._
import event.Event

class GUIApplication {
  //def defaultLookAndFeelDecorated: Boolean = true

  def init() = {
    //UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    //JFrame.setDefaultLookAndFeelDecorated(defaultLookAndFeelDecorated)
  }

  def run(prog: => Unit): Unit =
    SwingUtilities.invokeLater {
      new Runnable() {
        def run() = { init(); prog }
      }
    }
}
