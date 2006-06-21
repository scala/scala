package scala.actors.gui

import javax.swing._
import event.Event

class GUIApplication {
  def defaultLookAndFeelDecorated: boolean = true

  def init() = {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
    JFrame.setDefaultLookAndFeelDecorated(defaultLookAndFeelDecorated)
  }

  def run(prog: => unit): unit =
    SwingUtilities.invokeLater {
      new Runnable() {
        def run() = { init(); prog }
      }
    }
}
