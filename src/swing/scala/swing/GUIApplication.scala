package scala.swing

import javax.swing._
import event.Event

class GUIApplication {

  type Reaction = PartialFunction[Event, unit]

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
