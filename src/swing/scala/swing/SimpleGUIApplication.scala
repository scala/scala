package scala.swing

import javax.swing._

abstract class SimpleGUIApplication extends GUIApplication {
  def top: Frame

  def main(args: Array[String]) = {
    SwingUtilities.invokeLater {
      new Runnable { def run() { init(); top.pack(); top.visible = true } }
    }
  }
}
