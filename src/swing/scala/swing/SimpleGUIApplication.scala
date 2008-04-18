package scala.swing

import javax.swing._

abstract class SimpleGUIApplication extends GUIApplication {
  def top: Frame

  def main(args: Array[String]) = {
    SwingUtilities.invokeLater {
      new Runnable { def run() { init(); top.pack().show() } }
    }
  }

  implicit def string2label(s: String): Label = new Label(s)
}
