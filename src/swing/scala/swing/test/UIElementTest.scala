package scala.swing.test

import swing._
import swing.event._
import java.awt.Color

object UIElementTest extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "UIElement Test"
    background = Color.RED
  }
}

