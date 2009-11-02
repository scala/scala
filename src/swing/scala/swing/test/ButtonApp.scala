package scala.swing
package test

import java.awt.Dimension

import swing._
import swing.event._

object ButtonApp extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "My Frame"
    contents = new GridPanel(2, 2) {
      hGap = 3
      vGap = 3
      contents += new Button {
        text = "Press Me!"
        reactions += {
          case ButtonClicked(_) => text = "Hello Scala"
        }
      }
    }
    size = new Dimension(300, 80)
  }
}

