package scala.swing.test

import swing._, swing.event._

object MyApp extends SimpleGUIApplication {
  def top = new Frame {
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
    size = (300, 80)
  }
}
