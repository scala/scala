package scala.swing.test

import swing._
import swing.event._

object SwingApp extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "SwingApp"
    var numclicks = 0
    object label extends Label {
      val prefix = "Number of button clicks: "
      text = prefix + "0  "
      listenTo(button)
      reactions += {
        case ButtonPressed(button) =>
          numclicks = numclicks + 1
          text = prefix + numclicks
      }
    }
    object button extends Button {
      text = "I am a button"
    }
    content = new GridPanel(GridPanel.Adapt,1)(label, button) {
      border = EmptyBorder(5, 5, 5, 5)
    }
  }
}

