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
        case ButtonClicked(button) =>
          numclicks = numclicks + 1
          text = prefix + numclicks
      }
    }
    object button extends PushButton {
      text = "I am a button"
    }
    contents = new GridPanel(GridPanel.Adapt,1) {
      contents.append(label, button)
      border = Border.Empty(5, 5, 5, 5)
    }
  }
}

