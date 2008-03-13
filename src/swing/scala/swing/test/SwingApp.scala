package swing.test;

import swing._
import swing.event._
import swing.layout._

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
    contents += new Panel(label, button) {
      layout = column
      border = new EmptyBorder(30, 30, 10, 30)
    }
  }
}

