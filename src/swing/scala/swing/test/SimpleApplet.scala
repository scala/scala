package scala.swing.test

import event._

class SimpleApplet extends Applet {
  object ui extends UI with Reactor {
    def init() = {
      val button = new PushButton("Press here!")
      val text = new TextField("Java Version: " +
                System.getProperty("java.version")+"\n")
      listenTo(button)
      reactions += {
        case ButtonClicked(_) => text.contents += "Button Pressed!\n"
        case _ =>
      }
      content = new BoxPanel(Vertical) { contents.append(button, text) }
    }
  }
}