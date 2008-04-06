package scala.swing.test

import event._

class SimpleApplet extends Applet {
  object ui extends UI with Reactor {
    def init() = {
      val button = new Button("Press here!")
      val text = new TextField("Java Version: " +
                System.getProperty("java.version")+"\n")
      listenTo(button)
      reactions += {
        case ButtonPressed(_) => text.content += "Button Pressed!\n"
        case _ =>
      }
      content = new BoxPanel(Vertical)(button, text)
    }
  }
}