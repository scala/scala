package scala.swing.test

import scala.swing._
import scala.swing.event._

object LabelTest extends SimpleGUIApplication{
  def top = new MainFrame{
    contents = new Label {
      text = "Hello"
      import java.awt.event._
      listenTo(mouse.clicks)
      reactions += {
        case MousePressed(_,_,_,_,_) =>
          println("Mouse pressed2")
      }
    }
  }
}

