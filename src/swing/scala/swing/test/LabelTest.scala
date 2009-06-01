package scala.swing.test

import scala.swing._
import scala.swing.event._

object LabelTest extends SimpleGUIApplication{
  def top = new MainFrame{
    contents = new Label {
      text = "Hello"
      import java.awt.event._
      /*peer.addMouseListener (new MouseAdapter{
        override def mousePressed(e : MouseEvent ) {
          println("Mouse pressed")
        }
      })*/
      listenTo(Mouse.clicks)
      reactions += {
        case MousePressed(_,_,_,_,_) =>
          println("Mouse pressed2")
      }
    }
  }
}

