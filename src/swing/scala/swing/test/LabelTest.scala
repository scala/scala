package scala.swing
package test

import scala.swing._
import scala.swing.event._

object LabelTest extends SimpleSwingApplication {
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

