package scala.actors.gui;

import javax.swing._

abstract class SimpleGUIApplication extends GUIApplication {

  def top: Frame;

  def main(args: Array[String]) = {
    run { top.pack.show }
  }

  implicit def string2label(s: String): Label = new Label(s)
}
