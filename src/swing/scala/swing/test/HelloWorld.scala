package scala.swing.test

import swing._

object HelloWorld extends GUIApplication {
  def main(args: Array[String]) = run {
    val frame = new Frame {
      title = "HelloWorldSwing"
      contents = new Label("Hello World")
    }
    frame.pack()
    frame.visible = true
  }
}
