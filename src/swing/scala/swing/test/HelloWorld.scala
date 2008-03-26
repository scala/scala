package swing.test

import swing._

object HelloWorld extends GUIApplication {
  def main(args: Array[String]) = run {
    new Frame {
      title = "HelloWorldSwing"
      content = new Label("Hello World")
    }.pack.show
  }
}
