package scala.swing
package test

import swing._

/**
 * A simple swing demo.
 */
object HelloWorld extends GUIApplication {
  def main(args: Array[String]) = run {
    val frame = new MainFrame {
      title = "HelloWorldSwing"
      contents = new Label("Hello World")
    }
    frame.pack()
    frame.visible = true
  }
}
