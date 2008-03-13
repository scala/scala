package swing.test;

import swing._;
import javax.swing._;

object HelloWorld extends GUIApplication {
  def main(args: Array[String]) = run {
    new Frame {
      title = "HelloWorldSwing";
      contents += new Label("Hello World");
    }.pack.show
  }
}
