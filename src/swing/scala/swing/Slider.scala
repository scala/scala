package swing;

import javax.swing._
import event._

class Slider(val jbutton: JButton) extends Container(jbutton) with Publisher {
  def this(txt: String) = this(new JButton(txt))
  def this() = this("Untitled Button")
  def text: String = jbutton.getText()
  def text_=(s: String) = jbutton.setText(s)
  jbutton.addActionListener {
    new java.awt.event.ActionListener {
      def actionPerformed(e: java.awt.event.ActionEvent): unit =
        {}
    }
  }
}
