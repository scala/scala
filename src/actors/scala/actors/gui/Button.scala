package scala.actors.gui

import javax.swing._
import event._

/** A class for buttons; standard constructor wraps around a swing button */
class Button(val jbutton: JButton) extends Container(jbutton) with SwingComponent with Publisher {
  def this(txt: String) = this(new JButton(txt))
  def this() = this(new JButton())
  def text: String = jbutton.getText()
  def text_=(s: String) = jbutton.setText(s)
  def icon: Icon = jbutton.getIcon()
  def icon_=(i: Icon) = jbutton.setIcon(i)
  jbutton.addActionListener {
    new java.awt.event.ActionListener {
      def actionPerformed(e: java.awt.event.ActionEvent): unit =
        publish(ButtonPressed(Button.this))
    }
  }
}
