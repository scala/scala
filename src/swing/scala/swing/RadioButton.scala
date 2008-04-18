package scala.swing

import javax.swing._

class RadioButton(override val peer: JRadioButton) extends ToggleButton(peer) {
  def this(txt: String) = this(new JRadioButton(txt))
  def this() = this(new JRadioButton)
}