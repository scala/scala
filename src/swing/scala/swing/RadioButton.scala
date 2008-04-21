package scala.swing

import javax.swing._

/**
 * @see javax.swing.JRadioButton
 */
class RadioButton(override val peer: JRadioButton) extends ToggleButton(peer) {
  def this(text: String) = this(new JRadioButton(text))
  def this() = this(new JRadioButton)
}