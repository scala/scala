package scala.swing

import javax.swing._

/**
 * @see javax.swing.JRadioButton
 */
class RadioButton(text0: String) extends ToggleButton {
  override lazy val peer: JRadioButton = new JRadioButton(text0)
  def this() = this("")
}