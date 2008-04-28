package scala.swing

import javax.swing._

/**
 * Two state button that can be either be checked or not.
 *
 * @see javax.swing.JCheckBox
 */
class CheckBox(text: String) extends ToggleButton {
  override lazy val peer: JCheckBox = new JCheckBox(text)
  def this() = this("")
}