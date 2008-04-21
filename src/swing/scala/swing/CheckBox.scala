package scala.swing

import javax.swing._

/**
 * @see javax.swing.JCheckBox
 */
class CheckBox(override val peer: JCheckBox) extends ToggleButton(peer) {
  def this(txt: String) = this(new JCheckBox(txt))
  def this() = this(new JCheckBox)
}