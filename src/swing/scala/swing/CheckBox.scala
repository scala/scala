package scala.swing

import javax.swing._

class CheckBox(override val peer: JCheckBox) extends ToggleButton {
  def this(txt: String) = this(new JCheckBox(txt))
  def this() = this(new JCheckBox)
}