package scala.swing

import javax.swing._

/**
 * A two state button that is usually used in a <code>ButtonGroup</code>
 * together with other <code>RadioButton</code>s, in order to indicate
 * that at most one of them can be selected.
 *
 * @see javax.swing.JRadioButton
 */
class RadioButton(text0: String) extends ToggleButton {
  override lazy val peer: JRadioButton = new JRadioButton(text0)
  def this() = this("")
}