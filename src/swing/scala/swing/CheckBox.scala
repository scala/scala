package scala.swing

import javax.swing._

/**
 * Two state button that can either be checked or unchecked.
 *
 * @see javax.swing.JCheckBox
 */
class CheckBox(text: String) extends ToggleButton {
  override lazy val peer: JCheckBox = new JCheckBox(text) with SuperMixin
  def this() = this("")

  def borderPaintedFlat: Boolean = peer.isBorderPaintedFlat
  def borderPaintedFlat_=(flat: Boolean) { peer.setBorderPaintedFlat(flat) }
}