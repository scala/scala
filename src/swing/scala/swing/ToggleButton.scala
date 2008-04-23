package scala.swing

import javax.swing._
import event._

/**
 * @see javax.swing.JToggleButton
 */
class ToggleButton(text0: String) extends Button {
  override lazy val peer: JToggleButton = new JToggleButton(text0)
  def this() = this("")
}