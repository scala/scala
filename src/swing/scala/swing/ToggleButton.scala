package scala.swing

import javax.swing._
import event._

/**
 * @see javax.swing.JToggleButton
 */
class ToggleButton(override val peer: JToggleButton) extends Button(peer) {
  def this(txt: String) = this(new JToggleButton(txt))
  def this() = this(new JToggleButton)
}