package scala.swing

import javax.swing._
import event._

/**
 * @see javax.swing.JButton
 */
class PushButton(override val peer: JButton) extends Button(peer) with Publisher {
  def this(txt: String) = this(new JButton(txt))
  def this() = this("")
  def this(a: Action) = {
    this("")
    action = a
  }
}
