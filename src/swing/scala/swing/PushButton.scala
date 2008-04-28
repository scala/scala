package scala.swing

import javax.swing._
import event._

/**
 * A button that can be pushed, usually to perfrom some action.
 *
 * @see javax.swing.JButton
 */
class PushButton(text0: String) extends Button with Publisher {
  override lazy val peer: JButton = new JButton(text0)
  def this() = this("")
  def this(a: Action) = {
    this("")
    action = a
  }
}
