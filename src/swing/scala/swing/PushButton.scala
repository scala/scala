package scala.swing

import javax.swing._
import event._

/** A class for buttons; standard constructor wraps around a swing button */
class PushButton(override val peer: JButton) extends Button with Publisher {
  def this(txt: String) = this(new JButton(txt))
  def this() = this("")
  def this(a: Action) = {
    this("")
    action = a
  }
}
