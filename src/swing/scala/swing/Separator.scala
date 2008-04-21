package scala.swing

import javax.swing._

/**
 * @see javax.swing.JSeparator
 */
class Separator(override val peer: JSeparator) extends Component(peer) with Oriented {
  def this() = this(new JSeparator)
}
