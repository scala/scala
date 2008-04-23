package scala.swing

import javax.swing._

/**
 * @see javax.swing.JSeparator
 */
class Separator(o: Orientation.Value) extends Component with Oriented {
  override lazy val peer: JSeparator = new JSeparator(o.id)
  def this() = this(Orientation.Horizontal)
}
