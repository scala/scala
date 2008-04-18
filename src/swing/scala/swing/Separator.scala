package scala.swing

import javax.swing._

/**
 * @see javax.swing.JSeparator
 */
class Separator extends Component with Oriented {
  override lazy val peer = new JSeparator
}
