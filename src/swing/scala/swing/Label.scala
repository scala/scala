package scala.swing

import javax.swing._

/**
 * A label component.
 *
 * @see javax.swing.JLabel
 */
class Label(text0: String) extends Component {
  override lazy val peer: JLabel = new JLabel(text0)
  def this() = this("")
  def text: String = peer.getText()
  def text_=(s: String) = peer.setText(s)
  /**
   * The alignment of the label's contents relative to its bounding box.
   */
  def xAlignment: Alignment.Value = Alignment(peer.getHorizontalAlignment)
  def xAlignment_=(x: Alignment.Value) = peer.setHorizontalAlignment(x.id)
  def yAlignment: Alignment.Value = Alignment(peer.getVerticalAlignment)
  def yAlignment_=(x: Alignment.Value) = peer.setVerticalAlignment(x.id)
}
