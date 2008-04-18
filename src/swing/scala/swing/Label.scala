package scala.swing

import javax.swing._

class Label(override val peer: JLabel) extends Component {
  def this(txt: String) = this(new JLabel(txt))
  def this() = this("")
  def text: String = peer.getText()
  def text_=(s: String) = peer.setText(s)
  /**
   * The alignment of the label's contents relative to its bounding box.
   */
  def xAlignment: XAlignment = XAlignment.wrap(peer.getHorizontalAlignment)
  def xAlignment_=(x: XAlignment) = peer.setHorizontalAlignment(x.peer)
  def yAlignment: YAlignment = YAlignment.wrap(peer.getVerticalAlignment)
  def yAlignment_=(x: YAlignment) = peer.setVerticalAlignment(x.peer)
}
