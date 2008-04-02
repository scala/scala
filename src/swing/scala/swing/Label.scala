package scala.swing;

import javax.swing._;

class Label(override val peer: JLabel) extends Component {
  def this(txt: String) = this(new JLabel(txt))
  def this() = this("Untitled Label")
  def text: String = peer.getText()
  def text_=(s: String) = peer.setText(s)
  def xLabelAlignment: XAlignment = XAlignment.wrap(peer.getHorizontalAlignment)
  def xLabelAlignment_=(x: XAlignment) = peer.setHorizontalAlignment(x.peer)
  def yLabelAlignment: YAlignment = YAlignment.wrap(peer.getVerticalAlignment)
  def yLabelAlignment_=(x: YAlignment) = peer.setVerticalAlignment(x.peer)
}
