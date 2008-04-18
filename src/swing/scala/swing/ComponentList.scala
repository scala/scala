package scala.swing

import javax.swing._
import event._

class ComponentList(override val peer: JList) extends Component with Container with Publisher {
  def this() = this(new JList)
  def this(elems: Seq[Object]) = this(new JList(elems.toArray))

  def fixedCellWidth = peer.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = peer.setFixedCellWidth(x)

  def fixedCellHeight = peer.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = peer.setFixedCellHeight(x)
}
