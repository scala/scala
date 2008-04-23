package scala.swing

import javax.swing._
import event._

class ComponentList extends Component with SequentialContainer.Wrapper {
  override lazy val peer: JList = new JList
  def this(elems: Seq[Object]) = {
    this()
    peer.setListData(elems.toArray)
  }

  def fixedCellWidth = peer.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = peer.setFixedCellWidth(x)

  def fixedCellHeight = peer.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = peer.setFixedCellHeight(x)
}