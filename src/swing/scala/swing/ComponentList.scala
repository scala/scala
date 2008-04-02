package scala.swing

import javax.swing._
import event._

class ComponentList(val jlist: JList) extends Container(jlist) with Publisher {
  def this() = this(new JList)
  def this(elems: Seq[Object]) = this(new JList(elems.toArray))

  def fixedCellWidth = jlist.getFixedCellWidth
  def fixedCellWidth_=(x: Int) = jlist.setFixedCellWidth(x)

  def fixedCellHeight = jlist.getFixedCellHeight
  def fixedCellHeight_=(x: Int) = jlist.setFixedCellHeight(x)
}
