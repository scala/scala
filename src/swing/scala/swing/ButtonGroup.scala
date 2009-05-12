package scala.swing

import javax.swing.{AbstractButton => JAbstractButton,Icon}
import event._
import scala.collection._
import scala.collection.mutable.Buffer

/**
 * A button mutex. At most one of its associated buttons is selected
 * at a time.
 *
 * @see javax.swing.ButtonGroup
 */
class ButtonGroup(initialButtons: AbstractButton*) {
  val peer: javax.swing.ButtonGroup = new javax.swing.ButtonGroup

  val buttons: mutable.Set[AbstractButton] = new mutable.Set[AbstractButton] {
    def -=(b: AbstractButton): this.type = { peer.remove(b.peer); this }
    def +=(b: AbstractButton): this.type = { peer.add(b.peer); this }
    def contains(b: AbstractButton) = elements.contains(b)
    override def size = peer.getButtonCount
    def elements: Iterator[AbstractButton] = new Iterator[AbstractButton] {
      val enum = peer.getElements
      def next = Component.wrapperFor[AbstractButton](enum.nextElement)
      def hasNext = enum.hasMoreElements
    }
  }
  buttons ++= initialButtons

  //1.6: def deselectAll() { peer.clearSelection }
  def selected: Option[AbstractButton] = buttons.find(_.selected)
  def select(b: AbstractButton) { peer.setSelected(b.peer.getModel, true) }
}
