package scala.swing

import javax.swing.{AbstractButton => JAbstractButton,Icon}
import event._
import scala.collection._

/**
 * A button mutex. At most one of its associated buttons is selected
 * at a time.
 *
 * @see javax.swing.ButtonGroup
 */
class ButtonGroup(initialButtons: Button*) {
  val peer: javax.swing.ButtonGroup = new javax.swing.ButtonGroup

  val buttons: mutable.Set[Button] = new mutable.Set[Button] {
    def -=(b: Button) { peer.remove(b.peer) }
    def +=(b: Button) { peer.add(b.peer) }
    def contains(b: Button) = elements.contains(b)
    def size = peer.getButtonCount
    def elements: Iterator[Button] = new Iterator[Button] {
      val enum = peer.getElements
      def next = Component.wrapperFor[Button](enum.nextElement)
      def hasNext = enum.hasMoreElements
    }
  }
  buttons ++= initialButtons

  //1.6: def deselectAll() { peer.clearSelection }
  def selected: Option[Button] = buttons.find(_.selected)
  def select(b: Button) { peer.setSelected(b.peer.getModel, true) }
}
