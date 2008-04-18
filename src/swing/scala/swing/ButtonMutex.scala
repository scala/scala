package scala.swing

import javax.swing.{AbstractButton => JAbstractButton,Icon}
import event._
import scala.collection._


class ButtonMutex(initialButtons: Button*) {
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

  def deselectAll() { peer.clearSelection }
  def selected: Option[Selectable] = buttons.find(_.selected)
  def select(b: Selectable) { b.selected = true }
}
