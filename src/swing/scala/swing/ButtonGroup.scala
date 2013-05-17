/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import scala.collection.mutable

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
    def contains(b: AbstractButton) = this.iterator.contains(b)
    override def size = peer.getButtonCount
    def iterator: Iterator[AbstractButton] = new Iterator[AbstractButton] {
      val enum = peer.getElements
      def next = UIElement.cachedWrapper[AbstractButton](enum.nextElement)
      def hasNext = enum.hasMoreElements
    }
  }
  buttons ++= initialButtons

  //1.6: def deselectAll() { peer.clearSelection }
  def selected: Option[AbstractButton] = buttons.find(_.selected)
  def select(b: AbstractButton) { peer.setSelected(b.peer.getModel, true) }
}
