package scala.swing;

import javax.swing.JScrollPane

class ScrollPane(override val peer: JScrollPane) extends Container(peer) with Publisher {
  def this() = this(new JScrollPane)
  def this(contents: Component) = this(new JScrollPane(contents.peer))

  def rowHeaderView: Component = null
  def rowHeaderView_=(c: Component) = peer.setRowHeaderView(c.peer)
  def viewportView: Component = null
  def viewportView_=(c: Component) = peer.setViewportView(c.peer)

}
