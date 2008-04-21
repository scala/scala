package scala.swing

import javax.swing.JScrollPane

/**
 * @see javax.swing.JScrollPane
 */
class ScrollPane(override val peer: JScrollPane) extends Component(peer) with Container {
  def this() = this(new JScrollPane)
  def this(contents: Component) = this(new JScrollPane(contents.peer))

  def contents: Seq[Component] =
    List(Component.wrapperFor(peer.getViewport.getView.asInstanceOf[javax.swing.JComponent]))
  def contents_=(c: Component) { peer.setViewportView(c.peer) }

  /*def rowHeaderView: Component = null
  def rowHeaderView_=(c: Component) = peer.setRowHeaderView(c.peer)
  def viewportView: Component = null
  def viewportView_=(c: Component) = peer.setViewportView(c.peer)*/
}
