package scala.swing

import javax.swing.JScrollPane

/**
 * @see javax.swing.JScrollPane
 */
class ScrollPane extends Component with Container {
  override lazy val peer: JScrollPane = new JScrollPane
  def this(c: Component) = {
    this()
    viewportView = c
  }
  def contents: Seq[Component] =
    List(Component.wrapperFor(peer.getViewport.getView.asInstanceOf[javax.swing.JComponent]))
  def contents_=(c: Component) { peer.setViewportView(c.peer) }

  def rowHeaderView: Option[Component] = Swing.toOption(peer.getRowHeader.getView).map(Component.wrapperFor(_))
  def rowHeaderView_=(c: Component) = peer.setRowHeaderView(c.peer)
  def rowHeaderView_=(c: Option[Component]) = peer.setRowHeaderView(Swing.toNull(c.map(_.peer)))
  def viewportView: Option[Component] = Swing.toOption(peer.getViewport.getView).map(Component.wrapperFor(_))
  def viewportView_=(c: Component) = peer.setViewportView(c.peer)
  def viewportView_=(c: Option[Component]) = peer.setViewportView(Swing.toNull(c.map(_.peer)))
}
