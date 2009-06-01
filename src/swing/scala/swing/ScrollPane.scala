package scala.swing

import javax.swing.JScrollPane

/**
 * Can have at most a single child component, which will be put inside a canvas (the viewport)
 * that can be scrolled.
 *
 * @see javax.swing.JScrollPane
 */
class ScrollPane extends Component with Container {
  override lazy val peer: JScrollPane = new JScrollPane
  def this(c: Component) = {
    this()
    viewportView = c
  }
  def contents: Seq[Component] =
    List(UIElement.cachedWrapper(peer.getViewport.getView.asInstanceOf[javax.swing.JComponent]))

  /**
   * Sets the single child.
   */
  def contents_=(c: Component) { peer.setViewportView(c.peer) }

  /**
   * The component being displayed in this pane's row header.
   *
   * If you want to create a row header for lists or tables, you probably
   * want to let the row header be a list view with the same row height as
   * the viewport component.
   */
  def rowHeaderView: Option[Component] = Swing.toOption(peer.getRowHeader.getView).map(UIElement.cachedWrapper(_))
  def rowHeaderView_=(c: Component) = peer.setRowHeaderView(c.peer)
  def rowHeaderView_=(c: Option[Component]) = peer.setRowHeaderView(Swing.toNull(c.map(_.peer)))

  def viewportView: Option[Component] = Swing.toOption(peer.getViewport.getView).map(UIElement.cachedWrapper(_))
  def viewportView_=(c: Component) = peer.setViewportView(c.peer)
  def viewportView_=(c: Option[Component]) = peer.setViewportView(Swing.toNull(c.map(_.peer)))
}
