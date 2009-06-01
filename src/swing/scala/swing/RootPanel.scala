package scala.swing

/**
 * The root of a component hierarchy. Contains at most one component.
 *
 * @see javax.swing.RootPaneContainer
 */
trait RootPanel extends Container {
  def peer: java.awt.Component with javax.swing.RootPaneContainer

  /**
   * At most one component.
   */
  def contents: Seq[Component] = {
    Swing.toOption[Any](peer.getContentPane.getComponent(0)).map { c =>
      UIElement.cachedWrapper(c.asInstanceOf[javax.swing.JComponent])
    }.toList
  }
  def contents_=(c: Component) {
    if (peer.getContentPane.getComponentCount > 0) {
      val old = peer.getContentPane.getComponent(0)
      peer.getContentPane.remove(old)
    }
    peer.getContentPane.add(c.peer)
  }
}
