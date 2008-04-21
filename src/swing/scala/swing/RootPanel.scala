package scala.swing

/**
 * @see javax.swing.RootPaneContainer
 */
trait RootPanel extends Container {
  def peer: java.awt.Component with javax.swing.RootPaneContainer
  //contents = new Component { opaque = false }
  def contents: Seq[Component] = {
    Swing.toOption(peer.getContentPane.getComponent(0)).map { c =>
      Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
    }.toList
    /*if (peer.getContentPane.getComponentCount == 0) Nil
    else {
      val c = peer.getContentPane.getComponent(0)
      ListComponent.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
    }*/
  }
  def contents_=(c: Component) {
    if (peer.getContentPane.getComponentCount > 0) {
      val old = peer.getContentPane.getComponent(0)
      peer.getContentPane.remove(old)
    }
    peer.getContentPane.add(c.peer)
  }
}
