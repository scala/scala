package scala.swing

trait RootPanel { self: Object { def peer: java.awt.Component with javax.swing.RootPaneContainer } =>
  content = new Component {}
  def content: Component = {
    if (peer.getContentPane.getComponentCount == 0) new Component {}
    else {
      val c = peer.getContentPane.getComponent(0)
      Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
    }
  }
  def content_=(c: Component) {
    if (peer.getContentPane.getComponentCount > 0) {
      val old = peer.getContentPane.getComponent(0)
      peer.getContentPane.remove(old)
    }
    peer.getContentPane.add(c.peer)
  }
}
