package scala.swing

import event._
import scala.collection.mutable.Buffer

object Container {
  trait Wrapper extends Component with Container {
    protected val _contents = new Content
    def contents: Seq[Component] = _contents

    protected class Content extends BufferWrapper[Component] {
      def wrap(c: java.awt.Component): Component = Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
      override def clear { peer.removeAll() }
      def remove(n: Int): Component = {
        val c = peer.getComponent(n)
        peer.remove(n)
        wrap(c)
      }
      protected def insertAt(n: Int, c: Component) { peer.add(c.peer, n) }
      def +=(c: Component) { peer.add(c.peer) }
      def length = peer.getComponentCount
      def apply(n: Int) = wrap(peer.getComponent(n))
    }

    peer.addContainerListener(new java.awt.event.ContainerListener {
      def componentAdded(e: java.awt.event.ContainerEvent) {
        publish(ComponentAdded(Wrapper.this,
                               Component.wrapperFor(e.getChild.asInstanceOf[javax.swing.JComponent])))
      }
      def componentRemoved(e: java.awt.event.ContainerEvent) {
        publish(ComponentRemoved(Wrapper.this,
                                 Component.wrapperFor(e.getChild.asInstanceOf[javax.swing.JComponent])))
      }
    })
  }
}

/**
 * A UI element that can contain <code>Component</code>s, such as windows,
 * panels, and menus.
 */
trait Container extends UIElement {
  def contents: Seq[Component]
}