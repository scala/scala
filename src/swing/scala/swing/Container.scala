package scala.swing

import event._
import scala.collection.mutable.Buffer

object Container {
  /**
   * Utility trait for wrapping containers. Provides an immutable
   * implementation of the contents member.
   */
  trait Wrapper extends Component with Container {
    protected val _contents = new Content
    def contents: Seq[Component] = _contents

    protected class Content extends BufferWrapper[Component] {
      def wrap(c: java.awt.Component): Component = Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
      override def clear { peer.removeAll() }
      override def remove(n: Int): Component = {
        val c = peer.getComponent(n)
        peer.remove(n)
        wrap(c)
      }
      protected def insertAt(n: Int, c: Component) { peer.add(c.peer, n) }
      def +=(c: Component): this.type = { peer.add(c.peer) ; this }
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
 * The base traits for UI elements that can contain <code>Component</code>s.
 *
 * @note [Java Swing] This is not the wrapper for java.awt.Container but a trait
 * that extracts a common interface for components, menus, and windows.
 */
trait Container extends UIElement {
  /**
   * The child components of this container.
   */
  def contents: Seq[Component]
}
