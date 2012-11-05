/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import event._
import scala.collection.mutable.Buffer

object Container {
  /**
   * Utility trait for wrapping containers. Provides an immutable
   * implementation of the contents member.
   */
  trait Wrapper extends Container with Publisher {
    override def peer: javax.swing.JComponent

    protected val _contents = new Content
    def contents: Seq[Component] = _contents

    protected class Content extends BufferWrapper[Component] {
      override def clear() { peer.removeAll() }
      override def remove(n: Int): Component = {
        val c = peer.getComponent(n)
        peer.remove(n)
        UIElement.cachedWrapper[Component](c)
      }
      protected def insertAt(n: Int, c: Component) { peer.add(c.peer, n) }
      def +=(c: Component): this.type = { peer.add(c.peer) ; this }
      def length = peer.getComponentCount
      def apply(n: Int) = UIElement.cachedWrapper[Component](peer.getComponent(n))
    }

    peer.addContainerListener(new java.awt.event.ContainerListener {
      def componentAdded(e: java.awt.event.ContainerEvent) {
        publish(ComponentAdded(Wrapper.this,
          UIElement.cachedWrapper[Component](e.getChild.asInstanceOf[javax.swing.JComponent])))
      }
      def componentRemoved(e: java.awt.event.ContainerEvent) {
        publish(ComponentRemoved(Wrapper.this,
          UIElement.cachedWrapper[Component](e.getChild.asInstanceOf[javax.swing.JComponent])))
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
