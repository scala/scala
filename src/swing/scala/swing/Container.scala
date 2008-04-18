package scala.swing

import event._
import scala.collection.mutable.Buffer

trait Container extends Component {
  protected val _contents = new Content
  def contents: Seq[Component] = _contents

  protected class Content extends BufferAdapter[Component] {
    def wrap(c: java.awt.Component) = Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
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
      publish(ComponentAdded(Container.this,
                             Component.wrapperFor(e.getChild.asInstanceOf[javax.swing.JComponent])))
    }
    def componentRemoved(e: java.awt.event.ContainerEvent) {
      publish(ComponentRemoved(Container.this,
                               Component.wrapperFor(e.getChild.asInstanceOf[javax.swing.JComponent])))
    }
  })
}

