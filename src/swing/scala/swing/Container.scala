package scala.swing

abstract class Container(override val peer: javax.swing.JComponent) extends Component {
  def this() = this(new javax.swing.JComponent {})
  def content: Seq[Component] = new Content

  protected class Content extends Buffer[Component] {
    def wrap(c: java.awt.Component) = Component.wrapperFor(c.asInstanceOf[javax.swing.JComponent])
    def clear = peer.removeAll()
    def remove(n: Int): Component = {
      val c = peer.getComponent(n)
      peer.remove(n)
      wrap(c)
    }
    def update(n: Int, c: Component) { peer.add(c.peer, n) }
    def insertAll(n: Int, iter: Iterable[Component]) {
      var i = n
      for(el <- iter) {
        peer.add(el.peer, i)
        i += 1
      }
    }
    def readOnly : RandomAccessSeq[Component] = new RandomAccessSeq[Component] {
      def length = Content.this.length
      def apply(idx : Int) = Content.this.apply(idx)
      override def stringPrefix = Content.this.stringPrefix + "RO"
    }
    def +:(c: Component): Buffer[Component] = { update(0, c); this }
    def +=(c: Component) { peer.add(c.peer); this }
    def length = peer.getComponentCount
    def elements = peer.getComponents.projection.map(wrap(_)).elements
    def apply(n: Int) = wrap(peer.getComponent(n))
  }
}

