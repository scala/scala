package scala.swing

import javax.swing.JComponent
import scala.collection.mutable.Map

/**
 * A container that associates layout constraints with its children.
 * See GridBagPanel for an example.
 */
trait LayoutContainer extends Container.Wrapper {
  type Constraints <: { def peer: AnyRef }
  protected def constraintsFor(c: Component): Constraints
  def layout: Map[Component, Constraints] = new Map[Component, Constraints] {
    def -=(c: Component) { _contents -= c }
    def update(c: Component, l: Constraints) { peer.add(c.peer, l.peer) }
    def get(c: Component) = Swing.toOption(constraintsFor(c))
    def size = peer.getComponentCount
    def elements: Iterator[(Component, Constraints)] =
      Iterator.range(0,size).map { c =>
        val comp = Component.wrapperFor[Component](peer.getComponent(c).asInstanceOf[JComponent])
        (comp, constraintsFor(comp))
      }
  }
}
