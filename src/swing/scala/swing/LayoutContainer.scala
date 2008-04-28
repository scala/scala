package scala.swing

import javax.swing.JComponent
import scala.collection.mutable.Map

/**
 * A container that associates layout constraints with its children.
 * See GridBagPanel for an example.
 */
trait LayoutContainer extends Container.Wrapper {
  /**
   * The specific type of constraints.
   */
  type Constraints <: AnyRef

  /**
   * Used to obtain the constraints from a Swing layout manager.
   */
  protected def constraintsFor(c: Component): Constraints
  /**
   * Checks whether the given constraints are valid. Additionally returns
   * an error string that is only fetched if the constraints aren't valid.
   */
  protected def areValid(c: Constraints): (Boolean, String)
  /**
   * Adds the component to the layout manager and the peer.
   */
  protected def add(comp: Component, c: Constraints)

  /**
   * A map of components to the associated layout constraints.
   * Any element in this map is automatically added to this panel.
   */
  def layout: Map[Component, Constraints] = new Map[Component, Constraints] {
    def -=(c: Component) { _contents -= c }
    def update(c: Component, l: Constraints) {
      val (v, msg) = areValid(l)
      if (!v) throw new IllegalArgumentException(msg)
      add(c, l)
    }
    def get(c: Component) = Swing.toOption(constraintsFor(c))
    def size = peer.getComponentCount
    def elements: Iterator[(Component, Constraints)] =
      Iterator.range(0,size).map { c =>
        val comp = Component.wrapperFor[Component](peer.getComponent(c).asInstanceOf[JComponent])
        (comp, constraintsFor(comp))
      }
  }
}
