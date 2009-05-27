package scala.swing

import javax.swing.JComponent
import scala.collection.mutable.Map

/**
 * A container that associates layout constraints of member type Constraints
 * with its children. See GridBagPanel for an example container with custom
 * constraints.
 *
 * @note [Java Swing] In scala.swing, panels and layout managers are
 * combined into subclasses of this base class. This approach allows for typed
 * component constraints.
 */
trait LayoutContainer extends Container.Wrapper {
  /**
   * The type of component constraints for this container.
   */
  type Constraints <: AnyRef

  /**
   * Obtains the constraints for the given component from the underlying
   * Swing layout manager.
   */
  protected def constraintsFor(c: Component): Constraints
  /**
   * Checks whether the given constraints are valid. Additionally returns
   * an error string that is only fetched if the constraints aren't valid.
   */
  protected def areValid(c: Constraints): (Boolean, String)
  /**
   * Adds a component with the given constraints to the underlying layout
   * manager and the component peer.
   */
  protected def add(comp: Component, c: Constraints)

  /**
   * A map of components to the associated layout constraints.
   * Any element in this map is automatically added to the contents of this
   * panel. Therefore, specifying the layout of a component via
   *
   * layout(myComponent) = myConstraints
   *
   * also ensures that myComponent is properly add to this container.
   */
  def layout: Map[Component, Constraints] = new Map[Component, Constraints] {
    def -= (c: Component): this.type = { _contents -= c; this }
    def += (cl: (Component, Constraints)): this.type = { update(cl._1, cl._2); this }
    override def update (c: Component, l: Constraints) {
      val (v, msg) = areValid(l)
      if (!v) throw new IllegalArgumentException(msg)
      add(c, l)
      this
    }
    def get(c: Component) = Swing.toOption(constraintsFor(c))
    override def size = peer.getComponentCount
    def iterator: Iterator[(Component, Constraints)] =
      Iterator.range(0,size).map { c =>
        val comp = Component.wrapperFor[Component](peer.getComponent(c).asInstanceOf[JComponent])
        (comp, constraintsFor(comp))
      }
  }
}
