/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import javax.swing.JComponent
import scala.collection.mutable

/** A container that associates layout constraints of member type
 *  `Constraints` with its children.
 *
 *  See `GridBagPanel` for an example container with custom constraints.
 *
 *  @note [Java Swing] In scala.swing, panels and layout managers are
 *  combined into subclasses of this base class. This approach allows for
 *  typed component constraints.
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
   * manager and the component peer. This method needs to interact properly
   * with method `constraintsFor`, i.e., it might need to remove previously
   * held components in order to maintain layout consistency. See `BorderPanel`
   * for an example.
   */
  protected def add(comp: Component, c: Constraints)

  /**
   * A map of components to the associated layout constraints.
   * Any element in this map is automatically added to the contents of this
   * panel. Therefore, specifying the layout of a component via
   *
   * layout(myComponent) = myConstraints
   *
   * also ensures that myComponent is properly added to this container.
   */
  def layout: mutable.Map[Component, Constraints] = new mutable.Map[Component, Constraints] {
    def -= (c: Component): this.type = { _contents -= c; this }
    def += (cl: (Component, Constraints)): this.type = { update(cl._1, cl._2); this }
    override def update (c: Component, l: Constraints) {
      val (v, msg) = areValid(l)
      if (!v) throw new IllegalArgumentException(msg)
      add(c, l)
    }
    def get(c: Component) = Option(constraintsFor(c))
    override def size = peer.getComponentCount
    def iterator: Iterator[(Component, Constraints)] =
      peer.getComponents.iterator.map { c =>
        val comp = UIElement.cachedWrapper[Component](c.asInstanceOf[JComponent])
        (comp, constraintsFor(comp))
      }
  }
}
