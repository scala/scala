package scala.swing.event

abstract class ContainerEvent(override val source: Container) extends UIEvent(source)

case class ComponentAdded(override val source: Container, child: Component) extends ContainerEvent(source)
case class ComponentRemoved(override val source: Container, child: Component) extends ContainerEvent(source)
