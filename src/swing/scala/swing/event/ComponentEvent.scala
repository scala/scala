package scala.swing.event

abstract class ComponentEvent(override val source: Component) extends UIEvent(source)

case class ComponentMoved(override val source: Component) extends ComponentEvent(source)
case class ComponentResized(override val source: Component) extends ComponentEvent(source)
case class ComponentShown(override val source: Component) extends ComponentEvent(source)
case class ComponentHidden(override val source: Component) extends ComponentEvent(source)
