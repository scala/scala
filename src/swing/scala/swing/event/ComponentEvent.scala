package scala.swing.event

trait ComponentEvent extends UIEvent {
  override val source: Component
}

case class ComponentMoved(override val source: Component) extends ComponentEvent
case class ComponentResized(override val source: Component) extends ComponentEvent
case class ComponentShown(override val source: Component) extends ComponentEvent
case class ComponentHidden(override val source: Component) extends ComponentEvent
