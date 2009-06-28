package scala.swing.event

trait ComponentEvent extends UIEvent {
  override val source: Component
}

@deprecated("Use UIElementMoved instead.")
case class ComponentMoved(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementResized instead.")
case class ComponentResized(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementShown instead.")
case class ComponentShown(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementHidden instead.")
case class ComponentHidden(override val source: Component) extends ComponentEvent
