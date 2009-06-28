package scala.swing.event

trait UIEvent extends Event {
  val source: UIElement
}

case class UIElementMoved(source: UIElement) extends UIEvent
case class UIElementResized(source: UIElement) extends UIEvent
case class UIElementShown(source: UIElement) extends UIEvent
case class UIElementHidden(source: UIElement) extends UIEvent