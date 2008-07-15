package scala.swing.event

trait UIEvent extends Event {
  val source: UIElement
}
