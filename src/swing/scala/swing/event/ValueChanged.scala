package scala.swing.event

case class ValueChanged(override val source: Component, live: Boolean) extends ComponentEvent(source) with LiveEvent
