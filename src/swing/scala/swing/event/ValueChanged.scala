package scala.swing.event

case class ValueChanged(override val source: Component)(val adjusting: Boolean) extends ComponentEvent with AdjustingEvent
