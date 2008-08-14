package scala.swing.event

case class EditDone(override val source: TextField) extends ValueChanged(source)
