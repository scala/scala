package scala.swing.event

case class ButtonPressed(override val source: Button) extends ComponentEvent(source)
