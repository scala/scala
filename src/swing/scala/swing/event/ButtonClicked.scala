package scala.swing.event

case class ButtonClicked(override val source: Button) extends ComponentEvent(source)

