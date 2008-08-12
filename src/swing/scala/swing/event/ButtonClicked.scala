package scala.swing.event

case class ButtonClicked(override val source: AbstractButton) extends ActionEvent(source)

