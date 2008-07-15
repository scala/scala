package scala.swing.event

case class ButtonClicked(override val source: AbstractButton) extends ComponentEvent with ActionEvent

