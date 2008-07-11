package scala.swing.event

object ActionEvent {
  def unapply(e: ActionEvent): Option[Component] = Some(e.source)
}

trait ActionEvent extends ComponentEvent
