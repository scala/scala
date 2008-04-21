package scala.swing.event

trait SelectionEvent extends UIEvent

trait ListSelectionEvent extends SelectionEvent {
  def range: Range
}

case class SelectionChanged(override val source: Component) extends UIEvent(source) with SelectionEvent