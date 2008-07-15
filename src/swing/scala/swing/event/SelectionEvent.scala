package scala.swing.event

trait SelectionEvent extends ComponentEvent

trait ListSelectionEvent extends SelectionEvent {
  def range: Range
}

case class SelectionChanged(override val source: Component) extends ComponentEvent with SelectionEvent
case class ListSelectionChanged[A](override val source: ListView[A], range: Range, live: Boolean)
  extends SelectionChanged(source) with ListEvent[A]