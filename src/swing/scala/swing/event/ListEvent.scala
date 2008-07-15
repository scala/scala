package scala.swing.event

trait ListEvent[A] extends ComponentEvent {
  override val source: ListView[A]
}

//case class ElementSelected[A](override val source: ListView[A], range: Range, live: Boolean)
//           extends ListEvent[A] with LiveEvent with ListSelectionEvent

abstract class ListChange[A](override val source: ListView[A]) extends ListEvent[A]

case class ListChanged[A](override val source: ListView[A]) extends ListChange(source)
case class ListElementsAdded[A](override val source: ListView[A], range: Range)
           extends ListChange(source)
case class ListElementsRemoved[A](override val source: ListView[A], range: Range)
           extends ListChange(source)