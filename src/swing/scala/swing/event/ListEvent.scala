package scala.swing.event

abstract class ListEvent[A](override val source: ListView[A]) extends ComponentEvent(source)

case class ElementSelected[A](override val source: ListView[A], range: Range, live: Boolean)
           extends ListEvent(source) with LiveEvent with ListSelectionEvent

abstract class ListChange[A](override val source: ListView[A]) extends ListEvent(source)

case class ListChanged[A](override val source: ListView[A]) extends ListChange(source)
case class ListElementsAdded[A](override val source: ListView[A], range: Range)
           extends ListChange(source)
case class ListElementsRemoved[A](override val source: ListView[A], range: Range)
           extends ListChange(source)