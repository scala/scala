/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

/**
 * An event that indicates a change in a selection such as in a list view or a table.
 */
trait SelectionEvent

/**
 * An event that indicates a selection of a range of indices.
 */
trait ListSelectionEvent extends SelectionEvent {
  def range: Range
}

case class SelectionChanged(override val source: Component) extends ComponentEvent with SelectionEvent

object ListSelectionChanged {
  def unapply[A](e: ListSelectionChanged[A]): Option[(ListView[A], Range, Boolean)] =
    Some((e.source, e.range, e.live))
}

class ListSelectionChanged[A](override val source: ListView[A], val range: Range, val live: Boolean)
  extends SelectionChanged(source) with ListEvent[A]
