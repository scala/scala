/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing.event

trait SelectionEvent extends ComponentEvent

trait ListSelectionEvent extends SelectionEvent {
  def range: Range
}

case class SelectionChanged(override val source: Component) extends ComponentEvent with SelectionEvent
case class ListSelectionChanged[A](override val source: ListView[A], range: Range, live: Boolean)
  extends SelectionChanged(source) with ListEvent[A]
