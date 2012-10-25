/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

abstract class PopupMenuEvent extends ComponentEvent

case class PopupMenuCanceled(source: PopupMenu) extends PopupMenuEvent
case class PopupMenuWillBecomeInvisible(source: PopupMenu) extends PopupMenuEvent
case class PopupMenuWillBecomeVisible(source: PopupMenu) extends PopupMenuEvent