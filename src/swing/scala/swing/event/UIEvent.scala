/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

trait UIEvent extends Event {
  val source: UIElement
}

case class UIElementMoved(source: UIElement) extends UIEvent
case class UIElementResized(source: UIElement) extends UIEvent
case class UIElementShown(source: UIElement) extends UIEvent
case class UIElementHidden(source: UIElement) extends UIEvent
