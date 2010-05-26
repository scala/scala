/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

trait ComponentEvent extends UIEvent {
  val source: Component
}

@deprecated("Use UIElementMoved instead.")
case class ComponentMoved(source: Component) extends ComponentEvent
@deprecated("Use UIElementResized instead.")
case class ComponentResized(source: Component) extends ComponentEvent
@deprecated("Use UIElementShown instead.")
case class ComponentShown(source: Component) extends ComponentEvent
@deprecated("Use UIElementHidden instead.")
case class ComponentHidden(source: Component) extends ComponentEvent
