/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing
package event

trait ComponentEvent extends UIEvent {
  override val source: Component
}

@deprecated("Use UIElementMoved instead.")
case class ComponentMoved(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementResized instead.")
case class ComponentResized(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementShown instead.")
case class ComponentShown(override val source: Component) extends ComponentEvent
@deprecated("Use UIElementHidden instead.")
case class ComponentHidden(override val source: Component) extends ComponentEvent
