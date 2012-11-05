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
 * The other component is None if it is a non Swing, i.e., AWT or native, component.
 */
abstract class FocusEvent(override val source: Component, val other: Option[Component], val temporary: Boolean) extends ComponentEvent

case class FocusGained(override val source: Component, override val other: Option[Component], override val temporary: Boolean)
           extends FocusEvent(source, other, temporary)

case class FocusLost(override val source: Component, override val other: Option[Component], override val temporary: Boolean)
           extends FocusEvent(source, other, temporary)
