/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

abstract class ContainerEvent(override val source: Container) extends UIEvent

case class ComponentAdded(override val source: Container, child: Component) extends ContainerEvent(source)
case class ComponentRemoved(override val source: Container, child: Component) extends ContainerEvent(source)
