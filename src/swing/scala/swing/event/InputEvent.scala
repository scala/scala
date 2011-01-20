/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

trait InputEvent extends ComponentEvent {
  def peer: java.awt.event.InputEvent
  def when: Long = peer.getWhen
  def modifiers: Key.Modifiers
  def consume() { peer.consume() }
  def consumed: Boolean = peer.isConsumed
}
