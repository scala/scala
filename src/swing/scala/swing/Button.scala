/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import javax.swing._

object Button {
  def apply(text0: String)(op: => Unit) = new Button(Action(text0)(op))
}

/**
 * A button that can be clicked, usually to perform some action.
 *
 * @see javax.swing.JButton
 */
class Button(text0: String) extends AbstractButton with Publisher {
  override lazy val peer: JButton = new JButton(text0) with SuperMixin
  def this() = this("")
  def this(a: Action) = {
    this("")
    action = a
  }

  def defaultButton: Boolean = peer.isDefaultButton

  def defaultCapable: Boolean = peer.isDefaultCapable
  def defaultCapable_=(capable: Boolean) { peer.setDefaultCapable(capable) }
}
