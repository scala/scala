/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import javax.swing._

/**
 * A two state button with a push button like user interface.
 * Usually used in tool bars.
 *
 * @see javax.swing.JToggleButton
 */
class ToggleButton(text0: String) extends AbstractButton {
  override lazy val peer: JToggleButton = new JToggleButton(text0) with SuperMixin
  def this() = this("")
}
