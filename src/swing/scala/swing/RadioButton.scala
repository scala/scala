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
 * A two state button that is usually used in a <code>ButtonGroup</code>
 * together with other <code>RadioButton</code>s, in order to indicate
 * that at most one of them can be selected.
 *
 * @see javax.swing.JRadioButton
 */
class RadioButton(text0: String) extends ToggleButton {
  override lazy val peer: JRadioButton = new JRadioButton(text0) with SuperMixin
  def this() = this("")
}
