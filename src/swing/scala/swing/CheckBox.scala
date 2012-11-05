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
 * Two state button that can either be checked or unchecked.
 *
 * @see javax.swing.JCheckBox
 */
class CheckBox(text: String) extends ToggleButton {
  override lazy val peer: JCheckBox = new JCheckBox(text) with SuperMixin
  def this() = this("")

  def borderPaintedFlat: Boolean = peer.isBorderPaintedFlat
  def borderPaintedFlat_=(flat: Boolean) { peer.setBorderPaintedFlat(flat) }
}
