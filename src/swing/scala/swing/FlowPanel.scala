/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import java.awt.FlowLayout
import javax.swing.JPanel

object FlowPanel {
  object Alignment extends Enumeration {
    val Leading = Value(FlowLayout.LEADING)
    val Trailing = Value(FlowLayout.TRAILING)
    val Left = Value(FlowLayout.LEFT)
    val Right = Value(FlowLayout.RIGHT)
    val Center = Value(FlowLayout.CENTER)
  }
}

/**
 * A panel that arranges its contents horizontally, one after the other.
 * If they don't fit, this panel will try to insert line breaks.
 *
 * @see java.awt.FlowLayout
 */
class FlowPanel(alignment: FlowPanel.Alignment.Value)(contents0: Component*) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer: JPanel =
    new JPanel(new java.awt.FlowLayout(alignment.id)) with SuperMixin
  def this(contents0: Component*) = this(FlowPanel.Alignment.Center)(contents0: _*)
  def this() = this(FlowPanel.Alignment.Center)()

  contents ++= contents0

  private def layoutManager = peer.getLayout.asInstanceOf[java.awt.FlowLayout]

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
