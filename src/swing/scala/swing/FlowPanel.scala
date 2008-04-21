package scala.swing

import java.awt.FlowLayout

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
 * @see java.awt.FlowLayout
 */
class FlowPanel(override val peer: javax.swing.JPanel) extends Panel(peer) with SequentialContainer.Wrapper {
  def this(alignment: FlowPanel.Alignment.Value) = this(new javax.swing.JPanel(new java.awt.FlowLayout(alignment.id)))
  def this() = this(FlowPanel.Alignment.Center)
  private def layoutManager = peer.getLayout.asInstanceOf[java.awt.FlowLayout]

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
