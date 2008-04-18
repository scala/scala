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

class FlowPanel(alignment: FlowPanel.Alignment.Value) extends Panel with IndexedContainer {
  def this() = this(FlowPanel.Alignment.Center)
  override lazy val layoutManager = new java.awt.FlowLayout(alignment.id)
  override lazy val peer = new javax.swing.JPanel(layoutManager)

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
