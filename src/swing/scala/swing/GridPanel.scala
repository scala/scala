package scala.swing

object GridPanel {
  val Adapt = 0
}

/**
 * @see java.awt.GridLayout
 */
class GridPanel(rows0: Int, cols0: Int) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = new javax.swing.JPanel(new java.awt.GridLayout(rows0, cols0))

  private def layoutManager = peer.getLayout.asInstanceOf[java.awt.GridLayout]

  def rows: Int = layoutManager.getRows
  def rows_=(n: Int) { layoutManager.setRows(n) }
  def columns: Int = layoutManager.getColumns
  def columns_=(n: Int) { layoutManager.setColumns(n) }

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
