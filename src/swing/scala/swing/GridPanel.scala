package scala.swing

object GridPanel {
  val Adapt = 0
}

class GridPanel(rows0: Int, cols0: Int) extends Panel with IndexedContainer {
  override lazy val layoutManager = new java.awt.GridLayout(rows0, cols0)
  override lazy val peer = new javax.swing.JPanel(layoutManager)

  def rows: Int = layoutManager.getRows
  def rows_=(n: Int) { layoutManager.setRows(n) }
  def columns: Int = layoutManager.getColumns
  def columns_=(n: Int) { layoutManager.setColumns(n) }

  def vGap: Int = layoutManager.getVgap
  def vGap_=(n: Int) { layoutManager.setVgap(n) }
  def hGap: Int = layoutManager.getHgap
  def hGap_=(n: Int) { layoutManager.setHgap(n) }
}
