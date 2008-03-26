package swing

object GridPanel {
  val Adapt = 0
}

class GridPanel(rows0: Int, cols0: Int)(content0: Component*) extends IndexedPanel {
  override lazy val layout = new java.awt.GridLayout(rows0, cols0)
  override lazy val peer = new javax.swing.JPanel(layout)
  content ++ content0

  def rows: Int = layout.getRows
  def columns: Int = layout.getColumns
}
