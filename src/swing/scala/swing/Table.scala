package scala.swing

import model.Matrix
import javax.swing._
import javax.swing.table._
import javax.swing.event._
import event._

object Table {
  object AutoResizeMode extends Enumeration {
    import JTable._
    val Off = Value(AUTO_RESIZE_OFF, "Off")
    val NextColumn = Value(AUTO_RESIZE_NEXT_COLUMN, "NextColumn")
    val SubsequentColumns = Value(AUTO_RESIZE_SUBSEQUENT_COLUMNS, "SubsequentColumns")
    val LastColumn = Value(AUTO_RESIZE_LAST_COLUMN, "LastColumn")
    val AllColumns = Value(AUTO_RESIZE_ALL_COLUMNS, "AllColumns")
  }
}

class Table(override val peer: JTable) extends Component with Publisher {
  import Table._
  def this() = this(new JTable())
  def this(numRows: Int, numColumns: Int) = this(new JTable(numRows, numColumns))
  def this(rowData: Array[Array[AnyRef]], columnNames: Array[AnyRef]) = this(new JTable(rowData, columnNames))
  //todo: create constructor with List[List[Any]]

  def this(dm: TableModel) = this(new JTable(dm))
  def this(dm: TableModel, cm: TableColumnModel) = this(new JTable(dm, cm))
  def this(dm: TableModel, cm: TableColumnModel, sm: ListSelectionModel) = this(new JTable(dm, cm, sm))

  def rowHeight = peer.getRowHeight
  def rowHeight_=(x: Int) = peer.setRowHeight(x)

  def rowCount = peer.getRowCount

  def model = peer.getModel()
  def model_=(x: TableModel) = peer.setModel(x)

  def autoResizeMode: AutoResizeMode.Value = AutoResizeMode(peer.getAutoResizeMode)
  def autoResizeMode_=(x: Table.AutoResizeMode.Value) = peer.setAutoResizeMode(x.id)

  def showGrid = peer.getShowHorizontalLines && peer.getShowVerticalLines
  def showGrid_=(grid: Boolean) = peer.setShowGrid(grid)

  def gridColor = new swing.Color(peer.getGridColor)
  def gridColor_=(color: swing.Color) = peer.setGridColor(color)

  private val initialRenderer = peer.getDefaultRenderer(classOf[AnyRef])

  protected def render(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
    new Component {
      override lazy val peer = initialRenderer.getTableCellRendererComponent(Table.this.peer,
                 Table.this.peer.getValueAt(row, column), isSelected, hasFocus, row, column).asInstanceOf[JComponent]
    }

  peer.setDefaultRenderer(classOf[AnyRef], new TableCellRenderer {
      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) =
        render(isSelected, hasFocus, row, column).peer
    })

  def apply(row: Int, column: Int) = peer.getValueAt(row, column)
  def update(row: Int, column: Int, value: AnyRef) = peer.setValueAt(value, row, column)

  def markUpdated(row: Int, column: Int) = update(row, column, apply(row, column))

  /*
  peer.addActionListener {
    new java.awt.event.ActionListener {
      def actionPerformed(e: java.awt.event.ActionEvent): unit =
        publish(ButtonPressed(Button.this))
    }
  }
*/
  model.addTableModelListener(new TableModelListener {
    def tableChanged(event: TableModelEvent) = publish(
      if (event.getType == TableModelEvent.UPDATE)
        TableChanged(Table.this, event.getFirstRow, event.getLastRow, event.getColumn)
      else
    	TableResized(Table.this)
    )
  })
}
