package swing

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

class Table(val jtable: JTable) extends Container(jtable) with SwingComponent with Publisher {
  import Table._
  def this() = this(new JTable())
  def this(numRows: Int, numColumns: Int) = this(new JTable(numRows, numColumns))
  def this(rowData: Array[Array[AnyRef]], columnNames: Array[AnyRef]) = this(new JTable(rowData, columnNames))
  //todo: create constructor with List[List[Any]]

  def this(dm: TableModel) = this(new JTable(dm))
  def this(dm: TableModel, cm: TableColumnModel) = this(new JTable(dm, cm))
  def this(dm: TableModel, cm: TableColumnModel, sm: ListSelectionModel) = this(new JTable(dm, cm, sm))

  def rowHeight = jtable.getRowHeight
  def rowHeight_=(x: Int) = jtable.setRowHeight(x)

  def rowCount = jtable.getRowCount

  def model = jtable.getModel()
  def model_=(x: TableModel) = jtable.setModel(x)

  def autoResizeMode: AutoResizeMode.Value = AutoResizeMode(jtable.getAutoResizeMode)
  def autoResizeMode_=(x: Table.AutoResizeMode.Value) = jtable.setAutoResizeMode(x.id)

  def showGrid = jtable.getShowHorizontalLines && jtable.getShowVerticalLines
  def showGrid_=(grid: Boolean) = jtable.setShowGrid(grid)

  def gridColor = new swing.Color(jtable.getGridColor)
  def gridColor_=(color: swing.Color) = jtable.setGridColor(color)

  private val initialRenderer = jtable.getDefaultRenderer(classOf[AnyRef])

  protected def render(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
    new AWTComponentWrapper(
        initialRenderer.getTableCellRendererComponent(jtable, jtable.getValueAt(row, column), isSelected, hasFocus, row, column))

  jtable.setDefaultRenderer(classOf[AnyRef], new TableCellRenderer {
      def getTableCellRendererComponent(jtable: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) =
        render(isSelected, hasFocus, row, column).acomponent
    })

  def apply(row: Int, column: Int) = jtable.getValueAt(row, column)
  def update(row: Int, column: Int, value: AnyRef) = jtable.setValueAt(value, row, column)

  def markUpdated(row: Int, column: Int) = update(row, column, apply(row, column))

  /*
  jtable.addActionListener {
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
