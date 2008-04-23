package scala.swing

import javax.swing._
import javax.swing.table._
import javax.swing.event._
import java.awt.{Dimension, Color}
import event._
import scala.collection.mutable.Set

object Table {
  object AutoResizeMode extends Enumeration {
    import JTable._
    val Off = Value(AUTO_RESIZE_OFF, "Off")
    val NextColumn = Value(AUTO_RESIZE_NEXT_COLUMN, "NextColumn")
    val SubsequentColumns = Value(AUTO_RESIZE_SUBSEQUENT_COLUMNS, "SubsequentColumns")
    val LastColumn = Value(AUTO_RESIZE_LAST_COLUMN, "LastColumn")
    val AllColumns = Value(AUTO_RESIZE_ALL_COLUMNS, "AllColumns")
  }

  object IntervalMode extends Enumeration {
    val Single = Value(ListSelectionModel.SINGLE_SELECTION)
    val SingleInterval = Value(ListSelectionModel.SINGLE_INTERVAL_SELECTION)
    val MultiInterval = Value(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
  }
  object ElementMode extends Enumeration {
    val Row, Column, Cell, None = Value
  }
}

/**
 * @see javax.swing.JTable
 */
class Table extends Component with Scrollable with Publisher {
  override lazy val peer: JTable = new JTable {
    override def getCellRenderer(r: Int, c: Int) = new TableCellRenderer {
      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) =
        renderer(isSelected, hasFocus, row, column).peer
    }
    override def getCellEditor(r: Int, c: Int) = editor(r, c)
  }
  import Table._

  def this(rowData: Array[Array[Any]], columnNames: Seq[Any]) = {
    this()
    peer.setModel(new AbstractTableModel {
            override def getColumnName(column: Int) = columnNames(column).toString
            def getRowCount() = rowData.length
            def getColumnCount() = columnNames.length
            def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
            override def isCellEditable(row: Int, column: Int) = true
            override def setValueAt(value: Any, row: Int, col: Int) {
                rowData(row)(col) = value
                fireTableCellUpdated(row, col)
            }
        })
    }

  protected def scrollablePeer = peer

  def rowHeight = peer.getRowHeight
  def rowHeight_=(x: Int) = peer.setRowHeight(x)

  def rowCount = peer.getRowCount

  def model = peer.getModel()
  def model_=(x: TableModel) = peer.setModel(x)

  def autoResizeMode: AutoResizeMode.Value = AutoResizeMode(peer.getAutoResizeMode)
  def autoResizeMode_=(x: Table.AutoResizeMode.Value) = peer.setAutoResizeMode(x.id)

  def showGrid = peer.getShowHorizontalLines && peer.getShowVerticalLines
  def showGrid_=(grid: Boolean) = peer.setShowGrid(grid)

  def gridColor = peer.getGridColor
  def gridColor_=(color: Color) = peer.setGridColor(color)

  def preferredViewportSize_=(dim: Dimension) = peer.setPreferredScrollableViewportSize(dim)
  //1.6: def fillsViewportHeight: Boolean = peer.getFillsViewportHeight
  //def fillsViewportHeight_=(b: Boolean) = peer.setFillsViewportHeight(b)

  // TODO: could be a sorted set
  protected abstract class SelectionSet[A](a: =>Seq[A]) extends Set[A] {
    def -=(n: A)
    def +=(n: A)
    def contains(n: A) = a.contains(n)
    def size = a.length
    def elements = a.elements
  }

  class Selection extends Publisher {
    object rows extends SelectionSet(peer.getSelectedRows) {
      def -=(n: Int) { peer.removeRowSelectionInterval(n,n) }
      def +=(n: Int) { peer.addRowSelectionInterval(n,n) }

      def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex
    }

    object columns extends SelectionSet(peer.getSelectedColumns) {
      def -=(n: Int) { peer.removeColumnSelectionInterval(n,n) }
      def +=(n: Int) { peer.addColumnSelectionInterval(n,n) }

      def leadIndex: Int = peer.getColumnModel.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getColumnModel.getSelectionModel.getAnchorSelectionIndex
    }

    def cells: Set[(Int, Int)] =
      new SelectionSet[(Int, Int)]((for(r <- selection.rows; c <- selection.columns) yield (r,c)).toSeq) { outer =>
        def -=(n: (Int, Int)) {
          peer.removeRowSelectionInterval(n._1,n._1)
          peer.removeColumnSelectionInterval(n._2,n._2)
        }
        def +=(n: (Int, Int)) {
          peer.addRowSelectionInterval(n._1,n._1)
          peer.addColumnSelectionInterval(n._2,n._2)
        }
        override def size = peer.getSelectedRowCount * peer.getSelectedColumnCount
      }

    /**
     * From the JTable Swing tutorial:
     * You can specify selection by cell in multiple interval selection mode,
     * but the result is a table that does not produce useful selections.
     */
    def intervalMode: IntervalMode.Value = IntervalMode(peer.getSelectionModel.getSelectionMode)
    def intervalMode_=(m: IntervalMode.Value) { peer.getSelectionModel.setSelectionMode(m.id) }
    def elementMode: ElementMode.Value =
      if(peer.getColumnSelectionAllowed && peer.getRowSelectionAllowed) ElementMode.Cell
      else if(peer.getColumnSelectionAllowed) ElementMode.Column
      else if(peer.getRowSelectionAllowed) ElementMode.Row
      else ElementMode.None
    def elementMode_=(m: ElementMode.Value) {
      m match {
        case ElementMode.Cell => peer.setCellSelectionEnabled(true)
        case ElementMode.Column => peer.setRowSelectionAllowed(false); peer.setColumnSelectionAllowed(true)
        case ElementMode.Row => peer.setRowSelectionAllowed(true); peer.setColumnSelectionAllowed(false)
        case ElementMode.None => peer.setRowSelectionAllowed(false); peer.setColumnSelectionAllowed(false)
      }
    }

    peer.getColumnModel.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent) {
        publish(ColumnsSelected(Table.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
    peer.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent) {
        publish(RowsSelected(Table.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
  }

  val selection: Selection = new Selection

  private val initialRenderer = peer.getDefaultRenderer(classOf[AnyRef])

  /**
   * Supplies a renderer component for a given cell.
   */
  def renderer(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component =
    new Component {
      override lazy val peer = {
        val v = Table.this.peer.getValueAt(row, column)
        Table.this.peer.getDefaultRenderer(v.getClass).getTableCellRendererComponent(Table.this.peer,
                 v, isSelected, hasFocus, row, column).asInstanceOf[JComponent]
      }
    }

  protected def editor(row: Int, column: Int) =
    Table.this.peer.getDefaultEditor(Table.this.peer.getValueAt(row, column).getClass)

  def apply(row: Int, column: Int) = peer.getValueAt(row, column)
  def update(row: Int, column: Int, value: Any) = peer.setValueAt(value, row, column)

  def markUpdated(row: Int, column: Int) = update(row, column, apply(row, column))

  model.addTableModelListener(new TableModelListener {
    def tableChanged(event: TableModelEvent) = publish(
      if (event.getType == TableModelEvent.UPDATE)
        TableChanged(Table.this, event.getFirstRow, event.getLastRow, event.getColumn)
      else
    	TableResized(Table.this)
    )
  })
}
