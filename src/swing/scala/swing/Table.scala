/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import event._
import javax.swing._
import javax.swing.table._
import javax.swing.event._
import scala.collection.mutable

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

  /**
   * A table item renderer.
   *
   * @see javax.swing.table.TableCellRenderer
   */
  abstract class Renderer[-A] {
    def peer: TableCellRenderer = new TableCellRenderer {
      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) = {
        componentFor(table match {
          case t: JTableMixin => t.tableWrapper
          case _ => assert(false); null
        }, isSelected, hasFocus, value.asInstanceOf[A], row, column).peer
      }
    }
    def componentFor(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int): Component
  }

  abstract class AbstractRenderer[-A, C<:Component](val component: C) extends Renderer[A] {
    // The renderer component is responsible for painting selection
    // backgrounds. Hence, make sure it is opaque to let it draw
    // the background.
    component.opaque = true

    /**
     * Standard preconfiguration that is commonly done for any component.
     */
    def preConfigure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int) {
      if (isSelected) {
        component.background = table.selectionBackground
        component.foreground = table.selectionForeground
      } else {
        component.background = table.background
        component.foreground = table.foreground
      }
    }
    /**
     * Configuration that is specific to the component and this renderer.
     */
    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int)

    /**
     * Configures the component before returning it.
     */
    def componentFor(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int): Component = {
      preConfigure(table, isSelected, hasFocus, a, row, column)
      configure(table, isSelected, hasFocus, a, row, column)
      component
    }
  }

  class LabelRenderer[A](convert: A => (Icon, String)) extends AbstractRenderer[A, Label](new Label) {
    def this() {
      this{ a => (null, a.toString) }
    }

    def configure(table: Table, isSelected: Boolean, hasFocus: Boolean, a: A, row: Int, column: Int) {
      val (icon, text) = convert(a)
      component.icon = icon
      component.text = text
    }
  }

  private[swing] trait JTableMixin { def tableWrapper: Table }
}

/**
 * Displays a matrix of items.
 *
 * To obtain a scrollable table or row and columns headers,
 * wrap the table in a scroll pane.
 *
 * @see javax.swing.JTable
 */
class Table extends Component with Scrollable.Wrapper {
  override lazy val peer: JTable = new JTable with Table.JTableMixin with SuperMixin {
    def tableWrapper = Table.this
    override def getCellRenderer(r: Int, c: Int) = new TableCellRenderer {
      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) =
        Table.this.rendererComponent(isSelected, hasFocus, row, column).peer
    }
    override def getCellEditor(r: Int, c: Int) = editor(r, c)
    override def getValueAt(r: Int, c: Int) = Table.this.apply(r,c).asInstanceOf[AnyRef]
  }
  import Table._

  // TODO: use IndexedSeq[_ <: IndexedSeq[Any]], see ticket #2005
  def this(rowData: Array[Array[Any]], columnNames: Seq[_]) = {
    this()
    model = new AbstractTableModel {
      override def getColumnName(column: Int) = columnNames(column).toString
      def getRowCount() = rowData.length
      def getColumnCount() = columnNames.length
      def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
      override def isCellEditable(row: Int, column: Int) = true
      override def setValueAt(value: Any, row: Int, col: Int) {
        rowData(row)(col) = value
        fireTableCellUpdated(row, col)
      }
    }
  }
  def this(rows: Int, columns: Int) = {
    this()
    model = new DefaultTableModel(rows, columns) {
      override def setValueAt(value: Any, row: Int, col: Int) {
        super.setValueAt(value, row, col)
      }
    }
  }

  protected def scrollablePeer = peer

  def rowHeight = peer.getRowHeight
  def rowHeight_=(x: Int) = peer.setRowHeight(x)

  def rowCount = peer.getRowCount

  def model = peer.getModel()
  def model_=(x: TableModel) = {
    peer.setModel(x)
    model.removeTableModelListener(modelListener)
    model.addTableModelListener(modelListener)
  }

  def autoResizeMode: AutoResizeMode.Value = AutoResizeMode(peer.getAutoResizeMode)
  def autoResizeMode_=(x: Table.AutoResizeMode.Value) = peer.setAutoResizeMode(x.id)

  def showGrid = peer.getShowHorizontalLines && peer.getShowVerticalLines
  def showGrid_=(grid: Boolean) = peer.setShowGrid(grid)

  def gridColor = peer.getGridColor
  def gridColor_=(color: Color) = peer.setGridColor(color)

  def preferredViewportSize_=(dim: Dimension) = peer.setPreferredScrollableViewportSize(dim)
  //1.6: def fillsViewportHeight: Boolean = peer.getFillsViewportHeight
  //def fillsViewportHeight_=(b: Boolean) = peer.setFillsViewportHeight(b)

  object selection extends Publisher {
    // TODO: could be a sorted set
    protected abstract class SelectionSet[A](a: =>Seq[A]) extends mutable.Set[A] {
      def -=(n: A): this.type
      def +=(n: A): this.type
      def contains(n: A) = a.contains(n)
      override def size = a.length
      def iterator = a.iterator
    }

    object rows extends SelectionSet(peer.getSelectedRows) {
      def -=(n: Int) = { peer.removeRowSelectionInterval(n,n); this }
      def +=(n: Int) = { peer.addRowSelectionInterval(n,n); this }

      def leadIndex: Int = peer.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getSelectionModel.getAnchorSelectionIndex
    }

    object columns extends SelectionSet(peer.getSelectedColumns) {
      def -=(n: Int) = { peer.removeColumnSelectionInterval(n,n); this }
      def +=(n: Int) = { peer.addColumnSelectionInterval(n,n); this }

      def leadIndex: Int = peer.getColumnModel.getSelectionModel.getLeadSelectionIndex
      def anchorIndex: Int = peer.getColumnModel.getSelectionModel.getAnchorSelectionIndex
    }

    def cells: mutable.Set[(Int, Int)] =
      new SelectionSet[(Int, Int)]((for(r <- selection.rows; c <- selection.columns) yield (r,c)).toSeq) { outer =>
        def -=(n: (Int, Int)) = {
          peer.removeRowSelectionInterval(n._1,n._1)
          peer.removeColumnSelectionInterval(n._2,n._2)
          this
        }
        def +=(n: (Int, Int)) = {
          peer.addRowSelectionInterval(n._1,n._1)
          peer.addColumnSelectionInterval(n._2,n._2)
          this
        }
        override def size = peer.getSelectedRowCount * peer.getSelectedColumnCount
      }

    /**
     * From the JTable Swing tutorial:
     * You can specify selection by cell in multiple interval selection mode,
     * but the result is a table that does not produce useful selections.
     */
    def intervalMode: IntervalMode.Value = IntervalMode(peer.getSelectionModel.getSelectionMode)
    def intervalMode_=(m: IntervalMode.Value) { peer.setSelectionMode(m.id) }
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
        publish(TableColumnsSelected(Table.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
    peer.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: javax.swing.event.ListSelectionEvent) {
        publish(TableRowsSelected(Table.this, e.getFirstIndex to e.getLastIndex, e.getValueIsAdjusting))
      }
    })
  }

  /**
   * Supplies a renderer component for a given cell.
   */
  protected def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component =
    new Component {
      override lazy val peer = {
        val v = apply(row, column).asInstanceOf[AnyRef]
        if (v != null)
          Table.this.peer.getDefaultRenderer(v.getClass).getTableCellRendererComponent(Table.this.peer,
                 v, isSelected, focused, row, column).asInstanceOf[JComponent]
        else Table.this.peer.getDefaultRenderer(classOf[Object]).getTableCellRendererComponent(Table.this.peer,
                 v, isSelected, focused, row, column).asInstanceOf[JComponent]
      }
    }

  // TODO: a public API for setting editors
  protected def editor(row: Int, column: Int) = {
    val v = apply(row, column).asInstanceOf[AnyRef]
    if (v != null)
      Table.this.peer.getDefaultEditor(v.getClass)
    else
      Table.this.peer.getDefaultEditor(classOf[Object])
  }

  /**
   * Get the current value of the given cell.
   * The given cell coordinates are in view coordinates and thus not
   * necessarily the same as for the model.
   */
  def apply(row: Int, column: Int): Any = model.getValueAt(row, viewToModelColumn(column))

  // TODO: this is Java 6 stuff
  // def apply(row: Int, column: Int): Any = model.getValueAt(viewToModelRow(row), viewToModelColumn(column))
  //def viewToModelRow(idx: Int) = peer.convertRowIndexToModel(idx)
  //def modelToViewRow(idx: Int) = peer.convertRowIndexToView(idx)

  def viewToModelColumn(idx: Int) = peer.convertColumnIndexToModel(idx)
  def modelToViewColumn(idx: Int) = peer.convertColumnIndexToView(idx)


  /**
   * Change the value of the given cell.
   */
  def update(row: Int, column: Int, value: Any) { model.setValueAt(value, row, viewToModelColumn(column)) }

  /**
   * Visually update the given cell.
   */
  def updateCell(row: Int, column: Int) = update(row, column, apply(row, column))

  def selectionForeground: Color = peer.getSelectionForeground
  def selectionForeground_=(c: Color) = peer.setSelectionForeground(c)
  def selectionBackground: Color = peer.getSelectionBackground
  def selectionBackground_=(c: Color) = peer.setSelectionBackground(c)

  protected val modelListener = new TableModelListener {
    def tableChanged(e: TableModelEvent) = publish(
      e.getType match {
        case TableModelEvent.UPDATE =>
          if (e.getFirstRow == 0 && e.getLastRow == Int.MaxValue && e.getColumn == TableModelEvent.ALL_COLUMNS)
            TableChanged(Table.this)
          else if (e.getFirstRow == TableModelEvent.HEADER_ROW)
            TableStructureChanged(Table.this)
          else
            TableUpdated(Table.this, e.getFirstRow to e.getLastRow, e.getColumn)
        case TableModelEvent.INSERT =>
          TableRowsAdded(Table.this, e.getFirstRow to e.getLastRow)
        case TableModelEvent.DELETE =>
          TableRowsRemoved(Table.this, e.getFirstRow to e.getLastRow)
      }
    )
  }
}
