package scala.swing

import javax.swing.table.{AbstractTableModel, TableCellRenderer}
import javax.swing.event.{TableModelListener, TableModelEvent}
import javax.swing._
import event._

/*class Spreadsheet(width: Int, height: Int) extends Component with SwingComponent with Publisher {


  def showGrid: Boolean = acomponent.table.getShowHorizontalLines && acomponent.table.getShowVerticalLines
  def showGrid_=(x: Boolean) = acomponent.table.setShowGrid(x)

  case class SpreadsheetCell(var userData: String, var displayData: String)

  val data = model.Matrix[SpreadsheetCell](SpreadsheetCell("", ""), width, height)

  val acomponent = new JScrollPane {
    setPreferredSize(new java.awt.Dimension(800, 600))
    val table = new JTable {
      setSelectionModel(new DefaultListSelectionModel)
      setAutoResizeMode(JTable.AUTO_RESIZE_OFF)
      setRowHeight(25)
      val model = new AbstractTableModel { model =>
        class UserDataEvent(col: Int, row: Int) extends TableModelEvent(model, row, row, col)
        val getRowCount = height
        val getColumnCount = width
        def getValueAt(row: Int, col: Int) =
          data(col, row).userData
        override def setValueAt(value: AnyRef, row: Int, col: Int) = {
          data(col, row).userData = value.toString
          fireTableChanged(new UserDataEvent(col, row))
        }
        def setDisplayValueAt(value: AnyRef, row: Int, col: Int) = {
          data(col, row).displayData = value.toString
          fireTableCellUpdated(row, col)
        }
        override def isCellEditable(row: Int, col: Int) = true
        addTableModelListener(new TableModelListener {
          def tableChanged(event: TableModelEvent) = event match {
            case de:UserDataEvent =>
              publish(CellModified(event.getColumn, event.getFirstRow, data(event.getColumn, event.getFirstRow).userData))
            case _ =>
          }
        })
      }
      setModel(model)
      setDefaultRenderer(classOf[AnyRef], new TableCellRenderer {
        def getTableCellRendererComponent(table: JTable, value: AnyRef, selected: Boolean, focus: Boolean, row: Int, col: Int) =
          if (focus) new JTextField {
            setText(data(col, row).userData)
          }
          else new JLabel {
            setText(data(col, row).displayData)
          }
      })
      getTableHeader.setReorderingAllowed(false)
      getTableHeader.setResizingAllowed(false)
    }
    setViewportView(table)
    setColumnHeaderView(table.getTableHeader)
    setRowHeaderView(new JList {
      setListData(((1 to height) map (_.toString)).toArray.asInstanceOf[Array[Object]])
      setFixedCellWidth(50)
      setCellRenderer(new JLabel with ListCellRenderer {
        setOpaque(true)
        setBorder(UIManager.getBorder("TableHeader.cellBorder"))
        setHorizontalAlignment(SwingConstants.CENTER)
        setForeground(table.getTableHeader.getForeground)
        setBackground(table.getTableHeader.getBackground)
        setFont(table.getTableHeader.getFont)
        def getListCellRendererComponent(list: JList, value: AnyRef, index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
          setPreferredSize(new Dimension(50, table.getRowHeight(index)))
          setText(value.toString)
          this
        }
      })
    })
    def corner = new JLabel{
      setOpaque(true)
      setBorder(UIManager.getBorder("TableHeader.cellBorder"))
      setForeground(table.getTableHeader.getForeground)
      setBackground(table.getTableHeader.getBackground)
    }
    setCorner(ScrollPaneConstants.UPPER_LEFT_CORNER, corner)
    setCorner(ScrollPaneConstants.UPPER_RIGHT_CORNER, corner)
    setCorner(ScrollPaneConstants.LOWER_LEFT_CORNER, corner)
    setCorner(ScrollPaneConstants.UPPER_RIGHT_CORNER, corner)
  }

  def update(col: Int, row: Int, value: Any) = {
    acomponent.table.model.setDisplayValueAt(value.toString, row, col)
  }
//    SwingUtilities.invokeLater(new Runnable {
//      def run = acomponent.table.model.setDisplayValueAt(value.toString, row, col)
//   })

  def apply(pos: (Int, Int)) = data(pos._1, pos._2).userData

}*/
