package scala.swing.event

class TableEvent(override val source: Table) extends ComponentEvent(source)

case class TableChanged(override val source: Table, firstRow: Int, lastRow: Int, column: Int)
           extends TableEvent(source)
case class TableResized(override val source: Table) extends TableEvent(source)

case class ColumnsSelected(override val source: Table, range: Range, live: Boolean)
           extends TableEvent(source) with LiveEvent with ListSelectionEvent
case class RowsSelected(override val source: Table, range: Range, live: Boolean)
           extends TableEvent(source) with LiveEvent with ListSelectionEvent