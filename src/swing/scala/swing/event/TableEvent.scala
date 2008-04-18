package scala.swing.event

class TableEvent(override val source: Table) extends ComponentEvent(source)

case class TableChanged(override val source: Table, firstRow: Int, lastRow: Int, column: Int) extends TableEvent(source)
case class TableResized(override val source: Table) extends TableEvent(source)
//case class TextModified(override val source: TextComponent) extends ContentModified(source)

case class ColumnsSelected(override val source: Table, live: Boolean, range: Range) extends TableEvent(source) with LiveEvent with SelectionEvent
case class RowsSelected(override val source: Table, live: Boolean, range: Range) extends TableEvent(source) with LiveEvent with SelectionEvent