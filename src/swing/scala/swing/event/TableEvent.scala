package scala.swing.event

abstract class TableEvent(override val source: Table) extends ComponentEvent(source)

abstract class TableChange(override val source: Table) extends TableEvent(source)

case class TableChanged(override val source: Table) extends TableChange(source)
case class TableUpdated(override val source: Table, range: Range, column: Int)
           extends TableChange(source)

case class TableResized(override val source: Table) extends TableChange(source)
case class TableRowsAdded(override val source: Table, range: Range) extends TableResized(source)
case class TableRowsRemoved(override val source: Table, range: Range) extends TableResized(source)

case class TableColumnsSelected(override val source: Table, range: Range, live: Boolean)
           extends TableEvent(source) with LiveEvent with ListSelectionEvent
case class TableRowsSelected(override val source: Table, range: Range, live: Boolean)
           extends TableEvent(source) with LiveEvent with ListSelectionEvent