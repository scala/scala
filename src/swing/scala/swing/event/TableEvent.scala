/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing
package event

abstract class TableEvent(override val source: Table) extends ComponentEvent

abstract class TableChange(override val source: Table) extends TableEvent(source)

/**
 * The most general table change. The table might have changed completely,
 * i.e., columns might have been reordered, rows added or removed, etc.
 * No other event indicates that the structure might have changed.
 */
case class TableStructureChanged(override val source: Table) extends TableChange(source)
/**
 * The table structure, i.e., the column order, names, and types stay the same,
 * but anything else might have changed.
 */
case class TableChanged(override val source: Table) extends TableChange(source)
/**
 * The size of the table stays the same, but the given range of rows might
 * have changed but only in the given column. A value of -1 for the column
 * denotes all columns.
 */
case class TableUpdated(override val source: Table, range: Range, column: Int)
           extends TableChange(source)
/**
 * Any change that caused the table to change it's size
 */
class TableResized(override val source: Table) extends TableChange(source)
case class TableRowsAdded(override val source: Table, range: Range) extends TableResized(source)
case class TableRowsRemoved(override val source: Table, range: Range) extends TableResized(source)

case class TableColumnsSelected(override val source: Table, range: Range, adjusting: Boolean)
           extends TableEvent(source) with AdjustingEvent with ListSelectionEvent
case class TableRowsSelected(override val source: Table, range: Range, adjusting: Boolean)
           extends TableEvent(source) with AdjustingEvent with ListSelectionEvent
