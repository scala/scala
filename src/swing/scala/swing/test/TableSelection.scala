package scala.swing
package test

import java.awt.Dimension
import swing.event._

object TableSelection extends SimpleSwingApplication {
  val model = Array(List("Mary", "Campione", "Snowboarding", 5, false).toArray,
                    List("Alison", "Huml", "Rowing", 5, false).toArray,
                    List("Kathy", "Walrath", "Knitting", 5, false).toArray,
                    List("Sharon", "Zakhour", "Speed reading", 5, false).toArray,
                    List("Philip", "Milne", "Pool", 5, false).toArray)
  /*val model = Array.tabulate(10000) { i =>
    List("Mary", "Campione", "Snowboarding", i, false).toArray
  }*/

  lazy val ui = new BoxPanel(Orientation.Vertical) {
    val table = new Table(model, Array("First Name", "Last Name", "Sport", "# of Years", "Vegetarian")) {
      preferredViewportSize = new Dimension(500, 70)
    }
    //1.6:table.fillsViewportHeight = true
    listenTo(table.selection)

    contents += new ScrollPane(table)
    contents += new Label("Selection Mode")

    def radio(mutex: ButtonGroup, text: String): RadioButton = {
      val b = new RadioButton(text)
      listenTo(b)
      mutex.buttons += b
      contents += b
      b
    }

    val intervalMutex = new ButtonGroup
    val multiInterval = radio(intervalMutex, "Multiple Interval Selection")
    val elementInterval = radio(intervalMutex, "Single Selection")
    val singleInterval = radio(intervalMutex, "Single Interval Selection")
    intervalMutex.select(multiInterval)

    contents += new Label("Selection Options")
    val elemMutex = new ButtonGroup
    val rowSelection = radio(elemMutex, "Row Selection")
    val columnSelection = radio(elemMutex, "Column Selection")
    val cellSelection = radio(elemMutex, "Cell Selection")
    elemMutex.select(rowSelection)

    val output = new TextArea(5, 40) { editable = false }
    contents += new ScrollPane(output)

    def outputSelection() {
      output.append("Lead: " + table.selection.rows.leadIndex + "," +
          table.selection.columns.leadIndex + ". ")
      output.append("Rows:")
      for (c <- table.selection.rows) output.append(" " + c)
      output.append(". Columns:")
      for (c <- table.selection.columns) output.append(" " + c)
      output.append(".\n")
    }

    reactions += {
      case ButtonClicked(`multiInterval`) =>
        table.selection.intervalMode = Table.IntervalMode.MultiInterval
        if (cellSelection.selected) {
          elemMutex.select(rowSelection)
          table.selection.elementMode = Table.ElementMode.None
        }
        cellSelection.enabled = false
      case ButtonClicked(`elementInterval`) =>
        table.selection.intervalMode = Table.IntervalMode.Single
        cellSelection.enabled = true
      case ButtonClicked(`singleInterval`) =>
        table.selection.intervalMode = Table.IntervalMode.SingleInterval
        cellSelection.enabled = true
      case ButtonClicked(`rowSelection`) =>
        if (rowSelection.selected)
          table.selection.elementMode = Table.ElementMode.Row
      case ButtonClicked(`columnSelection`) =>
        if (columnSelection.selected)
          table.selection.elementMode = Table.ElementMode.Column
      case ButtonClicked(`cellSelection`) =>
        if (cellSelection.selected)
          table.selection.elementMode = Table.ElementMode.Cell
      case TableRowsSelected(_, range, false) =>
        output.append("Rows selected, changes: " + range + "\n")
        outputSelection()
      case TableColumnsSelected(_, range, false) =>
        output.append("Columns selected, changes " + range + "\n")
        outputSelection()
    }
  }

  def top = new MainFrame {
    title = "Table Selection"
    contents = ui
  }
}
