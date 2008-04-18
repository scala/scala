package scala.swing.test

import swing._
import swing.event._
import swing.geometry._

object TableSelection extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Table Selection"

    val model = Array[Array[AnyRef]](Array[AnyRef]("Mary", "Campione", "Snowboarding", new java.lang.Integer(5), new java.lang.Boolean(false)),
            Array[AnyRef]("Alison", "Huml", "Rowing", new java.lang.Integer(5), new java.lang.Boolean(false)),
            Array[AnyRef]("Kathy", "Walrath", "Knitting", new java.lang.Integer(5), new java.lang.Boolean(false)),
            Array[AnyRef]("Sharon", "Zakhour", "Speed reading", new java.lang.Integer(5), new java.lang.Boolean(false)),
            Array[AnyRef]("Philip", "Milne", "Pool", new java.lang.Integer(5), new java.lang.Boolean(false)))

    content = new BoxPanel(Vertical) {
      val table = new Table(model, Array[AnyRef]("First Name", "Last Name", "Sport", "# of Years", "Vegetarian"))
      listenTo()
      table.preferredViewportSize = Dimension(500, 70)
      table.fillsViewportHeight = true
      listenTo(table.selection)

      contents += new ScrollPane(table)

      contents += new Label("Selection Mode")
      val intervalMutex = new ButtonMutex
      def radio(mutex: ButtonMutex, text: String): RadioButton = {
        val b = new RadioButton(text)
        listenTo(b)
        mutex.buttons += b
        contents += b
        b
      }
      val multiInterval = radio(intervalMutex, "Multiple Interval Selection")
      val elementInterval = radio(intervalMutex, "Single Selection")
      val singleInterval = radio(intervalMutex, "Single Interval Selection")

      contents += new Label("Selection Options")
      val elemMutex = new ButtonMutex
      val rowSelection = radio(elemMutex, "Row Selection")
      val columnSelection = radio(elemMutex, "Column Selection")
      val cellSelection = radio(elemMutex, "Cell Selection")

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
        case ButtonClicked(_) =>
          rowSelection.selected = table.selection.elementMode == Table.ElementMode.Row
          columnSelection.selected = table.selection.elementMode == Table.ElementMode.Column
          if (cellSelection.enabled) {
            cellSelection.selected = table.selection.elementMode == Table.ElementMode.Cell
          }
      }

      reactions += {
        case ButtonClicked(`multiInterval`) =>
          table.selection.intervalMode = Table.IntervalMode.MultiInterval
          if (cellSelection.selected) {
            elemMutex.deselectAll()
            table.selection.elementMode = Table.ElementMode.None
          }
          cellSelection.enabled = false
        case ButtonClicked(`elementInterval`) =>
          table.selection.intervalMode = Table.IntervalMode.SingleInterval
          cellSelection.enabled = true
        case ButtonClicked(`singleInterval`) =>
          table.selection.intervalMode = Table.IntervalMode.Single
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
        case RowsSelected(table, false, range) =>
          output.append("Rows selected, changes: " + range + "\n")
          outputSelection()
        case ColumnsSelected(table, false, range) =>
          output.append("Columns selected, changes " + range + "\n")
          outputSelection()
      }
    }
  }
}

