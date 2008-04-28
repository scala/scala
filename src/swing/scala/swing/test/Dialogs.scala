package scala.swing.test

import swing._
import swing.event._

object Dialogs extends SimpleGUIApplication {
  val ui = new GridBagPanel { grid =>
    import GridBagPanel._
    val buttonText = new TextField("Click Me")

    val c = new Constraints
    c.fill = Fill.Horizontal
    c.grid = (1,1)

    val chooser = new FileChooser
    layout(new PushButton(Action("Open") {
      chooser.showOpenDialog(grid)
    })) = c

    c.grid = (1,2)
    layout(new PushButton(Action("Save") {
      chooser.showSaveDialog(grid)
    })) = c

    c.grid = (1,3)
    layout(new PushButton(Action("Custom") {
      chooser.showDialog(grid, buttonText.text)
    })) = c

    c.grid = (2,3)
    layout(new Label("  with Text  ")) = c

    c.grid = (3,3)
    c.ipadx = 50
    layout(buttonText) = c

    border = Border.Empty(5, 5, 5, 5)
  }

  def top = new MainFrame {
    title = "Dialog Demo"
    contents = ui
  }
}

