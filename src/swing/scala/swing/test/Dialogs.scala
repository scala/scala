package scala.swing
package test

import swing._
import swing.event._

object Dialogs extends SimpleSwingApplication {
  import TabbedPane._

  lazy val label = new Label("No Result yet")
  lazy val tabs = new TabbedPane {
    pages += new Page("File", new GridBagPanel { grid =>
      import GridBagPanel._
      val buttonText = new TextField("Click Me")

      val c = new Constraints
      c.fill = Fill.Horizontal
      c.grid = (1,1)

      val chooser = new FileChooser
      layout(new Button(Action("Open") {
        chooser.showOpenDialog(grid)
      })) = c

      c.grid = (1,2)
      layout(new Button(Action("Save") {
        chooser.showSaveDialog(grid)
      })) = c

      c.grid = (1,3)
      layout(new Button(Action("Custom") {
        chooser.showDialog(grid, buttonText.text)
      })) = c

      c.grid = (2,3)
      layout(new Label("  with Text  ")) = c

      c.grid = (3,3)
      c.ipadx = 50
      layout(buttonText) = c

      border = Swing.EmptyBorder(5, 5, 5, 5)
    })
    pages += new Page("Simple Modal Dialogs", new BorderPanel {
      import BorderPanel._
      val mutex = new ButtonGroup
      val ok = new RadioButton("OK (in the L&F's words)")
      val ynlf = new RadioButton("Yes/No (in the L&F's words)")
      val ynp = new RadioButton("Yes/No (in the programmer's words)")
      val yncp = new RadioButton("Yes/No/Cancel (in the programmer's words)")
      val radios = List(ok, ynlf, ynp, yncp)
      mutex.buttons ++= radios
      mutex.select(ok)
      val buttons = new BoxPanel(Orientation.Vertical) {
        contents ++= radios
      }
      layout(buttons) = Position.North
      layout(new Button(Action("Show It!") {
        import Dialog._
        mutex.selected.get match {
          case `ok` =>
            showMessage(buttons, "Eggs aren't supposed to be green.")
          case `ynlf` =>
            label.text = showConfirmation(buttons,
                             "Would you like green eggs and ham?",
                             "An Inane Question") match {
              case Result.Yes => "Ewww!"
              case Result.No => "Me neither!"
              case _ => "Come on -- tell me!"
          }
          case `ynp` =>
            val options = List("Yes, please",
                               "No, thanks",
                               "No eggs, no ham!")
            label.text = showOptions(buttons,
                        "Would you like some green eggs to go with that ham?",
                        "A Silly Question",
                        entries = options,
                        initial = 2) match {
              case Result.Yes => "You're kidding!"
              case Result.No => "I don't like them, either."
              case _ => "Come on -- 'fess up!"
            }
          case `yncp` =>
            val options = List("Yes, please",
                               "No, thanks",
                               "No eggs, no ham!")
            label.text = showOptions(buttons,
                        message = "Would you like some green eggs to go with that ham?",
                        title = "A Silly Question",
                        entries = options,
                        initial = 2) match {
              case Result.Yes => "Here you go: green eggs and ham!"
              case Result.No => "OK, just the ham, then."
              case Result.Cancel => "Well, I'm certainly not going to eat them!"
              case _ => "Please tell me what you want!"
            }
        }
      })) = Position.South
    })
    pages += new Page("More Dialogs", new BorderPanel {
      import BorderPanel._
      val mutex = new ButtonGroup
      val pick = new RadioButton("Pick one of several choices")
      val enter = new RadioButton("Enter some text")
      val custom = new RadioButton("Custom")
      val customUndec = new RadioButton("Custom undecorated")
      val custom2 = new RadioButton("2 custom dialogs")
      val radios = List(pick, enter, custom, customUndec, custom2)
      mutex.buttons ++= radios
      mutex.select(pick)
      val buttons = new BoxPanel(Orientation.Vertical) {
        contents ++= radios
      }
      layout(buttons) = Position.North
      layout(new Button(Action("Show It!") {
        import Dialog._
        mutex.selected.get match {
          case `pick` =>
            val possibilities = List("ham", "spam", "yam")
            val s = showInput(buttons,
                      "Complete the sentence:\n\"Green eggs and...\"",
                      "Customized Dialog",
                      Message.Plain,
                      Swing.EmptyIcon,
                      possibilities, "ham")

            //If a string was returned, say so.
            label.text = if ((s != None) && (s.get.length > 0))
              "Green eggs and... " + s.get + "!"
            else
              "Come on, finish the sentence!"
          case `enter` =>
            val s = showInput(buttons,
                      "Complete the sentence:\n\"Green eggs and...\"",
                      "Customized Dialog",
                      Message.Plain,
                      Swing.EmptyIcon,
                      Nil, "ham")

            //If a string was returned, say so.
            label.text = if ((s != None) && (s.get.length > 0))
              "Green eggs and... " + s.get + "!"
            else
              "Come on, finish the sentence!"
          case `custom` =>
            val dialog = new Dialog(top)
            dialog.open()
            dialog.contents = Button("Close Me!") { dialog.close() }
          case `customUndec` =>
            val dialog = new Dialog with RichWindow.Undecorated
            dialog.open()
            dialog.contents = Button("Close Me!") { dialog.close() }
          case `custom2` =>
            val d1 = new Dialog
            val d2 = new Dialog(d1)
            d1.open()
            d2.open()
            d1.contents = Button("Close Me! I am the owner and will automatically close the other one") { d1.close() }
            d2.contents = Button("Close Me!") { d2.close() }
        }
      })) = Position.South
    })
  }

  lazy val ui: Panel = new BorderPanel {
    layout(tabs) = BorderPanel.Position.Center
    layout(label) = BorderPanel.Position.South
  }


  lazy val top = new MainFrame {
    title = "Dialog Demo"
    contents = ui
  }
}

