package scala.swing.test

import swing._
import event._

object UIDemo extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "UIElement Test"

    menuBar = new MenuBar

    import Views._
    import Border._

    val menu = new Menu("A Menu")
    menu.contents += new MenuItem("An item")
    menu.contents += new MenuItem(Action("An action item") {
      println("Action '"+ title +"' invoked")
    })
    menu.contents += new Separator
    menu.contents += new CheckMenuItem("Check me")
    menu.contents += new CheckMenuItem("Me too!")
    menu.contents += new Separator
    val a = new RadioMenuItem("a")
    val b = new RadioMenuItem("b")
    val c = new RadioMenuItem("c")
    val mutex = new ButtonGroup(a,b,c)
    menu.contents ++= mutex.buttons

    menuBar.contents += menu
    menuBar.contents += new Menu("Empty Menu")

    var reactLive = false

    contents = new BoxPanel(Orientation.Vertical) {
      val tabs = new TabbedPane {
        import TabbedPane._
        val buttons = new FlowPanel {
          border = Empty(5,5,5,5)
          contents += new BoxPanel(Orientation.Vertical) {
            border = Compound(Titled(Etched, "Radio Buttons"), Empty(5,5,5,10))
            val a = new RadioButton("Green Vegetables")
    		val b = new RadioButton("Red Meat")
    		val c = new RadioButton("White Tofu")
    		val mutex = new ButtonGroup(a,b,c)
    		contents ++= mutex.buttons
          }
          contents += new BoxPanel(Orientation.Vertical) {
            border = Compound(Titled(Etched, "Check Boxes"), Empty(5,5,5,10))
            val paintLabels = new CheckBox("Paint Labels")
    		val paintTicks = new CheckBox("Paint Ticks")
    		val snapTicks = new CheckBox("Snap To Ticks")
    		val live = new CheckBox("Live")
    		contents.append(paintLabels, paintTicks, snapTicks, live)
    		listenTo(paintLabels, paintTicks, snapTicks, live)
    		reactions += {
    		  case ButtonClicked(`paintLabels`) =>
    		    slider.paintLabels = paintLabels.selected
    		  case ButtonClicked(`paintTicks`) =>
    		    slider.paintTicks = paintTicks.selected
    		  case ButtonClicked(`snapTicks`) =>
    		    slider.snapToTicks = snapTicks.selected
    		  case ButtonClicked(`live`) =>
    		    reactLive = live.selected
    		}
          }
        }
        pages += new Page("Buttons", buttons)
        pages += new Page("GridBag", GridBagDemo.ui)
        pages += new Page("Converter", CelsiusConverter2.ui)
        pages += new Page("Tables", TableSelection.ui)
        pages += new Page("Dialogs", Dialogs.ui)

        val password = new FlowPanel {
          contents += new Label("Enter your secret password here ")
          val field = new PasswordField(10)

          contents += field
          val label = new Label(field.text)
          contents += label
          listenTo(field)
          reactions += {
            case ValueChanged(`field`, false) => label.text = field.password.mkString
          }
        }

        pages += new Page("Password", password)
      }
      contents += tabs

      object slider extends Slider {
        min = 0
        value = tabs.selection.index
        max = tabs.pages.size-1
        majorTickSpacing = 1
      }
      contents += slider

      listenTo(slider)
      listenTo(tabs.selection)
      reactions += {
        case ValueChanged(`slider`, live) =>
          if(!live || live == reactLive) tabs.selection.index = slider.value
        case SelectionChanged(`tabs`) => slider.value = tabs.selection.index
      }
    }
  }
}

