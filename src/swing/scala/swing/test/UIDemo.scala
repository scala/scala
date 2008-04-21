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
            val a = new CheckBox("Paint Labels")
    		val b = new CheckBox("Paint Ticks")
    		val c = new CheckBox("Snap To Ticks")
    		contents.append(a,b,c)
    		listenTo(a,b,c)
    		reactions += {
    		  case ButtonClicked(`a`) => slider.paintLabels = a.selected
    		  case ButtonClicked(`b`) => slider.paintTicks = b.selected
    		  case ButtonClicked(`c`) => slider.snapToTicks = c.selected
    		}
          }
        }
        pages += new Page("Buttons", buttons)
        pages += new Page("GridBag", GridBagDemo.ui)
        pages += new Page("Converter", CelsiusConverter2.ui)
        pages += new Page("Tables", TableSelection.ui)
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
        case ValueChanged(`slider`, false) => tabs.selection.index = slider.value
        case SelectionChanged(`tabs`) => slider.value = tabs.selection.index
      }
    }
  }
}

