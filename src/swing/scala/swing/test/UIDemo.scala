package scala.swing.test

import swing._
import event._
import Swing._

object UIDemo extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "UIElement Test"
    menuBar = new MenuBar

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

    import Swing._

    contents = new BorderPanel {
      import BorderPanel.Position._
      val tabs = new TabbedPane {
        import TabbedPane._
        val buttons = new FlowPanel {
          border = Swing.EmptyBorder(5,5,5,5)

          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Radio Buttons"), EmptyBorder(5,5,5,10))
            val a = new RadioButton("Green Vegetables")
    		val b = new RadioButton("Red Meat")
    		val c = new RadioButton("White Tofu")
    		val mutex = new ButtonGroup(a,b,c)
    		contents ++= mutex.buttons
          }
          contents += new BoxPanel(Orientation.Vertical) {
            border = CompoundBorder(TitledBorder(EtchedBorder, "Check Boxes"), EmptyBorder(5,5,5,10))
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
          contents += new Button("Center Frame") {
            reactions += {
              case ButtonClicked(_) => centerOnScreen()
            }
          }
        }
        pages += new Page("Buttons", buttons)
        pages += new Page("GridBag", GridBagDemo.ui)
        pages += new Page("Converter", CelsiusConverter2.ui)
        pages += new Page("Tables", TableSelection.ui)
        pages += new Page("Dialogs", Dialogs.ui)
        pages += new Page("Combo Boxes", ComboBoxes.ui)
        pages += new Page("Split Panes", new SplitPane(Orientation.Vertical, new Button("Hello"), new Button("World")) {
                            continuousLayout = true
                          })

        val password = new FlowPanel {
          contents += new Label("Enter your secret password here ")
          val field = new PasswordField(10)
          contents += field
          val label = new Label(field.text)
          contents += label
          listenTo(field)
          reactions += {
            case EditDone(`field`) => label.text = field.password.mkString
          }
        }

        pages += new Page("Password", password)
        pages += new Page("Painting", LinePainting.ui)
      }

      val list = new ListView(tabs.pages) {
        selection.selectIndices(0)
        selection.intervalMode = ListView.IntervalMode.Single
        import ListView._
        renderer = ListView.Renderer(_.title)
      }
      val center = new SplitPane(Orientation.Vertical, new ScrollPane(list), tabs) {
        oneTouchExpandable = true
        continuousLayout = true
      }
      layout(center) = Center

      object slider extends Slider {
        min = 0
        value = tabs.selection.index
        max = tabs.pages.size-1
        majorTickSpacing = 1
      }
      layout(slider) = South

      listenTo(slider)
      listenTo(tabs.selection)
      listenTo(list.selection)
      reactions += {
        case ValueChanged(`slider`) =>
          if(!slider.adjusting || reactLive) tabs.selection.index = slider.value
        case SelectionChanged(`tabs`) =>
          slider.value = tabs.selection.index
          list.selection.selectIndices(tabs.selection.index)
        case SelectionChanged(`list`) =>
          if (list.selection.items.size == 1)
            tabs.selection.page = list.selection.items(0)
      }
    }
  }
}

