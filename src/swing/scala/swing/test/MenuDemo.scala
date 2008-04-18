package scala.swing.test

import swing._
import swing.event._

object MenuDemo extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Menu Demo"
    menuBar = new MenuBar

    import Views._

    val menu = new Menu("A Menu")
    menu.contents += new MenuItem("An item")
    menu.contents += new Action("An action item") {
      def apply() { println("Action '"+ title +"' invoked") }
    }
    menu.contents += new Separator
    menu.contents += new CheckMenuItem("Check me")
    menu.contents += new CheckMenuItem("Me too!")
    menu.contents += new Separator
    val a = new RadioMenuItem("a")
    val b = new RadioMenuItem("b")
    val c = new RadioMenuItem("b")
    val mutex = new ButtonMutex(a,b,c)
    menu.contents ++= mutex.buttons

    menuBar.contents += menu
    menuBar.contents += new Menu("Empty Menu")
  }
}

