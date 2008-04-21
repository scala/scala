package scala.swing.test

import swing._
import event._

object CelsiusConverter2 extends SimpleGUIApplication {
  val ui = new FlowPanel {
   	object Celsius extends TextField { columns = 5 }
    object Fahrenheit extends TextField { columns = 5 }
    contents.append(Celsius, new Label(" Celsius  =  "), Fahrenheit, new Label(" Fahrenheit"))
    border = Border.Empty(15, 10, 10, 10)

    listenTo(Fahrenheit, Celsius)
    reactions += {
      case ValueChanged(Fahrenheit, false) =>
        val f = Integer.parseInt(Fahrenheit.text)
        val c = (f - 32) * 5 / 9
        Celsius.text = c.toString
      case ValueChanged(Celsius, false) =>
        val c = Integer.parseInt(Celsius.text)
        val f = c * 9 / 5 + 32
        Fahrenheit.text = f.toString
    }
  }
  def top = new MainFrame {
    title = "Convert Celsius / Fahrenheit"
   	contents = ui
  }
}

