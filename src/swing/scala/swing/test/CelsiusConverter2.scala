package swing.test;

import swing._
import layout._
import event._
import Orientation.{left, right, center}

object CelsiusConverter2 extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Convert Celsius / Fahrenheit"
    object Celsius extends TextField { columns = 5 };
    object Fahrenheit extends TextField { columns = 5 }
    contents += new Panel(Celsius, new Label(" Celsius  =  "),
                          Fahrenheit, new Label(" Fahrenheit")) {
      border = new EmptyBorder(15, 10, 10, 10)
    }
    listenTo(Fahrenheit, Celsius)
    reactions += {
      case TextModified(Fahrenheit) =>
        val f = Integer.parseInt(Fahrenheit.text)
        val c = (f - 32) * 5 / 9
        Celsius.text = c.toString
      case TextModified(Celsius) =>
        val c = Integer.parseInt(Celsius.text)
        val f = c * 9 / 5 + 32
        Fahrenheit.text = f.toString
    }
  }
}

