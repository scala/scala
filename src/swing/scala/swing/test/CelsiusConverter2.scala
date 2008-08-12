package scala.swing.test

import swing._
import event._

object CelsiusConverter2 extends SimpleGUIApplication {
  val ui = new FlowPanel {
   	val Celsius = new TextField { columns = 5 }
    val Fahrenheit = new TextField { columns = 5 }
    contents.append(Celsius, new Label(" Celsius  =  "),
                    Fahrenheit, new Label(" Fahrenheit"))
    border = Swing.EmptyBorder(15, 10, 10, 10)

    listenTo(Fahrenheit, Celsius)
    reactions += {
      case v @ ValueChanged(Fahrenheit) if v.committed =>
        val f = Integer.parseInt(Fahrenheit.text)
        val c = (f - 32) * 5 / 9
        Celsius.text = c.toString
      case v @ ValueChanged(Celsius) if v.committed =>
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

