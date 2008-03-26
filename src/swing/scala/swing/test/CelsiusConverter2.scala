package swing.test

import swing._
import event._

object CelsiusConverter2 extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Convert Celsius / Fahrenheit"
    object Celsius extends TextField { columns = 5 }
    object Fahrenheit extends TextField { columns = 5 }
    content = new FlowPanel(Horizontal)(Celsius, new Label(" Celsius  =  "),
                                        Fahrenheit, new Label(" Fahrenheit")) {
      border = EmptyBorder(15, 10, 10, 10)
    }
    listenTo(Fahrenheit.contentModified, Celsius.contentModified)
    reactions += {
      case ContentModified(Fahrenheit) =>
        val f = Integer.parseInt(Fahrenheit.text)
        val c = (f - 32) * 5 / 9
        Celsius.text = c.toString
      case ContentModified(Celsius) =>
        val c = Integer.parseInt(Celsius.text)
        val f = c * 9 / 5 + 32
        Fahrenheit.text = f.toString
    }
  }
}

