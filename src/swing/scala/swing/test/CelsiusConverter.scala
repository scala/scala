package swing.test;

import swing._
import layout._
import event._
import Orientation.{left, right}

/** A GUI app to convert celsius to centigrade
 */
object CelsiusConverter extends SimpleGUIApplication {
  def top: Frame = new MainFrame {
    title = "Convert Celsius to Fahrenheit"
    defaultButton = convertButton
    object tempCelsius extends TextField();
    object celsiusLabel extends Label {
      text = "Celsius"
      border = new EmptyBorder(5, 5, 5, 5)
    }
    object convertButton extends Button {
      icon = new javax.swing.ImageIcon("c:\\workspace\\gui\\images\\convert.gif")
      border = new EmptyBorder(5, 5, 5, 5)
    }
    object fahrenheitLabel extends Label {
      text = "Fahrenheit     "
      border = new EmptyBorder(5, 5, 5, 5)
      listenTo(convertButton, tempCelsius)
      reactions += {
        case ButtonPressed(_) | TextModified(_) =>
          val c = Integer.parseInt(tempCelsius.text)
          val f = c * 9 / 5 + 32
          text = "<html><font color = red>"+f+"</font> Fahrenheit</html>"
      }
    }
    contents += new Panel(tempCelsius, celsiusLabel, convertButton, fahrenheitLabel) {
      layout = grid(2, 2)
      border = new EmptyBorder(10, 10, 10, 10)
    }
  }
}

