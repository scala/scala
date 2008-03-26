package swing.test

import swing._
import event._

/** A GUI app to convert celsius to centigrade
 */
object CelsiusConverter extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Convert Celsius to Fahrenheit"
    defaultButton = convertButton
    object tempCelsius extends TextField
    object celsiusLabel extends Label {
      text = "Celsius"
      border = EmptyBorder(5, 5, 5, 5)
    }
    object convertButton extends Button {
      icon = new javax.swing.ImageIcon("c:\\workspace\\gui\\images\\convert.gif")
      border = EmptyBorder(5, 5, 5, 5)
    }
    object fahrenheitLabel extends Label {
      text = "Fahrenheit     "
      border = EmptyBorder(5, 5, 5, 5)
      listenTo(convertButton, tempCelsius)
      reactions += {
        case ButtonPressed(_) | ContentModified(_) =>
          val c = Integer.parseInt(tempCelsius.text)
          val f = c * 9 / 5 + 32
          text = "<html><font color = red>"+f+"</font> Fahrenheit</html>"
      }
    }
    content = new GridPanel(2,2)(tempCelsius, celsiusLabel, convertButton, fahrenheitLabel) {
      border = EmptyBorder(10, 10, 10, 10)
    }
  }
}

