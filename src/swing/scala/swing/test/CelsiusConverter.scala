package scala.swing.test

import swing._
import event._

/** A GUI app to convert celsius to centigrade
 */
object CelsiusConverter extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "Convert Celsius to Fahrenheit"
    defaultButton = Some(convertButton)
    val tempCelsius = new TextField
    val celsiusLabel = new Label {
      text = "Celsius"
      border = Border.Empty(5, 5, 5, 5)
    }
    val convertButton = new Button {
      text = "Convert"//new javax.swing.ImageIcon("c:\\workspace\\gui\\images\\convert.gif")
      //border = Border.Empty(5, 5, 5, 5)
    }
    val fahrenheitLabel = new Label {
      text = "Fahrenheit     "
      border = Border.Empty(5, 5, 5, 5)
      listenTo(convertButton, tempCelsius)
      reactions += {
        case ButtonClicked(_) | ValueChanged(_,false) =>
          val c = Integer.parseInt(tempCelsius.text)
          val f = c * 9 / 5 + 32
          text = "<html><font color = red>"+f+"</font> Fahrenheit</html>"
      }
    }
    contents = new GridPanel(2,2) {
      contents.append(tempCelsius, celsiusLabel, convertButton, fahrenheitLabel)
      border = Border.Empty(10, 10, 10, 10)
    }
  }
}

