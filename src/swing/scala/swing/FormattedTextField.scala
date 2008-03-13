package swing;

import javax.swing._
import java.awt.event._
import event._

class FormattedTextField(val jftextfield: JFormattedTextField) extends TextComponent(jftextfield) {
  def this(format: java.text.Format) = this(new JFormattedTextField(format));
}
