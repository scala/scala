package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JFormattedTextField
 */
class FormattedTextField(format: java.text.Format) extends TextComponent {
  override lazy val peer: JFormattedTextField = new JFormattedTextField(format)
}
