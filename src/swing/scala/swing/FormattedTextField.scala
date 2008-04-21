package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JFormattedTextField
 */
class FormattedTextField(override val peer: JFormattedTextField) extends TextComponent(peer) {
  def this(format: java.text.Format) = this(new JFormattedTextField(format))
}
