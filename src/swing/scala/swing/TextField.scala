package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JTextField
 */
class TextField(text0: String, columns0: Int) extends TextComponent with TextComponent.HasColumns {
  override lazy val peer: JTextField = new JTextField(text0, columns0)
  def this(text: String) = this(text, 0)
  def this(columns: Int) = this("", columns)
  def this() = this("")

  def columns: Int = peer.getColumns
  def columns_=(n: Int) = peer.setColumns(n)

  peer.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) { publish(ValueChanged(TextField.this, false)) }
  })
}
