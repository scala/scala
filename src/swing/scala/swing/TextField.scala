package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JTextField
 */
class TextField(override val peer: JTextField) extends TextComponent(peer) with TextComponent.HasColumns {
  def this(text: String, columns: int) = this(new JTextField(text, columns))
  def this(text: String) = this(new JTextField(text))
  def this(columns: int) = this(new JTextField(columns))
  def this() = this(new JTextField())

  def columns: Int = peer.getColumns
  def columns_=(n: Int) = peer.setColumns(n)

  peer.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) { publish(ValueChanged(TextField.this, false)) }
  })
}
