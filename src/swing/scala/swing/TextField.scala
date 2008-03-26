package swing

import javax.swing._
import java.awt.event._
import event._

class TextField(override val peer: JTextField) extends TextComponent(peer) {
  def this(text: String, columns: int) = this(new JTextField(text, columns))
  def this(text: String) = this(new JTextField(text))
  def this(columns: int) = this(new JTextField(columns))
  def this() = this(new JTextField())

  def columns: int = peer.getColumns()
  def columns_=(x: int) = peer.setColumns(x)

  override lazy val contentModified = new Publisher {
    peer.addActionListener {
      new ActionListener {
        def actionPerformed(e: ActionEvent) { publish(ContentModified(TextField.this)) }
      }
    }
  }

  override lazy val liveContentModified = super.liveContentModified
}
