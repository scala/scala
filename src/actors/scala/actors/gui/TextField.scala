package scala.actors.gui;

import javax.swing._
import java.awt.event._
import event._

class TextField(val jtextfield: JTextField) extends TextComponent(jtextfield) {
  def this(text: String, columns: int) = this(new JTextField(text, columns));
  def this(text: String) = this(new JTextField(text));
  def this(columns: int) = this(new JTextField(columns));
  def this() = this(new JTextField());

  def columns: int = jtextfield.getColumns()
  def columns_=(x: int) = jtextfield.setColumns(x)

  jtextfield.addActionListener {
    new ActionListener {
      def actionPerformed(e: ActionEvent) =
        publish(TextModified(TextField.this))
    }
  }
}
