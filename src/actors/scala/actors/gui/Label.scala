package scala.actors.gui;

import javax.swing._;

class Label(val jlabel: JLabel) extends Container(jlabel) with SwingComponent {
  def this(txt: String) = this(new JLabel(txt))
  def this() = this("Untitled Label")
  def text: String = jlabel.getText()
  def text_=(s: String) = jlabel.setText(s)
  def halign: Orientation.Value = Orientation(jlabel.getHorizontalAlignment())
  def halign_=(x: Orientation.Value) = jlabel.setHorizontalAlignment(x.id)
  def valign: Orientation.Value = Orientation(jlabel.getVerticalAlignment())
  def valign_=(x: Orientation.Value) = jlabel.setVerticalAlignment(x.id)
}
