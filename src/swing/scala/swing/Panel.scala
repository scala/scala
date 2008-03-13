package swing;

import javax.swing._
import java.awt.event._

class Panel(val jpanel: JPanel) extends Container(jpanel) with SwingComponent {
  def this(layout: java.awt.LayoutManager, elements: Component*) = {
    this(new JPanel(layout));
    for (val elem <- elements) this += elem
  }
  def this(elements: Component*) = this(new java.awt.FlowLayout, elements: _*)

  def layout: java.awt.LayoutManager = jpanel.getLayout()
  def layout_=(x: java.awt.LayoutManager) = jpanel.setLayout(x)
}
