package swing;

import javax.swing._
import javax.swing.table._
import javax.swing.event._
import event._

class ScrollPane(val jscrollpane: JScrollPane) extends Container(jscrollpane) with SwingComponent with Publisher {
  def this() = this(new JScrollPane())
  def this(contents: Component) = this(new JScrollPane(contents.acomponent))

  def rowHeaderView: Component = null
  def rowHeaderView_=(c: Component) = jscrollpane.setRowHeaderView(c.acomponent)

  def viewportView: Component = null
  def viewportView_=(c: Component) = jscrollpane.setViewportView(c.acomponent)

}
