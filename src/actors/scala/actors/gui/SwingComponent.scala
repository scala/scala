package scala.actors.gui

import javax.swing._
import java.awt._

trait SwingComponent extends Component {
  val jcomponent = acomponent.asInstanceOf[JComponent];
  def border: javax.swing.border.Border = jcomponent.getBorder()
  def border_=(x: javax.swing.border.Border): unit = jcomponent.setBorder(x)
}

