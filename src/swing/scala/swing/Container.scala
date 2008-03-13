package swing;

import javax.swing._
import scala.collection.mutable.ListBuffer

class Container(val jcontainer: java.awt.Container) extends Component {
  val acomponent = jcontainer
  def this() = this(new java.awt.Container())
  val elems = new ListBuffer[Component]
  def += (c: Component) = {
    elems += c
    jcontainer.add(c.acomponent)
  }
  def -= (c: Component) = {
    elems -= c
    jcontainer.remove(c.acomponent)
  }
}

