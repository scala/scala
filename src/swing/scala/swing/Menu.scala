package scala.swing

import scala.collection.mutable._
import javax.swing._

class MenuBar extends IndexedContainer {
  override lazy val peer = new JMenuBar

  def menus: Seq[Menu] = contents.filter(_.isInstanceOf[Menu]).map(_.asInstanceOf[Menu])

  // Not implemented by Swing
  //def helpMenu: Menu = Component.wrapperFor(peer.getHelpMenu)
  //def helpMenu_=(m: Menu) { peer.setHelpMenu(m.peer) }
}

/*trait MenuElement extends Component {
  def peer: javax.swing.JComponent with javax.swing.MenuElement

  def subElements: Seq[MenuElement] = peer.getSubElements.map(Component.wrapperFor(_))
}*/

class MenuItem(val title: String) extends Button {
  def this(a: Action) = {
    this("")
    action = a
  }
  override lazy val peer = new JMenuItem(title)
}

class Menu(title: String) extends MenuItem(title) with IndexedContainer { self: Menu =>
  override lazy val peer = new JMenu(title)
}

class RadioMenuItem(title: String) extends MenuItem(title) {
  override lazy val peer = new JRadioButtonMenuItem(title)
}

class CheckMenuItem(title: String) extends MenuItem(title) {
  override lazy val peer = new JCheckBoxMenuItem(title)
}