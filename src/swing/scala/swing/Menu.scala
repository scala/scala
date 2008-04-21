package scala.swing

import scala.collection.mutable._
import javax.swing._

/**
 * @see javax.swing.JMenuBar
 */
class MenuBar(override val peer: JMenuBar) extends Component(peer) with SequentialContainer.Wrapper {
  def this() = this(new JMenuBar)

  def menus: Seq[Menu] = contents.filter(_.isInstanceOf[Menu]).map(_.asInstanceOf[Menu])

  // Not implemented by Swing
  //def helpMenu: Menu = Component.wrapperFor(peer.getHelpMenu)
  //def helpMenu_=(m: Menu) { peer.setHelpMenu(m.peer) }
}

/*trait MenuElement extends Component {
  def peer: javax.swing.JComponent with javax.swing.MenuElement

  def subElements: Seq[MenuElement] = peer.getSubElements.map(Component.wrapperFor(_))
}*/

/**
 * @see javax.swing.JMenuItem
 */
class MenuItem(override val peer: JMenuItem) extends Button(peer) {
  def this(title: String) = this(new JMenuItem(title))
  def this(a: Action) = {
    this("")
    action = a
  }
}

/**
 * @see javax.swing.JMenu
 */
class Menu(override val peer: JMenu) extends MenuItem(peer) with SequentialContainer.Wrapper { self: Menu =>
  def this(title: String) = this(new JMenu(title))
}

/**
 * @see javax.swing.JRadioButtonMenuItem
 */
class RadioMenuItem(override val peer: JRadioButtonMenuItem) extends MenuItem(peer) {
  def this(title: String) = this(new JRadioButtonMenuItem(title))
}
/**
 * @see javax.swing.JCheckBoxMenuItem
 */
class CheckMenuItem(override val peer: JCheckBoxMenuItem) extends MenuItem(peer) {
  def this(title: String) = this(new JCheckBoxMenuItem(title))
}