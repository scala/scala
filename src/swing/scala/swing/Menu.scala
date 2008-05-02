package scala.swing

import scala.collection.mutable._
import javax.swing._

/**
 * @see javax.swing.JMenuBar
 */
class MenuBar extends Component with SequentialContainer.Wrapper {
  override lazy val peer: JMenuBar = new JMenuBar

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
class MenuItem(title0: String) extends AbstractButton {
  override lazy val peer: JMenuItem = new JMenuItem(title0)
  def this(a: Action) = {
    this("")
    action = a
  }
}

/**
 * @see javax.swing.JMenu
 */
class Menu(title0: String) extends MenuItem(title0) with SequentialContainer.Wrapper { self: Menu =>
  override lazy val peer: JMenu = new JMenu(title0)
}

/**
 * @see javax.swing.JRadioButtonMenuItem
 */
class RadioMenuItem(title0: String) extends MenuItem(title0) {
  override lazy val peer: JRadioButtonMenuItem = new JRadioButtonMenuItem(title0)
}
/**
 * @see javax.swing.JCheckBoxMenuItem
 */
class CheckMenuItem(title0: String) extends MenuItem(title0) {
  override lazy val peer: JCheckBoxMenuItem = new JCheckBoxMenuItem(title0)
}
