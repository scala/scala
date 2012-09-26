/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2012, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing.JPopupMenu
import javax.swing.event.{PopupMenuListener, PopupMenuEvent}
import event._

/**
 * A popup menu.
 * 
 * Example usage:
 *
 * {{{
 * val popupMenu = new PopupMenu {
 *   contents += new Menu("menu 1") {
 *     contents += new RadioMenuItem("radio 1.1")
 *     contents += new RadioMenuItem("radio 1.2")
 *   }
 *   contents += new Menu("menu 2") {
 *     contents += new RadioMenuItem("radio 2.1")
 *     contents += new RadioMenuItem("radio 2.2")
 *   }
 * }
 * val button = new Button("Show Popup Menu")
 * reactions += {
 *   case e: ButtonClicked => popupMenu.show(button, 0, button.bounds.height)
 * }
 * listenTo(button)
 * }}}
 * 
 * @author John Sullivan
 * @author Ingo Maier
 * @see javax.swing.JPopupMenu
 */
class PopupMenu extends Component with SequentialContainer.Wrapper with Publisher {
  override lazy val peer: JPopupMenu = new JPopupMenu with SuperMixin 

  peer.addPopupMenuListener(new PopupMenuListener {
  	def popupMenuCanceled(e: PopupMenuEvent) {
  	  publish(PopupMenuCanceled(PopupMenu.this))
  	}
  	def popupMenuWillBecomeInvisible(e: PopupMenuEvent) {
  	  publish(PopupMenuWillBecomeInvisible(PopupMenu.this))
  	}
  	def popupMenuWillBecomeVisible(e: PopupMenuEvent) {
  	  publish(PopupMenuWillBecomeVisible(PopupMenu.this))
  	}
  })

  def show(invoker: Component, x: Int, y: Int): Unit = peer.show(invoker.peer, x, y)

  def margin: Insets = peer.getMargin
  def label: String = peer.getLabel
  def label_=(s: String) { peer.setLabel(s) }
}

