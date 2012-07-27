/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing.{KeyStroke, Icon}
import java.awt.event.ActionListener

object Action {
  /**
   * Special action that has an empty title and all default properties and does nothing.
   * Use this as a "null action", i.e., to tell components that they do not have any
   * associated action. A component may then obtain its properties from its direct members
   * instead of from its action.
   * In Java Swing, one would use `null` instead of a designated action.
   */
  case object NoAction extends Action("") { def apply() {} }

  object Trigger {
    trait Wrapper extends Action.Trigger {
      def peer: javax.swing.JComponent {
        def addActionListener(a: ActionListener)
        def removeActionListener(a: ActionListener)
        def setAction(a: javax.swing.Action)
        def getAction(): javax.swing.Action
      }

      // TODO: we need an action cache
      private var _action: Action = Action.NoAction
      def action: Action = _action
      def action_=(a: Action) { _action = a; peer.setAction(a.peer) }

      //1.6: def hideActionText: Boolean = peer.getHideActionText
      //def hideActionText_=(b: Boolean) = peer.setHideActionText(b)
    }
  }

  /**
   * Something that triggers an action.
   */
  trait Trigger {
    def action: Action
    def action_=(a: Action)

    //1.6: def hideActionText: Boolean
    //def hideActionText_=(b: Boolean)
  }

  /**
   * Convenience method to create an action with a given title and body to run.
   */
  def apply(title: String)(body: =>Unit) = new Action(title) {
    def apply() { body }
  }
}

/**
 * An abstract action to be performed in reaction to user input.
 *
 * Not every action component will honor every property of its action.
 * An action itself can generally be configured so that certain properties
 * should be ignored and instead taken from the component directly. In the
 * end, it is up to a component which property it uses in which way.
 *
 * @see javax.swing.Action
 */
abstract class Action(title0: String) {
  import Swing._

  lazy val peer: javax.swing.Action = new javax.swing.AbstractAction(title0) {
    def actionPerformed(a: java.awt.event.ActionEvent) = apply()
  }

  /**
   * Title is not optional.
   */
  def title: String = ifNull(peer.getValue(javax.swing.Action.NAME),"")
  def title_=(t: String) { peer.putValue(javax.swing.Action.NAME, t) }

  /**
   * None if large icon and small icon are not equal.
   */
  def icon: Icon = smallIcon //if(largeIcon == smallIcon) largeIcon else None
  def icon_=(i: Icon) { /*largeIcon = i;*/ smallIcon = i }
  // 1.6: def largeIcon: Icon = toNoIcon(peer.getValue(javax.swing.Action.LARGE_ICON_KEY).asInstanceOf[Icon])
  // def largeIcon_=(i: Icon) { peer.putValue(javax.swing.Action.LARGE_ICON_KEY, toNullIcon(i)) }
  def smallIcon: Icon = toNoIcon(peer.getValue(javax.swing.Action.SMALL_ICON).asInstanceOf[Icon])
  def smallIcon_=(i: Icon) { peer.putValue(javax.swing.Action.SMALL_ICON, toNullIcon(i)) }

  /**
   * For all components.
   */
  def toolTip: String =
    ifNull(peer.getValue(javax.swing.Action.SHORT_DESCRIPTION), "")
  def toolTip_=(t: String) {
    peer.putValue(javax.swing.Action.SHORT_DESCRIPTION, t)
  }
  /**
   * Can be used for status bars, for example.
   */
  def longDescription: String =
    ifNull(peer.getValue(javax.swing.Action.LONG_DESCRIPTION), "")
  def longDescription_=(t: String) {
    peer.putValue(javax.swing.Action.LONG_DESCRIPTION, t)
  }

  /**
   * Default: java.awt.event.KeyEvent.VK_UNDEFINED, i.e., no mnemonic key.
   * For all buttons and thus menu items.
   */
  def mnemonic: Int = ifNull(peer.getValue(javax.swing.Action.MNEMONIC_KEY),
                             java.awt.event.KeyEvent.VK_UNDEFINED)
  def mnemonic_=(m: Int) { peer.putValue(javax.swing.Action.MNEMONIC_KEY, m) }

  /*/**
   * Indicates which character of the title should be underlined to indicate the mnemonic key.
   * Ignored if out of bounds of the title string. Default: -1, i.e., ignored.
   * For all buttons and thus menu items.
   */
   1.6: def mnemonicIndex: Int =
   ifNull(peer.getValue(javax.swing.Action.DISPLAYED_MNEMONIC_INDEX_KEY), -1)
   def mnemonicIndex_=(n: Int) { peer.putValue(javax.swing.Action.DISPLAYED_MNEMONIC_INDEX_KEY, n) }
  */

  /**
   * For menus.
   */
  def accelerator: Option[KeyStroke] =
    toOption(peer.getValue(javax.swing.Action.ACCELERATOR_KEY))
  def accelerator_=(k: Option[KeyStroke]) {
    peer.putValue(javax.swing.Action.ACCELERATOR_KEY, k.orNull)
  }

  /**
   * For all components.
   */
  def enabled: Boolean = peer.isEnabled
  def enabled_=(b: Boolean) { peer.setEnabled(b) }

  /*/**
   * Only honored if not <code>None</code>. For various buttons.
   */
   1.6: def selected: Option[Boolean] = Option(peer.getValue(javax.swing.Action.SELECTED_KEY))
   def selected_=(b: Option[Boolean]) {
   peer.putValue(javax.swing.Action.SELECTED_KEY,
                 if (b == None) null else new java.lang.Boolean(b.get))
  }*/

  def apply()
}
