package scala.swing

import javax.swing.{KeyStroke, Icon}

object Action {
  case object NoAction extends Action("") { def apply() {} }

  /**
   * Anything that triggers an action.
   */
  trait Trigger {
    def action: Action
    def action_=(a: Action)

    def hideActionText: Boolean
    def hideActionText_=(b: Boolean)
  }
}

/**
 * Not every action component will honor every property of its action.
 * An action itself can generally be configured so that certain properties
 * should be ignored and instead taken from the component directly. In the
 * end, it is up to a component, which property it uses in which way.
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
  def icon: Option[Icon] = if(largeIcon == smallIcon) largeIcon else None
  def icon_=(i: Option[Icon]) { largeIcon = i; smallIcon = i }
  def largeIcon: Option[Icon] = toOption(peer.getValue(javax.swing.Action.LARGE_ICON_KEY))
  def largeIcon_=(i: Option[Icon]) { peer.putValue(javax.swing.Action.LARGE_ICON_KEY, toNull(i)) }
  def smallIcon: Option[Icon] = toOption(peer.getValue(javax.swing.Action.SMALL_ICON))
  def smallIcon_=(i: Option[Icon]) { peer.putValue(javax.swing.Action.SMALL_ICON, toNull(i)) }

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

  /**
   * Indicates which character of the title should be underlined to indicate the mnemonic key.
   * Ignored if out of bounds of the title string. Default: -1, i.e., ignored.
   * For all buttons and thus menu items.
   */
  def mnemonicIndex: Int =
    ifNull(peer.getValue(javax.swing.Action.DISPLAYED_MNEMONIC_INDEX_KEY), -1)
  def mnemonicIndex_=(n: Int) { peer.putValue(javax.swing.Action.DISPLAYED_MNEMONIC_INDEX_KEY, n) }

  /**
   * For menus.
   */
  def accelerator: Option[KeyStroke] =
    toOption(peer.getValue(javax.swing.Action.ACCELERATOR_KEY))
  def accelerator_=(k: Option[KeyStroke]) {
    peer.putValue(javax.swing.Action.ACCELERATOR_KEY, toNull(k))
  }

  /**
   * For all components.
   */
  def enabled: Boolean = peer.isEnabled
  def enabled_=(b: Boolean) { peer.setEnabled(b) }

  /**
   * Only honored if not <code>None</code>. For various buttons.
   */
  def selected: Option[Boolean] = toOption(peer.getValue(javax.swing.Action.SELECTED_KEY))
  def selected_=(b: Option[Boolean]) {
    peer.putValue(javax.swing.Action.SELECTED_KEY,
                  if (b == None) null else new java.lang.Boolean(b.get))
  }

  def apply()
}
