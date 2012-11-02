/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import javax.swing._
import scala.swing.Swing._

/**
 * A label component that display either a text, an icon, or both.
 *
 * @see javax.swing.JLabel
 */
class Label(text0: String, icon0: Icon, align: Alignment.Value) extends Component {
  override lazy val peer: JLabel =
    new JLabel(text0, toNullIcon(icon0), align.id) with SuperMixin

  def this() = this("", EmptyIcon, Alignment.Center)
  def this(s: String) = this(s, EmptyIcon, Alignment.Center)
  def text: String = peer.getText
  def text_=(s: String) = peer.setText(s)
  def icon: Icon = peer.getIcon
  def icon_=(i: Icon) = peer.setIcon(i)

  /**
   * The alignment of the label's contents relative to its bounding box.
   */
  def xAlignment: Alignment.Value = Alignment(peer.getHorizontalAlignment)
  def xAlignment_=(x: Alignment.Value) { peer.setHorizontalAlignment(x.id) }
  def yAlignment: Alignment.Value = Alignment(peer.getVerticalAlignment)
  def yAlignment_=(x: Alignment.Value) { peer.setVerticalAlignment(x.id) }

  /** @see javax.swing.JLabel#getHorizontalAlignment() */
  def horizontalAlignment: Alignment.Value = Alignment(peer.getHorizontalAlignment)
  /** @see javax.swing.JLabel#setHorizontalAlignment() */
  def horizontalAlignment_=(x: Alignment.Value) { peer.setHorizontalAlignment(x.id) }

  def verticalAlignment: Alignment.Value = Alignment(peer.getVerticalAlignment)
  def verticalAlignment_=(x: Alignment.Value) { peer.setVerticalAlignment(x.id) }

  def horizontalTextPosition: Alignment.Value = Alignment(peer.getHorizontalTextPosition)
  def horizontalTextPosition_=(x: Alignment.Value) { peer.setHorizontalTextPosition(x.id) }

  def verticalTextPosition: Alignment.Value = Alignment(peer.getVerticalTextPosition)
  def verticalTextPosition_=(x: Alignment.Value) { peer.setVerticalTextPosition(x.id) }

  def disabledIcon: Icon = peer.getDisabledIcon
  def disabledIcon_=(icon: Icon) { peer.setDisabledIcon(icon) }

  def iconTextGap: Int = peer.getIconTextGap
  def iconTextGap_=(gap: Int) { peer.setIconTextGap(gap) }

  def displayedMnemonicIndex: Int = peer.getDisplayedMnemonicIndex
  def displayedMnemonicIndex_=(index: Int) { peer.setDisplayedMnemonicIndex(index) }
}
