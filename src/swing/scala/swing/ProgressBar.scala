/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

/**
 * A bar indicating progress of some action. Can be in indeterminate mode,
 * in which it indicates that the action is in progress (usually by some
 * animation) but does not indicate the amount of work done or to be done.
 *
 * @see javax.swing.JProgressBar
 */
class ProgressBar extends Component with Orientable.Wrapper {
  override lazy val peer: javax.swing.JProgressBar =
    new javax.swing.JProgressBar with SuperMixin

  def min: Int = peer.getMinimum
  def min_=(v: Int) { peer.setMinimum(v) }
  def max: Int = peer.getMaximum
  def max_=(v: Int) { peer.setMaximum(v) }
  def value: Int = peer.getValue
  def value_=(v: Int) { peer.setValue(v) }

  def labelPainted: Boolean = peer.isStringPainted
  def labelPainted_=(v: Boolean) { peer.setStringPainted(v) }

  def label: String = peer.getString
  def label_=(v: String) = peer.setString(v)

  def indeterminate: Boolean = peer.isIndeterminate
  def indeterminate_=(v: Boolean) { peer.setIndeterminate(v) }

  def paintBorder: Boolean = peer.isBorderPainted
  def paintBorder(v: Boolean) { peer.setBorderPainted(v) }
}
