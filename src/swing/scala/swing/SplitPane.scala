/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.swing

import Swing._

/**
 * A container with exactly two children. Arranges them side by side, either
 * horizontally or vertically. Displays a draggable divider component between
 * them that lets the user adjust the size ratio of the children.
 *
 * @see javax.swing.JSplitPane
 */
class SplitPane(o: Orientation.Value, left: Component, right: Component) extends Component with Container with Orientable.Wrapper {
  override lazy val peer: javax.swing.JSplitPane =
    new javax.swing.JSplitPane(o.id, left.peer, right.peer) with SuperMixin
  def this(o: Orientation.Value) = this(o, new Component {}, new Component {})
  def this() = this(Orientation.Horizontal)

  def contents: Seq[Component] = List(leftComponent, rightComponent)
  def contents_=(left: Component, right: Component) {
    peer.setLeftComponent(nullPeer(left))
    peer.setRightComponent(nullPeer(right))
  }

  def topComponent: Component =
    UIElement.cachedWrapper[Component](peer.getTopComponent.asInstanceOf[javax.swing.JComponent])
  def topComponent_=(c: Component) { peer.setTopComponent(nullPeer(c)) }
  def bottomComponent: Component =
    UIElement.cachedWrapper[Component](peer.getBottomComponent.asInstanceOf[javax.swing.JComponent])
  def bottomComponent_=(c: Component) { peer.setBottomComponent(nullPeer(c)) }

  def leftComponent: Component = topComponent
  def leftComponent_=(c: Component) { topComponent = c }
  def rightComponent: Component = bottomComponent
  def rightComponent_=(c: Component) { bottomComponent = c }

  def dividerLocation: Int = peer.getDividerLocation
  def dividerLocation_=(n: Int) { peer.setDividerLocation(n) }

  /*def proportionalDividerLocation: Double =
    if (orientation == Orientation.Vertical) dividerLocation / (size.height - dividerSize)
    else dividerLocation / (size.width - dividerSize)*/
  def dividerLocation_=(f: Double) { peer.setDividerLocation(f) }

  def dividerSize: Int = peer.getDividerSize
  def dividerSize_=(n: Int) { peer.setDividerSize(n) }
  def resizeWeight: Double = peer.getResizeWeight
  def resizeWeight_=(n: Double) { peer.setResizeWeight(n) }

  def resetToPreferredSizes() { peer.resetToPreferredSizes() }

  def oneTouchExpandable: Boolean = peer.isOneTouchExpandable
  def oneTouchExpandable_=(b: Boolean) { peer.setOneTouchExpandable(b) }
  def continuousLayout: Boolean = peer.isContinuousLayout
  def continuousLayout_=(b: Boolean) { peer.setContinuousLayout(b) }
}
