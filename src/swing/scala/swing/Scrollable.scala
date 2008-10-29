package scala.swing

import java.awt.Rectangle

/**
 * A component that is specially suitable for being placed inside a
 * <code>ScrollPane</code>.
 *
 * @see javax.swing.Scrollable
 */
trait Scrollable extends Component {
  protected def scrollablePeer: javax.swing.Scrollable
  def preferredViewportSize = scrollablePeer.getPreferredScrollableViewportSize

  def tracksViewportHeight: Boolean = scrollablePeer.getScrollableTracksViewportHeight
  def tracksViewportWidth: Boolean = scrollablePeer.getScrollableTracksViewportWidth

  def blockIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int =
    scrollablePeer.getScrollableBlockIncrement(visibleRect, orientation.id, direction)

  def unitIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int =
    scrollablePeer.getScrollableUnitIncrement(visibleRect, orientation.id, direction)
}
