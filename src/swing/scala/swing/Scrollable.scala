package scala.swing

import geometry._

trait Scrollable extends Component {
  protected def scrollablePeer: javax.swing.Scrollable
  def preferredViewportSize = scrollablePeer.getPreferredScrollableViewportSize

  def tracksViewportHeight: Boolean = scrollablePeer.getScrollableTracksViewportHeight
  def tracksViewportWidth: Boolean = scrollablePeer.getScrollableTracksViewportWidth

  def blockIncrement(visibleRect: Rectangle, orientation: Orientation, direction: Int): Int =
    scrollablePeer.getScrollableBlockIncrement(visibleRect.peer, orientation.peer, direction)

  def unitIncrement(visibleRect: Rectangle, orientation: Orientation, direction: Int): Int =
    scrollablePeer.getScrollableUnitIncrement(visibleRect.peer, orientation.peer, direction)
}
