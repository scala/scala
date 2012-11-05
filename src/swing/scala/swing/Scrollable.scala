/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

object Scrollable {
  trait Wrapper extends Scrollable {
    protected def scrollablePeer: javax.swing.Scrollable
    def preferredViewportSize = scrollablePeer.getPreferredScrollableViewportSize

    def tracksViewportHeight: Boolean = scrollablePeer.getScrollableTracksViewportHeight
    def tracksViewportWidth: Boolean = scrollablePeer.getScrollableTracksViewportWidth

    def blockIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int =
      scrollablePeer.getScrollableBlockIncrement(visibleRect, orientation.id, direction)

    def unitIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int =
      scrollablePeer.getScrollableUnitIncrement(visibleRect, orientation.id, direction)
  }
}

/**
 * A component that is specially suitable for being placed inside a
 * <code>ScrollPane</code>.
 *
 * @see javax.swing.Scrollable
 */
trait Scrollable extends Component {
  def preferredViewportSize: Dimension

  def tracksViewportHeight: Boolean
  def tracksViewportWidth: Boolean

  def blockIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int
  def unitIncrement(visibleRect: Rectangle, orientation: Orientation.Value, direction: Int): Int
}
