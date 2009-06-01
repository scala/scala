package scala.swing

/**
 * Something that can have an orientation.
 */
trait Oriented {
  def peer: javax.swing.JComponent with OrientedMixin

  protected trait OrientedMixin {
    def getOrientation(): Int
    def setOrientation(n: Int)
  }
  def orientation: Orientation.Value = Orientation(peer.getOrientation)
}
