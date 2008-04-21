package scala.swing

trait Oriented {
  def peer: javax.swing.JComponent {
    def getOrientation(): Int
    def setOrientation(n: Int)
  }
  def orientation: Orientation.Value = Orientation(peer.getOrientation)
}
