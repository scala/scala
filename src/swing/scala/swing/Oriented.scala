package scala.swing

trait Oriented {
  def peer: javax.swing.JComponent {
    def getOrientation(): Int
    def setOrientation(n: Int)
  }
  def orientation: Orientation = Orientation.wrap(peer.getOrientation)
}

trait Orientable extends Oriented {
  def orientation_=(o: Orientation) { peer.setOrientation(o.peer) }
}
