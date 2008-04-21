package scala.swing

trait Orientable extends Oriented {
  def orientation_=(o: Orientation.Value) { peer.setOrientation(o.id) }
}
