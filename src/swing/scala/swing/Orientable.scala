package scala.swing

/**
 * An <code>Oriented</code> whose orientation can be changed.
 */
trait Orientable extends Oriented {
  def orientation_=(o: Orientation.Value) { peer.setOrientation(o.id) }
}
