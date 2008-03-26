package swing

object Dimension {
  def apply(w: Int, h: Int) = new Dimension {
    lazy val peer: java.awt.Dimension = new java.awt.Dimension(w, h)
  }

  def wrap(dim: java.awt.Dimension) = new Dimension {
    def peer: java.awt.Dimension = dim
  }
}

abstract class Dimension {
  def peer: java.awt.Dimension
  def width = peer.getWidth
  def height = peer.getHeight
}
