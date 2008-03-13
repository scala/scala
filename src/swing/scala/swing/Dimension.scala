package swing;

case class Dimension(w: Int, h: Int) extends java.awt.Dimension(w, h) {
  def this(dim: java.awt.Dimension) = this(dim.width, dim.height)
}
