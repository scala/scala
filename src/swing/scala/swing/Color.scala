package scala.swing;

case class Color(r: Int, g: Int, b: Int) extends java.awt.Color(r, g, b) {
  def this(col: java.awt.Color) = this(col.getRed, col.getGreen, col.getBlue)
}
