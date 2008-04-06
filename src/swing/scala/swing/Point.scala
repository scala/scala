package scala.swing

object Point {
  def apply(p: java.awt.Point) = new Point(p.getX, p.getY)
}

class Point(x: Double, y: Double) extends Pair(x,y)
