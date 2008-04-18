package scala.swing.geometry

object Rectangle {
  def apply(x: Int, y: Int, w: Int, h: Int) = new Rectangle {
    lazy val peer: java.awt.Rectangle = new java.awt.Rectangle(x, y, w, h)
  }

  def wrap(rect: java.awt.Rectangle) = new Rectangle {
    def peer: java.awt.Rectangle = rect
  }
}

abstract class Rectangle {
  def peer: java.awt.Rectangle
  def width = peer.getWidth
  def height = peer.getHeight
  def x = peer.x
  def y = peer.y
}
