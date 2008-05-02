package scala.swing

import java.awt.{Color, Cursor, Font, Dimension}

/**
 * The base trait of all user interface elements.
 */
trait UIElement {
  def peer: java.awt.Component
  def foreground: Color = peer.getForeground
  def foreground_=(c: Color) = peer.setForeground(c)
  def background: Color = peer.getBackground
  def background_=(c: Color) = peer.setBackground(c)

  def font: Font = peer.getFont
  def font_=(f: Font) = peer.setFont(f)

  def locationOnScreen = peer.getLocationOnScreen
  def location = peer.getLocation
  def bounds = peer.getBounds
  def size = peer.getSize
  def size_=(dim: Dimension) = peer.setSize(dim)
  def size_=(xy: (Int, Int)) = peer.setSize(new Dimension(xy._1, xy._2))
  def locale = peer.getLocale
  def toolkit = peer.getToolkit

  def cursor: Cursor = peer.getCursor
  def cursor_=(c: Cursor) { peer.setCursor(c) }

  def visible: Boolean = peer.isVisible
  def visible_=(b: Boolean) { peer.setVisible(b) }
  def showing: Boolean = peer.isShowing

  def repaint() { peer.repaint }
}
