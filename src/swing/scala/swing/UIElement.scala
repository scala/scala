package scala.swing

import java.awt.{Color, Cursor, Font}

/**
 * The root of all user interface elements.
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
  def locale = peer.getLocale
  def toolkit = peer.getToolkit

  def cursor: Cursor = peer.getCursor
  def cursor_=(c: Cursor) { peer.setCursor(c) }

  def visible: Boolean = peer.isVisible
  def visible_=(b: Boolean) { peer.setVisible(b) }
  def showing: Boolean = peer.isShowing

  def repaint() { peer.repaint }
}
