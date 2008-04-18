package scala.swing

import java.awt.Color
import java.awt.Font

trait UIElement {
  def peer: java.awt.Component
  def foreground: Color = peer.getForeground
  def foreground_=(c: Color) = peer.setForeground(c)
  def background: Color = peer.getBackground
  def background_=(c: Color) = peer.setBackground(c)

  def font: Font = peer.getFont
  def font_=(f: Font) = peer.setFont(f)
}
