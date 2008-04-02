package scala.swing

import javax.swing._;
import java.awt._;

abstract class Component
extends Object
with Reactor
{
  val acomponent: java.awt.Component

  def foreground: Color = new Color(peer.getForeground)
  def foreground_=(x: Color) = peer.setForeground(x)
  def background: Color = new Color(peer.getBackground)
  def background_=(x: Color) = peer.setBackground(x)
  def border: Border = peer.getBorder
  def border_=(x: Border) { peer.setBorder(x) }

  def font: Font = peer.getFont
  def font_=(x: Font) = peer.setFont(x)
  def opaque: Boolean = peer.isOpaque
  def opaque_=(x: Boolean) = peer.setOpaque(x)

  def tooltip: String = peer.getToolTipText
  def tooltip_=(t: String) = peer.setToolTipText(t)

  def inputVerifier: this.type => Boolean = { a =>
    peer.getInputVerifier().verify(a.peer)
  }
  def inputVerifier_=(v: this.type => Boolean) {
    peer.setInputVerifier(new javax.swing.InputVerifier {
      def verify(c: javax.swing.JComponent) = v(Component.wrapperFor(c))
    })
  }

  /*def verifyOnTraversal: (Component, Component) => Boolean = { a =>
    peer.getInputVerifier().verify(a.peer)
  }
  def verifyOnTraversal_=(v: (Component, Component) => Boolean) {
    peer.setInputVerifier(new javax.swing.InputVerifier {
      def verify(c: javax.swing.JComponent) = v(Component.wrapperFor(c))
    })
  }*/

  override def toString = "scala.swing wrapper " + peer.toString
}
