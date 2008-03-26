package swing

import java.awt.Font
import javax.swing.border.Border

object Component {
  val ClientKey = "scala.swingWrapper"
  def wrapperFor[C<:Component](c: javax.swing.JComponent): C = c.getClientProperty(ClientKey).asInstanceOf[C]
}

abstract class Component extends UIElement with Showable.Swing with Reactor {
  lazy val peer: javax.swing.JComponent = new javax.swing.JComponent {}
  peer.putClientProperty(Component.ClientKey, this)

  def minimumSize = peer.getMinimumSize
  def minimumSize_=(x: Dimension) = peer.setMinimumSize(x.peer)
  def maxiumumSize = peer.getMaximumSize
  def maxiumumSize_=(x: Dimension) = peer.setMaximumSize(x.peer)
  def preferredSize = peer.getPreferredSize
  def preferredSize_=(x: Dimension) = peer.setPreferredSize(x.peer)

  def xAlignment: Double = peer.getAlignmentX
  def xAlignment_=(x: Double) = peer.setAlignmentX(x.toFloat)
  def yAlignment: Double = peer.getAlignmentY
  def yAlignment_=(x: Double) = peer.setAlignmentY(x.toFloat)

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
