package scala.swing

import java.awt.Color
import javax.swing.{Icon, BorderFactory}
import javax.swing.border._

/**
 * Helper object for creating <code>java.awt.Border</code>s more conveniently.
 *
 * @see javax.swing.BorderFactory
 */
object Border {
  def Empty = BorderFactory.createEmptyBorder()
  def Empty(top: Int, left: Int, bottom: Int, right: Int) =
    BorderFactory.createEmptyBorder(top, left, bottom, right)

  def Line(c: Color) = BorderFactory.createLineBorder(c)
  def Line(c: Color, weight: Int) = BorderFactory.createLineBorder(c, weight)

  def Beveled(kind: Embossing) = BorderFactory.createBevelBorder(kind.bevelPeer)
  def Beveled(kind: Embossing, highlight: Color, shadow: Color) =
    BorderFactory.createBevelBorder(kind.bevelPeer, highlight, shadow)
  def Beveled(kind: Embossing,
              highlightOuter: Color, highlightInner: Color,
              shadowOuter: Color, shadowInner: Color) =
    BorderFactory.createBevelBorder(kind.bevelPeer,
          highlightOuter, highlightInner,
          shadowOuter, shadowInner)

  sealed abstract class Embossing {
    def bevelPeer: Int
    def etchPeer: Int
  }
  case object Lowered extends Embossing {
    def bevelPeer = BevelBorder.LOWERED
    def etchPeer = EtchedBorder.LOWERED
  }
  case object Raised extends Embossing {
    def bevelPeer = BevelBorder.RAISED
    def etchPeer = EtchedBorder.RAISED
  }

  def Etched = BorderFactory.createEtchedBorder()
  def Etched(kind: Embossing) =
    BorderFactory.createEtchedBorder(kind.etchPeer)
  def Etched(kind: Embossing, highlight: Color, shadow: Color) =
    BorderFactory.createEtchedBorder(kind.etchPeer, highlight, shadow)

  def Matte(top: Int, left: Int, bottom: Int, right: Int, color: Color) =
    BorderFactory.createMatteBorder(top, left, bottom, right, color)
  def Matte(top: Int, left: Int, bottom: Int, right: Int, icon: Icon) =
    BorderFactory.createMatteBorder(top, left, bottom, right, icon)

  def Compound(outside: Border, inside: Border) =
    BorderFactory.createCompoundBorder(outside, inside)

  def Titled(border: Border, title: String) =
    BorderFactory.createTitledBorder(border, title)
}