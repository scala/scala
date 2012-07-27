/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import java.awt.event._
import javax.swing.event._
import javax.swing.border._
import javax.swing.{JComponent, Icon, BorderFactory, SwingUtilities}


/**
 * Helpers for this package.
 */
object Swing {
  protected[swing] def toNoIcon(i: Icon): Icon = if(i == null) EmptyIcon else i
  protected[swing] def toNullIcon(i: Icon): Icon = if(i == EmptyIcon) null else i
  protected[swing] def nullPeer(c: Component) = if (c != null) c.peer else null

  implicit def pair2Dimension(p: (Int, Int)): Dimension = new Dimension(p._1, p._2)
  implicit def pair2Point(p: (Int, Int)): Point = new Point(p._1, p._2)
  implicit def pair2Point(p: (Int, Int, Int, Int)): Rectangle = new Rectangle(p._1, p._2, p._3, p._4)

  @inline final def Runnable(@inline block: =>Unit) = new Runnable {
    def run = block
  }
  final def ChangeListener(f: ChangeEvent => Unit) = new ChangeListener {
    def stateChanged(e: ChangeEvent) { f(e) }
  }
  final def ActionListener(f: ActionEvent => Unit) = new ActionListener {
    def actionPerformed(e: ActionEvent) { f(e) }
  }

  def Box(min: Dimension, pref: Dimension, max: Dimension) = new Component {
    override lazy val peer = new javax.swing.Box.Filler(min, pref, max)
  }
  def HGlue = new Component {
    override lazy val peer = javax.swing.Box.createHorizontalGlue.asInstanceOf[JComponent]
  }
  def VGlue = new Component {
    override lazy val peer = javax.swing.Box.createVerticalGlue.asInstanceOf[JComponent]
  }
  def Glue = new Component {
    override lazy val peer = javax.swing.Box.createGlue.asInstanceOf[JComponent]
  }
  def RigidBox(dim: Dimension) = new Component {
    override lazy val peer = javax.swing.Box.createRigidArea(dim).asInstanceOf[JComponent]
  }
  def HStrut(width: Int) = new Component {
    override lazy val peer = javax.swing.Box.createHorizontalStrut(width).asInstanceOf[JComponent]
  }
  def VStrut(height: Int) = new Component {
    override lazy val peer = javax.swing.Box.createVerticalStrut(height).asInstanceOf[JComponent]
  }

  def Icon(image: java.awt.Image) = new javax.swing.ImageIcon(image)
  def Icon(filename: String) = new javax.swing.ImageIcon(filename)
  def Icon(url: java.net.URL) = new javax.swing.ImageIcon(url)

  /**
   * The empty icon. Use this icon instead of <code>null</code> to indicate
   * that you don't want an icon.
   */
  case object EmptyIcon extends Icon {
    def getIconHeight: Int = 0
    def getIconWidth: Int = 0
    def paintIcon(c: java.awt.Component, g: java.awt.Graphics, x: Int, y: Int) {}
  }

  def unwrapIcon(icon: Icon): Icon = if (icon == null) EmptyIcon else icon
  def wrapIcon(icon: Icon): Icon = if (icon == EmptyIcon) null else icon

  def EmptyBorder = BorderFactory.createEmptyBorder()
  def EmptyBorder(weight: Int) =
    BorderFactory.createEmptyBorder(weight, weight, weight, weight)
  def EmptyBorder(top: Int, left: Int, bottom: Int, right: Int) =
    BorderFactory.createEmptyBorder(top, left, bottom, right)

  def LineBorder(c: Color) = BorderFactory.createLineBorder(c)
  def LineBorder(c: Color, weight: Int) = BorderFactory.createLineBorder(c, weight)

  def BeveledBorder(kind: Embossing) = BorderFactory.createBevelBorder(kind.bevelPeer)
  def BeveledBorder(kind: Embossing, highlight: Color, shadow: Color) =
    BorderFactory.createBevelBorder(kind.bevelPeer, highlight, shadow)
  def BeveledBorder(kind: Embossing,
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
    def etchPeer = javax.swing.border.EtchedBorder.LOWERED
  }
  case object Raised extends Embossing {
    def bevelPeer = BevelBorder.RAISED
    def etchPeer = javax.swing.border.EtchedBorder.RAISED
  }

  def EtchedBorder = BorderFactory.createEtchedBorder()
  def EtchedBorder(kind: Embossing) =
    BorderFactory.createEtchedBorder(kind.etchPeer)
  def EtchedBorder(kind: Embossing, highlight: Color, shadow: Color) =
    BorderFactory.createEtchedBorder(kind.etchPeer, highlight, shadow)

  def MatteBorder(top: Int, left: Int, bottom: Int, right: Int, color: Color) =
    BorderFactory.createMatteBorder(top, left, bottom, right, color)
  def MatteBorder(top: Int, left: Int, bottom: Int, right: Int, icon: Icon) =
    BorderFactory.createMatteBorder(top, left, bottom, right, icon)

  def CompoundBorder(outside: Border, inside: Border) =
    BorderFactory.createCompoundBorder(outside, inside)

  def TitledBorder(border: Border, title: String) =
    BorderFactory.createTitledBorder(border, title)

  /**
   * Schedule the given code to be executed on the Swing event dispatching
   * thread (EDT). Returns immediately.
   */
  @inline final def onEDT(op: =>Unit) = SwingUtilities invokeLater Runnable(op)

  /**
   * Schedule the given code to be executed on the Swing event dispatching
   * thread (EDT). Blocks until after the code has been run.
   */
  @inline final def onEDTWait(op: =>Unit) = SwingUtilities invokeAndWait Runnable(op)
}
