package scala.swing

import java.awt.Dimension
import java.awt.event._
import javax.swing._
import javax.swing.event._

/**
 * Helpers for this package.
 */
object Swing {
  protected[swing] def ifNull[A](o: Object, a: A): A = if(o eq null) a else o.asInstanceOf[A]
  protected[swing] def toOption[A](o: Object): Option[A] = if(o eq null) None else Some(o.asInstanceOf[A])
  protected[swing] def toNull[A>:Null<:AnyRef](a: Option[A]): A = if(a == None) null else a.get
  protected[swing] def toNoIcon(i: Icon): Icon = if(i == null) Icon.Empty else i
  protected[swing] def toNullIcon(i: Icon): Icon = if(i == Icon.Empty) null else i

  implicit def block2Runnable(block: =>Unit): Runnable = new Runnable {
    override def run = block
  }
  def ChangeListener(f: ChangeEvent => Unit) = new ChangeListener {
    def stateChanged(e: ChangeEvent) { f(e) }
  }
  def ActionListener(f: ActionEvent => Unit) = new ActionListener {
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
}
