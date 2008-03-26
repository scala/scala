package swing

import javax.swing._
import javax.swing.event._

object Swing {
  implicit def block2Runnable(block: =>Unit): Runnable = new Runnable {
    override def run = block
  }
  def ChangeListener(f: ChangeEvent => Unit) = new ChangeListener {
    def stateChanged(e: ChangeEvent) { f(e) }
  }

  def Box(min: Dimension, pref: Dimension, max: Dimension) = new Component {
    override lazy val peer = new javax.swing.Box.Filler(min.peer, pref.peer, max.peer)
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
    override lazy val peer = javax.swing.Box.createRigidArea(dim.peer).asInstanceOf[JComponent]
  }
  def HStrut(width: Int) = new Component {
    override lazy val peer = javax.swing.Box.createHorizontalStrut(width).asInstanceOf[JComponent]
  }
  def VStrut(height: Int) = new Component {
    override lazy val peer = javax.swing.Box.createVerticalStrut(height).asInstanceOf[JComponent]
  }
}
