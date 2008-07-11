package scala.swing

import event._

import java.awt.{Dimension, Point}
import java.awt.event._
import javax.swing.JComponent
import javax.swing.border.Border

object Component {
  private val ClientKey = "scala.swingWrapper"

  /**
   * Returns the wrapper for a given peer.
   * Fails if there is no wrapper for the given component.
   */
  protected[swing] def wrapperFor[C<:Component](c: javax.swing.JComponent): C =
    c.getClientProperty(ClientKey).asInstanceOf[C]

  /**
   * Wraps a given Java Swing Component into a new wrapper.
   */
  def wrap[A](c: JComponent) = new Component {
    override lazy val peer = c
  }
}

/**
 * Base class for all UI elements that can be displayed in a window.
 *
 * @see javax.swing.JComponent
 */
abstract class Component extends UIElement with Publisher {
  override lazy val peer: javax.swing.JComponent = new javax.swing.JComponent with SuperMixin {}
  var initP: JComponent = null
  peer.putClientProperty(Component.ClientKey, this)

  trait SuperMixin extends JComponent {
    override def paintComponent(g: java.awt.Graphics) {
      Component.this.paintComponent(g)
    }
    def __super__paintComponent(g: java.awt.Graphics) {
      super.paintComponent(g)
    }
  }


  /**
   * Used by certain layout managers, e.g., BoxLayout or OverlayLayout to
   * align components relative to each other.
   */
  def xLayoutAlignment: Double = peer.getAlignmentX
  def xLayoutAlignment_=(x: Double) = peer.setAlignmentX(x.toFloat)
  def yLayoutAlignment: Double = peer.getAlignmentY
  def yLayoutAlignment_=(y: Double) = peer.setAlignmentY(y.toFloat)

  def border: Border = peer.getBorder
  def border_=(b: Border) { peer.setBorder(b) }

  def opaque: Boolean = peer.isOpaque
  def opaque_=(b: Boolean) = peer.setOpaque(b)

  def enabled: Boolean = peer.isEnabled
  def enabled_=(b: Boolean) = peer.setEnabled(b)

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

  peer.addComponentListener(new java.awt.event.ComponentListener {
    def componentHidden(e: java.awt.event.ComponentEvent) {
      publish(ComponentHidden(Component.this))
    }
    def componentShown(e: java.awt.event.ComponentEvent) {
      publish(ComponentShown(Component.this))
    }
    def componentMoved(e: java.awt.event.ComponentEvent) {
      publish(ComponentMoved(Component.this))
    }
    def componentResized(e: java.awt.event.ComponentEvent) {
      publish(ComponentResized(Component.this))
    }
  })

  peer.addFocusListener(new java.awt.event.FocusListener {
    def other(e: java.awt.event.FocusEvent) = e.getOppositeComponent match {
      case c: JComponent => Some(Component.wrapperFor(c))
      case _ => None
    }

    def focusGained(e: java.awt.event.FocusEvent) {
      publish(FocusGained(Component.this, other(e), e.isTemporary))
    }
    def focusLost(e: java.awt.event.FocusEvent) {
      publish(FocusLost(Component.this, other(e), e.isTemporary))
    }
  })

  /**
   * Contains publishers for various mouse events. They are subdivided for
   * efficiency reasons.
   */
  object Mouse {
    /**
     * Publishes clicks, presses and releases.
     */
    val clicks: Publisher = new Publisher {
      peer.addMouseListener(new MouseListener {
        def mouseEntered(e: java.awt.event.MouseEvent) { }
        def mouseExited(e: java.awt.event.MouseEvent) { }
        def mouseClicked(e: java.awt.event.MouseEvent) {
          publish(MouseClicked(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                               e.getPoint, e.getModifiers, e.getClickCount, e.isPopupTrigger))
        }
        def mousePressed(e: java.awt.event.MouseEvent) {
          publish(MousePressed(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                               e.getPoint, e.getModifiers, e.getClickCount, e.isPopupTrigger))
        }
        def mouseReleased(e: java.awt.event.MouseEvent) {
          publish(MouseReleased(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                                e.getPoint, e.getModifiers, e.getClickCount, e.isPopupTrigger))
        }
      })
    }
    /**
     * Publishes enters, exits, moves, and drags.
     */
    val moves: Publisher = new Publisher {
      peer.addMouseListener(new MouseListener {
        def mouseEntered(e: java.awt.event.MouseEvent) {
          publish(MouseEntered(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                               e.getPoint, e.getModifiers))
        }
        def mouseExited(e: java.awt.event.MouseEvent) {
          publish(MouseExited(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                              e.getPoint, e.getModifiers))
        }
        def mouseClicked(e: java.awt.event.MouseEvent) {}
        def mousePressed(e: java.awt.event.MouseEvent) { }
        def mouseReleased(e: java.awt.event.MouseEvent) { }
      })
      peer.addMouseMotionListener(new MouseMotionListener {
        def mouseMoved(e: java.awt.event.MouseEvent) {
          publish(MouseMoved(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                             e.getPoint, e.getModifiers))
        }
        def mouseDragged(e: java.awt.event.MouseEvent) {
          publish(MouseDragged(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                               e.getPoint, e.getModifiers))
        }
      })
    }
    /**
     * Publishes mouse wheel moves.
     */
    val wheel: Publisher = new Publisher {
      peer.addMouseWheelListener(new MouseWheelListener {
        def mouseWheelMoved(e: java.awt.event.MouseWheelEvent) {
          publish(MouseWheelMoved(Component.wrapperFor(e.getSource.asInstanceOf[JComponent]),
                             e.getPoint, e.getModifiers, e.getWheelRotation)) }
      })
    }
  }


  peer.addPropertyChangeListener(new java.beans.PropertyChangeListener {
    def propertyChange(e: java.beans.PropertyChangeEvent) {
      e.getPropertyName match {
        case "font" => publish(FontChanged(Component.this))
        case "background" => publish(ForegroundChanged(Component.this))
        case "foreground" => publish(BackgroundChanged(Component.this))
        case _ =>
        /*case "focusable" =>
        case "focusTraversalKeysEnabled" =>
        case "forwardFocusTraversalKeys" =>
        case "backwardFocusTraversalKeys" =>
        case "upCycleFocusTraversalKeys" =>
        case "downCycleFocusTraversalKeys" =>
        case "focusTraversalPolicy" =>
        case "focusCycleRoot" =>*/
      }
    }
  })

  def revalidate() { peer.revalidate() }

  def requestFocus() { peer.requestFocus() }

  protected def paintComponent(g: java.awt.Graphics) {
    peer match {
      case peer: SuperMixin => peer.__super__paintComponent(g)
      case _ => // it's a wrapper created on the fly
    }
  }

  override def toString = "scala.swing wrapper " + peer.toString
}
