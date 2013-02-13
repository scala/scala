package scala.swing

import event. {

  InternalFrameActivated,
  InternalFrameClosed,
  InternalFrameClosing,
  InternalFrameDeactivated,
  InternalFrameDeiconified,
  InternalFrameOpened,
  InternalFrameIconified,
  InternalFrameMaximized,
  InternalFrameUnmaximized

}

import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

import javax.swing. { Icon, JInternalFrame, JMenuBar }
import javax.swing.border.Border
import javax.swing.event. { InternalFrameEvent, InternalFrameListener }
import javax.swing.plaf.InternalFrameUI

import scala.swing. { Component, MenuBar, Point, Rectangle, RootPanel }

/** Companion of [[scala.swing.InternalFrame]].
 *
 * @author myst3r10n
 */
object InternalFrame {

  /** @see [[javax.swing.JInternalFrame]]. */
  object Property extends Enumeration {

    type Property = String

    /** @see [[javax.swing.JInternalFrame]]. */
    val Closed = JInternalFrame.IS_CLOSED_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val ContentPane = JInternalFrame.CONTENT_PANE_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val FrameIcon = JInternalFrame.FRAME_ICON_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val GlassPane = JInternalFrame.GLASS_PANE_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val Icon = JInternalFrame.IS_ICON_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val LayeredPane = JInternalFrame.LAYERED_PANE_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val Maximum = JInternalFrame.IS_MAXIMUM_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val MenuBar = JInternalFrame.MENU_BAR_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val RootPane = JInternalFrame.ROOT_PANE_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val Selected = JInternalFrame.IS_SELECTED_PROPERTY

    /** @see [[javax.swing.JInternalFrame]]. */
    val Title = JInternalFrame.TITLE_PROPERTY

  }

}

/** It's [[javax.swing.JInternalFrame]].
 *
 * @param t The String to display in the title bar.
 * @param r If true, the internal frame can be resized.
 * @param c If true, the internal frame can be closed.
 * @param m If true, the internal frame can be maximized.
 * @param i If true, the internal frame can be iconified.
 * 
 * @author myst3r10n
 */
class InternalFrame(t: String = "", r: Boolean = false, c: Boolean = false, m: Boolean = false, i: Boolean = false) extends Component with RootPanel {

  /** The [[javax.swing.JInternalFrame]] object. */
  override lazy val peer = new JInternalFrame(t, r, c, m, i)

  // Frame event handling.
  peer.addInternalFrameListener(new InternalFrameListener {

    // Activated event.
    def internalFrameActivated(event: InternalFrameEvent) { publish(InternalFrameActivated(InternalFrame.this)) }

    // Closed event.
    def internalFrameClosed(event: InternalFrameEvent) { publish(InternalFrameClosed(InternalFrame.this)) }

    // Closing event.
    def internalFrameClosing(event: InternalFrameEvent) { publish(InternalFrameClosing(InternalFrame.this)) }

    // Deactivated event.
    def internalFrameDeactivated(event: InternalFrameEvent) { publish(InternalFrameDeactivated(InternalFrame.this)) }

    // Deiconified event.
    def internalFrameDeiconified(event: InternalFrameEvent) { publish(InternalFrameDeiconified(InternalFrame.this)) }

    // Iconified event.
    def internalFrameIconified(event: InternalFrameEvent) { publish(InternalFrameIconified(InternalFrame.this)) }

    // Opened event.
    def internalFrameOpened(event: InternalFrameEvent) { publish(InternalFrameOpened(InternalFrame.this)) }

  } )

  // Properties event handling.
  peer.addPropertyChangeListener(new PropertyChangeListener {

    def propertyChange(event: PropertyChangeEvent) {

      // Maximize event toggled on.
      if(event.getPropertyName.equalsIgnoreCase(InternalFrame.Property.Maximum) &&
	      event.getNewValue.equals(true))
	publish(InternalFrameMaximized(InternalFrame.this))

      // Maximize event toggled off.
      else if(event.getPropertyName.equalsIgnoreCase(InternalFrame.Property.Maximum) &&
	      event.getNewValue.equals(false))
        publish(InternalFrameUnmaximized(InternalFrame.this))

    }
  })

  /** @see [[javax.swing.JInternalFrame]]. */
  def back = peer.toBack

  /** @see [[javax.swing.JInternalFrame]]. */
  def bounds_= (r: Rectangle) { peer.setBounds(r) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def closable = peer.isClosable

  /** @see [[javax.swing.JInternalFrame]]. */
  def closable_= (b: Boolean) { peer.setClosable(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def closed = peer.isClosed

  /** @see [[javax.swing.JInternalFrame]]. */
  def closed_= (b: Boolean) { peer.setClosed(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def desktop =
    if(InternalFrame.this.peer.getDesktopPane == null)
      null
    else
      new DesktopPane { override lazy val peer = InternalFrame.this.peer.getDesktopPane }

  /** @see [[javax.swing.JInternalFrame]]. */
  def dispose = peer.dispose

  /** @see [[javax.swing.JInternalFrame]]. */
  def doDefaultCloseAction = peer.doDefaultCloseAction

  /** @see [[javax.swing.JInternalFrame]]. */
  def focusCycleRoot = peer.isFocusCycleRoot

  /** @see [[javax.swing.JInternalFrame]]. */
  def focusCycleRoot_= (focusCycleRoot: Boolean) { peer.setFocusCycleRoot(focusCycleRoot) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def frameIcon = peer.getFrameIcon

  /** @see [[javax.swing.JInternalFrame]]. */
  def frameIcon_= (icon: Icon) { peer.setFrameIcon(icon) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def front = peer.toFront

  /** @see [[javax.swing.JInternalFrame]]. */
  def hide = peer.hide

  /** @see [[javax.swing.JInternalFrame]]. */
  def icon = peer.isIcon

  /** @see [[javax.swing.JInternalFrame]]. */
  def icon_= (b: Boolean) { peer.setIcon(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def iconifiable = peer.isIconifiable

  /** @see [[javax.swing.JInternalFrame]]. */
  def iconifiable_= (b: Boolean) { peer.setIconifiable(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def location_= (p: Point) { peer.setLocation(p) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def maximizable = peer.isMaximizable

  /** @see [[javax.swing.JInternalFrame]]. */
  def maximizable_= (b: Boolean) { peer.setMaximizable(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def maximum = peer.isMaximum

  /** @see [[javax.swing.JInternalFrame]]. */
  def maximum_= (b: Boolean) { peer.setMaximum(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def menuBar = 
    if(InternalFrame.this.peer.getJMenuBar == null)
      null
    else
      new MenuBar { override lazy val peer = InternalFrame.this.peer.getJMenuBar }

  /** @see [[javax.swing.JInternalFrame]]. */
  def menuBar_= (m: MenuBar) { peer.setJMenuBar(m.peer) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def normalBounds = peer.getNormalBounds

  /** @see [[javax.swing.JInternalFrame]]. */
  def normalBounds_= (r: Rectangle) { peer.setNormalBounds(r) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def resizable = peer.isResizable

  /** @see [[javax.swing.JInternalFrame]]. */
  def resizable_= (b: Boolean) { peer.setResizable(b) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def selected = peer.isSelected

  /** @see [[javax.swing.JInternalFrame]]. */
  def selected_= (selected: Boolean) { peer.setSelected(selected) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def show = peer.show

  /** @see [[javax.swing.JInternalFrame]]. */
  def title = peer.getTitle

  /** @see [[javax.swing.JInternalFrame]]. */
  def title_= (title: String) { peer.setTitle(title) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def ui = peer.getUI

  /** @see [[javax.swing.JInternalFrame]]. */
  def ui_= (ui: InternalFrameUI) { peer.setUI(ui) }

  /** @see [[javax.swing.JInternalFrame]]. */
  def updateUI = peer.updateUI

}
