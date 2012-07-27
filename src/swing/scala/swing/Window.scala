/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import java.awt.{Window => AWTWindow}
import event._
import javax.swing._

/**
 * A window with decoration such as a title, border, and action buttons.
 *
 * An AWT window cannot be wrapped dynamically with this class, i.e., you cannot
 * write something like new Window { def peer = myAWTWindow }
 *
 * @see javax.swing.JFrame
 */
abstract class Window extends UIElement with RootPanel with Publisher { outer =>
  def peer: AWTWindow with InterfaceMixin

  protected trait InterfaceMixin extends javax.swing.RootPaneContainer

  protected trait SuperMixin extends AWTWindow {
    override protected def processWindowEvent(e: java.awt.event.WindowEvent) {
      super.processWindowEvent(e)
      if (e.getID() == java.awt.event.WindowEvent.WINDOW_CLOSING)
        closeOperation()
    }
  }

  /**
   * This method is called when the window is closing, after all other window
   * event listeners have been processed.
   */
  def closeOperation() {}

  override def contents_=(c: Component) {
    super.contents_=(c)
    peer.pack() // pack also validates, which is generally required after an add
  }
  def defaultButton: Option[Button] =
    toOption(peer.getRootPane.getDefaultButton) map UIElement.cachedWrapper[Button]
  def defaultButton_=(b: Button) {
    peer.getRootPane.setDefaultButton(b.peer)
  }
  def defaultButton_=(b: Option[Button]) {
    peer.getRootPane.setDefaultButton(b.map(_.peer).orNull)
  }

  def dispose() { peer.dispose() }

  def pack(): this.type = { peer.pack(); this }

  def setLocationRelativeTo(c: UIElement) { peer.setLocationRelativeTo(c.peer) }
  def centerOnScreen() { peer.setLocationRelativeTo(null) }
  def location_=(p: Point) { peer.setLocation(p) }
  def size_=(size: Dimension) { peer.setSize(size) }
  def bounds_=(rect: Rectangle) { peer.setBounds(rect) }

  def owner: Window = UIElement.cachedWrapper[Window](peer.getOwner)

  def open() { peer setVisible true }
  def close() { peer setVisible false }

  peer.addWindowListener(new java.awt.event.WindowListener {
    def windowActivated(e: java.awt.event.WindowEvent) { publish(WindowActivated(outer)) }
    def windowClosed(e: java.awt.event.WindowEvent) { publish(WindowClosed(outer)) }
    def windowClosing(e: java.awt.event.WindowEvent) { publish(WindowClosing(outer)) }
    def windowDeactivated(e: java.awt.event.WindowEvent) { publish(WindowDeactivated(outer)) }
    def windowDeiconified(e: java.awt.event.WindowEvent) { publish(WindowDeiconified(outer)) }
    def windowIconified(e: java.awt.event.WindowEvent) { publish(WindowIconified(outer)) }
    def windowOpened(e: java.awt.event.WindowEvent) { publish(WindowOpened(outer)) }
  })
}
