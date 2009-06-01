package scala.swing

import java.awt.{Image, Window => AWTWindow}
import javax.swing._
import Swing._

object RichWindow {
  /**
   * Mixin this trait if you want an undecorated window.
   */
  trait Undecorated extends RichWindow {
    peer.setUndecorated(true)
  }
}

/**
 * A window that adds some functionality to the plain Window class and serves as
 * the common base class for frames and dialogs.
 *
 * Implementation note: this class is sealed since we need to know that a rich
 * window is either a dialog or a frame at some point.
 */
sealed trait RichWindow extends Window {
  def peer: AWTWindow with InterfaceMixin

  trait InterfaceMixin extends super.InterfaceMixin {
    def getJMenuBar: JMenuBar
    def setJMenuBar(b: JMenuBar)
    def setUndecorated(b: Boolean)
    def setTitle(s: String)
    def getTitle: String
    def setResizable(b: Boolean)
    def isResizable: Boolean
  }

  def title: String = peer.getTitle
  def title_=(s: String) = peer.setTitle(s)

  def menuBar: MenuBar = UIElement.cachedWrapper(peer.getJMenuBar)
  def menuBar_=(m: MenuBar) = peer.setJMenuBar(m.peer)

  def resizable_=(b: Boolean) { peer.setResizable(b) }
  def resizable = peer.isResizable
}

/**
 * A window with decoration such as a title, border, and action buttons.
 *
 * An AWT window cannot be wrapped dynamically with this class, i.e., you cannot
 * write something like new Window { def peer = myAWTWindow }
 *
 * @see javax.swing.JFrame
 */
class Frame extends RichWindow {
  override lazy val peer: JFrame with InterfaceMixin = new JFrame with InterfaceMixin with SuperMixin

  protected trait SuperMixin extends JFrame {
    override protected def processWindowEvent(e: java.awt.event.WindowEvent) {
      super.processWindowEvent(e)
      if (e.getID() == java.awt.event.WindowEvent.WINDOW_CLOSING)
        closeOperation()
    }
  }

  def iconImage: Image = peer.getIconImage
  def iconImage_=(i: Image) { peer.setIconImage(i) }
}

/**
 * Simple predefined dialogs.
 *
 * @see javax.swing.JOptionPane
 */
object Dialog {
  /**
   * The message type of a dialog.
   */
  object Message extends Enumeration {
    val Error = Value(JOptionPane.ERROR_MESSAGE)
    val Info = Value(JOptionPane.INFORMATION_MESSAGE)
    val Warning = Value(JOptionPane.WARNING_MESSAGE)
    val Question = Value(JOptionPane.QUESTION_MESSAGE)
    val Plain = Value(JOptionPane.PLAIN_MESSAGE)
  }

  /**
   * The possible answers a user can select.
   */
  object Options extends Enumeration {
    val Default = Value(JOptionPane.DEFAULT_OPTION)
    val YesNo = Value(JOptionPane.YES_NO_OPTION)
    val YesNoCancel = Value(JOptionPane.YES_NO_CANCEL_OPTION)
    val OkCancel = Value(JOptionPane.OK_CANCEL_OPTION)
  }

  /**
   * The selected result of dialog.
   */
  object Result extends Enumeration {
    val Yes = Value(JOptionPane.YES_OPTION)
    val Ok = Yes
    val No = Value(JOptionPane.NO_OPTION)
    val Cancel = Value(JOptionPane.CANCEL_OPTION)
    val Closed = Value(JOptionPane.CLOSED_OPTION)
  }


  def showConfirmation(parent: Component, message: String, title: String,
     optionType: Options.Value, messageType: Message.Value, icon: Icon): Result.Value =
     Result(JOptionPane.showConfirmDialog(nullPeer(parent), message, title,
                                   optionType.id, messageType.id, Swing.wrapIcon(icon)))
  def showConfirmation(parent: Component, message: String, title: String,
     optionType: Options.Value): Result.Value =
     Result(JOptionPane.showConfirmDialog(nullPeer(parent), message, title,
                                   optionType.id))

  def showOptions(parent: Component, message: String, title: String,
     optionType: Options.Value, messageType: Message.Value, icon: Icon,
     entries: Seq[Any], initialEntry: Int): Result.Value = {
       val r = JOptionPane.showOptionDialog(nullPeer(parent), message, title,
                                   optionType.id, messageType.id, Swing.wrapIcon(icon),
                                   entries.map(_.asInstanceOf[AnyRef]).toArray, entries(initialEntry))
       Result(r)
     }

  def showInput[A](parent: Component, message: String, title: String,
                   messageType: Message.Value, icon: Icon,
     entries: Seq[A], initialEntry: A): Option[A] = {
       val e = if (entries.isEmpty) null
               else entries.map(_.asInstanceOf[AnyRef]).toArray
       val r = JOptionPane.showInputDialog(nullPeer(parent), message, title,
                                         messageType.id, Swing.wrapIcon(icon),
                                         e, initialEntry)
       Swing.toOption(r)
  }
  def showMessage(parent: Component, message: String, title: String,
     messageType: Message.Value, icon: Icon) {
     JOptionPane.showMessageDialog(nullPeer(parent), message, title,
                                   messageType.id, Swing.wrapIcon(icon))
  }

  def showMessage(parent: Component, message: String) {
     JOptionPane.showMessageDialog(nullPeer(parent), message)
  }
}

/**
 * A dialog window.
 *
 * @see javax.swing.JDialog
 */
class Dialog(owner: Window) extends RichWindow {
  override lazy val peer: JDialog with InterfaceMixin =
    if (owner == null) new JDialog with InterfaceMixin
    else owner match {
      case f: Frame => new JDialog(f.peer) with InterfaceMixin
      case d: Dialog => new JDialog(d.peer) with InterfaceMixin
    }

  def this() = this(null)

  def modal_=(b: Boolean) { peer.setModal(b) }
  def modal = peer.isModal
}

