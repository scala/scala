/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.swing

import javax.swing._
import java.awt.event._
import event._


/*object TextField {
  object FocusLostBehavior extends Enumeration {
    val Revert = Value(JFormattedTextField.REVERT)
    val Commit = Value(JFormattedTextField.REVERT)
    val CommitOrRevert = Value(JFormattedTextField.REVERT)
    val Persist = Value(JFormattedTextField.REVERT)
  }
}*/

/**
 * A text component that allows single line text input and display.
 *
 * @see javax.swing.JTextField
 */
class TextField(text0: String, columns0: Int) extends TextComponent with TextComponent.HasColumns {
  override lazy val peer: JTextField = new JTextField(text0, columns0) with SuperMixin
  def this(text: String) = this(text, 0)
  def this(columns: Int) = this("", columns)
  def this() = this("")

  def columns: Int = peer.getColumns
  def columns_=(n: Int) = peer.setColumns(n)

  private lazy val actionListener = Swing.ActionListener { e =>
    publish(EditDone(TextField.this))
  }

  override def onFirstSubscribe {
    super.onFirstSubscribe
    peer.addActionListener(actionListener)
    peer.addFocusListener(new FocusAdapter {
      override def focusLost(e: java.awt.event.FocusEvent) { publish(EditDone(TextField.this)) }
    })
  }

  override def onLastUnsubscribe {
    super.onLastUnsubscribe
    peer.removeActionListener(actionListener)
  }

  def verifier: String => Boolean = s => peer.getInputVerifier.verify(peer)
  def verifier_=(v: String => Boolean) {
    peer.setInputVerifier(new InputVerifier {
      def verify(c: JComponent) = v(text)
      override def shouldYieldFocus(c: JComponent) =
        peer.getInputVerifier.shouldYieldFocus(c)
    })
  }
  def shouldYieldFocus: String=>Boolean = s => peer.getInputVerifier.shouldYieldFocus(peer)
  def shouldYieldFocus_=(y: String=>Boolean) {
    peer.setInputVerifier(new InputVerifier {
      def verify(c: JComponent) = peer.getInputVerifier.verify(c)
      override def shouldYieldFocus(c: JComponent) = y(text)
    })
  }
}
