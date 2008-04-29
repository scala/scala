package scala.swing

import javax.swing._
import java.awt.event._
import event._

/**
 * @see javax.swing.JPasswordField
 */
class PasswordField(text0: String, columns0: Int) extends TextField(text0, columns0) {
  override lazy val peer: JPasswordField = new JPasswordField(text0, columns0)
  def this(text: String) = this(text, 0)
  def this(columns: Int) = this("", columns)
  def this() = this("")

  def echoChar: Char = peer.getEchoChar
  def echoChar_=(c: Char) = peer.setEchoChar(c)

  /**
   * The text property should not be used on a password field for
   * security reasons.
   */
  override def text: String = ""
  override def text_=(s: String) {}
  def password: Array[Char] = peer.getPassword
}
