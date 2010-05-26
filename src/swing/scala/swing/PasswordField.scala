/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.swing

import event._
import javax.swing._
import java.awt.event._

/**
 * A password field, that displays a replacement character for each character in the password.
 *
 * @see javax.swing.JPasswordField
 */
class PasswordField(text0: String, columns0: Int) extends TextField(text0, columns0) {
  override lazy val peer: JPasswordField = new JPasswordField(text0, columns0) with SuperMixin
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
