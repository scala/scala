/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.util
package color

case object Reset extends AnsiAttr(0)
case object Bright extends AnsiAttr(1)
case object Faint extends AnsiAttr(2)
case object Italic extends AnsiAttr(3)
case object Underline extends AnsiAttr(4)
case object Blink extends AnsiAttr(5)
case object Inverse extends AnsiAttr(7)
case object Hidden extends AnsiAttr(8)
case object Strikethrough extends AnsiAttr(9)

case object Black extends AnsiForeground(30)
case object Red extends AnsiForeground(31)
case object Green extends AnsiForeground(32)
case object Yellow extends AnsiForeground(33)
case object Blue extends AnsiForeground(34)
case object Magenta extends AnsiForeground(35)
case object Cyan extends AnsiForeground(36)
case object White extends AnsiForeground(37)
case object Default extends AnsiForeground(39)

/** One piece of an ansi control sequence.  Either a color
 *  (foreground or background) or an attribute (e.g. bright, underline.)
 *  Control sequences are created from AnsiAtoms with the / operator.
 */
trait AnsiAtom {
  def code: Int
  def isAttr: Boolean
}
sealed abstract class AnsiAttr(val code: Int) extends AnsiAtom {
  final def isAttr = true
}
sealed abstract class AnsiColor(val code: Int) extends AnsiAtom {
  final def isAttr = false
  def flip: AnsiColor
}
sealed abstract class AnsiForeground(code: Int) extends AnsiColor(code) {
  require(30 <= code && code <= 39, code)
  val flip: AnsiBackground = new AnsiBackground(this)
}
sealed class AnsiBackground(val flip: AnsiForeground) extends AnsiColor(flip.code + 10) {
  require(40 <= code && code <= 49, code)
  override def toString = "(on " + flip + " background)"
}
