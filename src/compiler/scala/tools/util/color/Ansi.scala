/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.util
package color

import collection.mutable

object Ansi {
  final val ESC = '\u001b'                    // <esc>
  final val LBR = '\u005b'                    // [
  final val CSI = new String(Array(ESC, LBR)) // control sequence introducer
  final val CSI_FINAL = "m"                   // control sequence final byte
  
  def colors  = List(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White)
  def effects = List(Reset, Bright, Faint, Italic, Underline, Blink, Inverse, Hidden, Strikethrough)
  
  // No, that's not the finale of "CSI: Crime Scene Investigation."

  def colorizerFor(codes: Seq[Int]): String => String =
    s => ansiCodeToString(codes) + s + ansiCodeToString(0)
    
  def ansiCodeToString(code: Int): String = CSI + code + CSI_FINAL
  def ansiCodeToString(codes: Seq[Int]): String = codes.mkString(CSI, ";", CSI_FINAL)
}

/** An ansi control sequence.  The colorize function prepends
 *  the control sequence to the given String and appends a
 *  reset sequence.
 */
class Ansi(atoms0: List[AnsiAtom]) {
  val atoms    = atoms0 sortBy (x => (!x.isAttr, x.isInstanceOf[AnsiBackground]))
  val colorize = Ansi colorizerFor codes

  def codes = atoms map (_.code)
  def /(that: AnsiAtom) = new Ansi(atoms :+ that)
  // This looks redundant with / , but isn't - it is a way
  // to ensure that the argument will be a background color,
  // even if a foreground color is passed as an argument
  // (as it will be implicitly converted.)
  def on(that: AnsiBackground) = this / that

  // Convenience functions.
  def reset         = this / Reset
  def bright        = this / Bright
  def faint         = this / Faint
  def italic        = this / Italic
  def underline     = this / Underline
  def blink         = this / Blink
  def inverse       = this / Inverse
  def hidden        = this / Hidden
  def strikethrough = this / Strikethrough

  // adjectives first
  override def toString = atoms mkString " "
}
