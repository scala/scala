/* NSC -- new Scala compiler
 * Copyright 2006-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import annotation.{ tailrec, switch }

/** Contains constants and classifier methods for characters */
object Chars {
  // Be very careful touching these.
  // Apparently trivial changes to the way you write these constants
  // will cause Scanners.scala to go from a nice efficient switch to
  // a ghastly nested if statement which will bring the type checker
  // to its knees. See ticket #1456
  // Martin: (this should be verified now that the pattern rules have been redesigned).
  final val LF = '\u000A'
  final val FF = '\u000C'
  final val CR = '\u000D'
  final val SU = '\u001A'

  /** Convert a character digit to an Int according to given base,
   *  -1 if no success */
  def digit2int(ch: Char, base: Int): Int = {
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1
  }

  /** Is character a line break? */
  @inline def isLineBreakChar(c: Char) = (c: @switch) match {
    case LF|FF|CR|SU  => true
    case _            => false
  }

  /** Is character a whitespace character (but not a new line)? */
  def isWhitespace(c: Char) =
    c == ' ' || c == '\t' || c == CR

  /** Can character form part of a doc comment variable $xxx? */
  def isVarPart(c: Char) =
    '0' <= c && c <= '9' || 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'

  /** Can character start an alphanumeric Scala identifier? */
  def isIdentifierStart(c: Char): Boolean =
    (c == '_') || (c == '$') || Character.isUnicodeIdentifierStart(c)

  /** Can character form part of an alphanumeric Scala identifier? */
  def isIdentifierPart(c: Char) =
    (c == '$') || Character.isUnicodeIdentifierPart(c)

  /** Is character a math or other symbol in Unicode?  */
  def isSpecial(c: Char) = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL.toInt || chtp == Character.OTHER_SYMBOL.toInt
  }

  /** Can character form part of a Scala operator name? */
  def isOperatorPart(c : Char) : Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }
}

