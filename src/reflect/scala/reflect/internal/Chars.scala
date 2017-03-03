/* NSC -- new Scala compiler
 * Copyright 2006-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala
package reflect
package internal

import scala.language.postfixOps

import scala.annotation.switch
import java.lang.{ Character => JCharacter }

/** Contains constants and classifier methods for characters */
trait Chars {
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
   *  -1 if no success
   */
  def digit2int(ch: Char, base: Int): Int = {
    val num = (
      if (ch <= '9') ch - '0'
      else if ('a' <= ch && ch <= 'z') ch - 'a' + 10
      else if ('A' <= ch && ch <= 'Z') ch - 'A' + 10
      else -1
    )
    if (0 <= num && num < base) num else -1
  }
  /** Buffer for creating '\ u XXXX' strings. */
  private[this] val char2uescapeArray = Array[Char]('\\', 'u', 0, 0, 0, 0)

  /** Convert a character to a backslash-u escape */
  def char2uescape(c: Char): String = {
    @inline def hexChar(ch: Int): Char =
      ( if (ch < 10) '0' else 'A' - 10 ) + ch toChar

    char2uescapeArray(2) = hexChar((c >> 12)     )
    char2uescapeArray(3) = hexChar((c >>  8) % 16)
    char2uescapeArray(4) = hexChar((c >>  4) % 16)
    char2uescapeArray(5) = hexChar((c      ) % 16)

    new String(char2uescapeArray)
  }

  /** Is character a line break? */
  def isLineBreakChar(c: Char) = (c: @switch) match {
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

  private final val otherLetters = Set[Char]('\u0024', '\u005F')  // '$' and '_'
  private final val letterGroups = {
    import JCharacter._
    Set[Byte](LOWERCASE_LETTER, UPPERCASE_LETTER, OTHER_LETTER, TITLECASE_LETTER, LETTER_NUMBER)
  }
  def isScalaLetter(ch: Char) = letterGroups(JCharacter.getType(ch).toByte) || otherLetters(ch)

  /** Can character form part of a Scala operator name? */
  def isOperatorPart(c : Char) : Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }
}

object Chars extends Chars { }
