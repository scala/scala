/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package parsing

/**
 * Helper functions for parsing XML fragments
 */
trait TokenTests {

  /** {{{
   *  (#x20 | #x9 | #xD | #xA)
   *  }}} */
  final def isSpace(ch: Char): Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false
  }
  /** {{{
   *  (#x20 | #x9 | #xD | #xA)+
   *  }}} */
  final def isSpace(cs: Seq[Char]): Boolean = cs.nonEmpty && (cs forall isSpace)

  /** These are 99% sure to be redundant but refactoring on the safe side. */
  def isAlpha(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  def isAlphaDigit(c: Char) = isAlpha(c) || (c >= '0' && c <= '9')

  def isNameChar(c: Char): Boolean = (
    isNameStart(c) ||
    (c >= '0' && c <= '9') ||
    c == '-' ||
    c == '.' ||
    c == 0xB7 ||
    (c >= 0x300 && c <= 0x36F) ||
    (c >= 0x203F && c <= 0x2040)
  )
  def isNameStart(c: Char): Boolean = (
         if (c < 0x00C0)  isAlpha(c) || c == ':' || c == '_'
    else if (c < 0x0300)  c != 0xD7 && c != 0xF7
    else if (c < 0x2000)  c >= 0x370 && c != 0x37E
    else if (c < 0x3001)  c == 0x200C || c == 0x200D || (0x2070 to 0x218F contains c) ||
                          (0x2C00 to 0x2FEF contains c)
    else if (c < 0xD800)  true
    else if (c < 0x10000) (0xF900 to 0xFDCF contains c) || (0xFDF0 to 0xFFFD contains c)
    else                  false // codepoint < 0xF0000
  )

  /** {{{
   *  Name ::= ( Letter | '_' ) (NameChar)*
   *  }}}
   *  See [5] of XML 1.0 specification.
   */
  def isName(s: String) =
    s.nonEmpty && isNameStart(s.head) && (s.tail forall isNameChar)

  def isPubIDChar(ch: Char): Boolean =
    isAlphaDigit(ch) || (isSpace(ch) && ch != '\u0009') ||
    ("""-\()+,./:=?;!*#@$_%""" contains ch)

  /**
   * Returns `true` if the encoding name is a valid IANA encoding.
   * This method does not verify that there is a decoder available
   * for this encoding, only that the characters are valid for an
   * IANA encoding name.
   *
   * @param ianaEncoding The IANA encoding name.
   */
  def isValidIANAEncoding(ianaEncoding: Seq[Char]) = {
    def charOK(c: Char) = isAlphaDigit(c) || ("._-" contains c)

    ianaEncoding.nonEmpty && isAlpha(ianaEncoding.head) &&
    (ianaEncoding.tail forall charOK)
  }

  def checkSysID(s: String) = List('"', '\'') exists (c => !(s contains c))
  def checkPubID(s: String) = s forall isPubIDChar
}
