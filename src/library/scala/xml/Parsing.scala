/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml


/** The object <code>Parsing</code> ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 *
 *  @deprecated use either <code>parsing.TokenTests</code> or
 *              <code>Utilty</code> (helper functions for parsing XML fragments).
 */
object Parsing {

  /** <pre>(#x20 | #x9 | #xD | #xA)</pre> */
  final def isSpace(ch: Char): Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false
  }

  /** <pre>(#x20 | #x9 | #xD | #xA)+</pre> */
  final def isSpace(cs: Seq[Char]): Boolean = {
    val it = cs.iterator
    it.hasNext && it.forall { isSpace }
  }

  /** <pre>NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender</pre>
   *
   * see [4] and Appendix B of XML 1.0 specification
   */
  def isNameChar(ch: Char) = isNameStart(ch) || (ch match {
    case '.' | '-' | ':' => true
    case _ => java.lang.Character.getType(ch).asInstanceOf[Byte] match {
      case java.lang.Character.COMBINING_SPACING_MARK => true  // Mc
      case java.lang.Character.ENCLOSING_MARK => true          // Me
      case java.lang.Character.NON_SPACING_MARK => true        // Mn
      case java.lang.Character.MODIFIER_LETTER => true         // Lm
      case java.lang.Character.DECIMAL_DIGIT_NUMBER => true    // Nd
      case _ => false
    }
  });

  /** <pre>NameStart ::= ( Letter | '_' )</pre>
   *  where Letter means in one of the Unicode general
   *  categories { Ll, Lu, Lo, Lt, Nl }
   *
   *  We do not allow a name to start with ':'.
   *  see [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Char) =
    java.lang.Character.getType(ch).asInstanceOf[Byte] match {
      case java.lang.Character.LOWERCASE_LETTER => true
      case java.lang.Character.UPPERCASE_LETTER => true
      case java.lang.Character.OTHER_LETTER     => true
      case java.lang.Character.TITLECASE_LETTER => true
      case java.lang.Character.LETTER_NUMBER    => true
      case _ => ch == '_'
    }

  /** <pre>Name ::= ( Letter | '_' ) (NameChar)*</pre>
   *
   *  see  [5] of XML 1.0 specification
   */
  def isName(s: String): Boolean =
    if (s.length() > 0) {
      val z: Seq[Char] = s
      val y = z.iterator
      if (isNameStart(y.next)) {
        while (y.hasNext && isNameChar(y.next)) {}
        !y.hasNext
      } else false
    } else false

  def isPubIDChar(c: Char) = c match {
    case '\u0020' | '\u000D' | '\u000A' => true
    case _ if
      ('0' < c && c < '9')||('a' < c && c < 'z')||('A' < c && c < 'Z') => true
    case '-' | '\''| '(' | ')' | '+' | ',' | '.' | '/' | ':'  | '=' |
         '?' | ';' | '!' | '*' | '#' | '@' | '$' | '_' | '%'           => true
    case _ => false
  }

  def checkSysID(s: String): Boolean =
    s.indexOf('"') == -1 || s.indexOf('\'') == -1

  def checkPubID(s: String): Boolean =
    if (s.length() > 0) {
      val z:Seq[Char] = s
      val y = z.iterator
      while (y.hasNext && isPubIDChar(y.next)) {}
      !y.hasNext
    } else true

}
