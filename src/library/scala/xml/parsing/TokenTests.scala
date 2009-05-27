/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.parsing;


/**
 * Helper functions for parsing XML fragments
 */
trait TokenTests {

  /** (#x20 | #x9 | #xD | #xA) */
  final def isSpace( ch:Char ): Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false;
  }

  /** (#x20 | #x9 | #xD | #xA)+ */
  final def isSpace(cs: Seq[Char]): Boolean = {
    val it = cs.iterator;
    it.hasNext && it.forall { isSpace };
  }

  /** NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   *
   * see [4] and Appendix B of XML 1.0 specification
  */
  def isNameChar(ch: Char) = isNameStart(ch) || (ch match {
    case '.' | '-' | ':' => true;
    case _ => java.lang.Character.getType( ch ).asInstanceOf[Byte] match {
      case java.lang.Character.COMBINING_SPACING_MARK => true; // Mc
      case java.lang.Character.ENCLOSING_MARK => true;         // Me
      case java.lang.Character.NON_SPACING_MARK => true;       // Mn
      case java.lang.Character.MODIFIER_LETTER => true;        // Lm
      case java.lang.Character.DECIMAL_DIGIT_NUMBER => true;   // Nd
      case _ => false;
    }
  });

  /** NameStart ::= ( Letter | '_' )
   *  where Letter means in one of the Unicode general
   *  categories { Ll, Lu, Lo, Lt, Nl }
   *
   *  We do not allow a name to start with ':'.
   *  see [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Char) =
    java.lang.Character.getType(ch).asInstanceOf[Byte] match {
      case java.lang.Character.LOWERCASE_LETTER => true;
      case java.lang.Character.UPPERCASE_LETTER => true;
      case java.lang.Character.OTHER_LETTER     => true;
      case java.lang.Character.TITLECASE_LETTER => true;
      case java.lang.Character.LETTER_NUMBER    => true;
      case _ => ch match {
        case '_' => true
        case _ => false;
      }
    }

  /** Name ::= ( Letter | '_' ) (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def isName(s: String): Boolean = {
    if( s.length() > 0 ) {
      val y           = s.iterator;
      if (isNameStart(y.next)) {
        while (y.hasNext && isNameChar(y.next)) {};
        !y.hasNext
      } else false;
    } else false;
  }

  def isPubIDChar(ch: Char): Boolean = {
    //Console.println("char: '" + ch + "'");
    ch match {
      case '\u0020' | '\u000D' | '\u000A' => true;
      case _ if
        (('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'z') ||
         ('A' <= ch && ch <= 'Z')) => true;
      case '-' | '\''| '(' | ')' | '+' | ',' | '.' |
           '/' | ':' | '=' | '?' | ';' | '!' | '*' |
           '#' | '@' | '$' | '_' | '%' => true
      case _ =>
        //Console.println("false: '" + ch + "'");
        false;
    }
  }

  /**
   * Returns true if the encoding name is a valid IANA encoding.
   * This method does not verify that there is a decoder available
   * for this encoding, only that the characters are valid for an
   * IANA encoding name.
   *
   * @param ianaEncoding The IANA encoding name.
   */
  def isValidIANAEncoding(ianaEncoding: Seq[Char]): Boolean = {
    val it = ianaEncoding.iterator;
    if (!it.hasNext)
      return false;

    var c = it.next;
    if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
      while (it.hasNext) {
        c = it.next;
        if ((c < 'A' || c > 'Z') && (c < 'a' || c > 'z') &&
            (c < '0' || c > '9') && c != '.' && c != '_' &&
            c != '-') {
              return false;
            }
      }
      return true;
    } else
      return false;
  } // isValidIANAEncoding(String): Boolean

  def checkSysID( s:String ): Boolean = {
    s.indexOf('"'.asInstanceOf[Int]) == -1 || s.indexOf('\''.asInstanceOf[Int]) == -1
  }

  def checkPubID(s: String): Boolean = {
    //Console.println("checkPubID of \""+s+"\"");
    if (s.length() > 0) {
      val y = s.iterator;
      var c = ' ';
      while (y.hasNext && isPubIDChar(c)) {
        //Console.println(c);
        c = y.next
      };
      !y.hasNext
    }
    else
      true
  }

}
