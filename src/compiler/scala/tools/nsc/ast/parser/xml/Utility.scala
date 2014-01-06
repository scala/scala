/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.nsc.ast.parser.xml

import scala.collection.mutable


/**
 * The `Utility` object provides utility functions for processing instances
 * of bound and not bound XML classes, as well as escaping text nodes.
 *
 * @author Burak Emir
 */
object Utility {
  import scala.reflect.internal.Chars.SU

  private val unescMap = Map(
    "lt"    -> '<',
    "gt"    -> '>',
    "amp"   -> '&',
    "quot"  -> '"',
    "apos"  -> '\''
  )

  /**
   * Appends unescaped string to `s`, `amp` becomes `&amp;`,
   * `lt` becomes `&lt;` etc..
   *
   * @return    `'''null'''` if `ref` was not a predefined entity.
   */
  private final def unescape(ref: String, s: StringBuilder): StringBuilder =
    ((unescMap get ref) map (s append _)).orNull

  def parseAttributeValue[T](value: String, text: String => T, entityRef: String => T): List[T] = {
    val sb  = new StringBuilder
    var rfb: StringBuilder = null
    val nb = new mutable.ListBuffer[T]()

    val it = value.iterator
    while (it.hasNext) {
      var c = it.next()
      // entity! flush buffer into text node
      if (c == '&') {
        c = it.next()
        if (c == '#') {
          c = it.next()
          val theChar = parseCharRef ({ ()=> c },{ () => c = it.next() },{s => throw new RuntimeException(s)}, {s => throw new RuntimeException(s)})
          sb.append(theChar)
        }
        else {
          if (rfb eq null) rfb = new StringBuilder()
          rfb append c
          c = it.next()
          while (c != ';') {
            rfb.append(c)
            c = it.next()
          }
          val ref = rfb.toString()
          rfb.clear()
          unescape(ref,sb) match {
            case null =>
              if (!sb.isEmpty) {  // flush buffer
                nb += text(sb.toString())
                sb.clear()
              }
              nb += entityRef(ref) // add entityref
            case _ =>
          }
        }
      }
      else sb append c
    }

    if(!sb.isEmpty) // flush buffer
      nb += text(sb.toString())

    nb.toList
  }

  /**
   * {{{
   *   CharRef ::= "&amp;#" '0'..'9' {'0'..'9'} ";"
   *             | "&amp;#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   * }}}
   * See [66]
   */
  def parseCharRef(ch: () => Char, nextch: () => Unit, reportSyntaxError: String => Unit, reportTruncatedError: String => Unit): String = {
    val hex  = (ch() == 'x') && { nextch(); true }
    val base = if (hex) 16 else 10
    var i = 0
    while (ch() != ';') {
      ch() match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + ch().asDigit
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (! hex)
            reportSyntaxError("hex char not allowed in decimal char ref\n" +
                              "Did you mean to write &#x ?")
          else
            i = i * base + ch().asDigit
        case SU =>
          reportTruncatedError("")
        case _ =>
          reportSyntaxError("character '" + ch() + "' not allowed in char ref\n")
      }
      nextch()
    }
    new String(Array(i), 0, 1)
  }

  /** {{{
   *  (#x20 | #x9 | #xD | #xA)
   *  }}} */
  final def isSpace(ch: Char): Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false
  }

  /** {{{
   *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   *  }}}
   *  See [4] and Appendix B of XML 1.0 specification.
  */
  def isNameChar(ch: Char) = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    isNameStart(ch) || (getType(ch).toByte match {
      case COMBINING_SPACING_MARK |
              ENCLOSING_MARK | NON_SPACING_MARK |
              MODIFIER_LETTER | DECIMAL_DIGIT_NUMBER => true
      case _                                         => ".-:" contains ch
    })
  }

  /** {{{
   *  NameStart ::= ( Letter | '_' )
   *  }}}
   *  where Letter means in one of the Unicode general
   *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
   *
   *  We do not allow a name to start with `:`.
   *  See [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Char) = {
    import java.lang.Character._

    getType(ch).toByte match {
      case LOWERCASE_LETTER |
              UPPERCASE_LETTER | OTHER_LETTER |
              TITLECASE_LETTER | LETTER_NUMBER => true
      case _                                   => ch == '_'
    }
  }
}
