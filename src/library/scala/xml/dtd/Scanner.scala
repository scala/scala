/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.xml
package dtd

/** Scanner for regexps (content models in DTD element declarations)
 *  todo: cleanup
 */
class Scanner extends Tokens with parsing.TokenTests {

  final val ENDCH = '\u0000'

  var token:Int = END
  var value:String = _

  private var it: Iterator[Char] = null
  private var c: Char = 'z'

  /** initializes the scanner on input s */
  final def initScanner(s: String) {
    value = ""
    it = (s).iterator
    token = 1+END
    next
    nextToken
  }

  /** scans the next token */
  final def nextToken() {
    if (token != END) token = readToken
  }

  // todo: see XML specification... probably isLetter,isDigit is fine
  final def isIdentChar = ( ('a' <= c && c <= 'z')
                           || ('A' <= c && c <= 'Z'));

  final def next() = if (it.hasNext) c = it.next else c = ENDCH

  final def acc(d: Char) {
    if (c == d) next else scala.sys.error("expected '"+d+"' found '"+c+"' !");
  }

  final def accS(ds: Seq[Char]) { ds foreach acc }

  final def readToken: Int =
    if (isSpace(c)) {
      while (isSpace(c)) c = it.next
      S
    } else c match {
      case '('   => next; LPAREN
      case ')'   => next; RPAREN
      case ','   => next; COMMA
      case '*'   => next; STAR
      case '+'   => next; PLUS
      case '?'   => next; OPT
      case '|'   => next; CHOICE
      case '#'   => next; accS( "PCDATA" ); TOKEN_PCDATA
      case ENDCH => END
      case _     =>
        if (isNameStart(c)) name; // NAME
        else scala.sys.error("unexpected character:" + c)
    }

  final def name = {
    val sb = new StringBuilder()
    do { sb.append(c); next } while (isNameChar(c));
    value = sb.toString()
    NAME
  }

}
