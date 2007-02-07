/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.xml.dtd;


/** Scanner for regexps (content models in DTD element declarations)
 *  todo: cleanup
 */
class Scanner extends Tokens with parsing.TokenTests {

  //                                                   zzz   constants   zzz
  final val ENDCH = '\u0000';

  //                                                      zzz   fields   zzz
  var token:Int = END;
  var value:String = _;

  private var it:Iterator[Char] = null;
  private var c:Char = 'z';


  /** initializes the scanner on input s */
  final def initScanner( s:String ) = {
    //Console.println("[scanner init on \""+s+"\"]");
    value = "";
    it = Iterator.fromString( s );
    token = 1+END;
    next;
    nextToken;
  }

  /** scans the next token */
  final def nextToken:Unit = {
    if( token != END ) token = readToken;
    //Console.println("["+token2string( token )+"]");
  }

  //                                            zzz    scanner methods   zzz

  // todo: see XML specification... probably isLetter,isDigit is fine
  final def isIdentChar = ( ('a' <= c && c <= 'z')
                           || ('A' <= c && c <= 'Z'));

  final def next = if( it.hasNext ) c = it.next else c = ENDCH;

  final def acc( d:char ):Unit =
    if( c == d ) next; else error("expected '"+d+"' found '"+c+"' !");

  final def accS( ds:Seq[Char] ):Unit = {
    val jt = ds.elements; while( jt.hasNext ) { acc( jt.next ) }
  }

  /*
  final def isSpace = c match {
    case  '\u0020' | '\u0009' | '\u000D' | '\u000A' => true
    case _ => false;
  }
  */

  final def readToken: Int =
    if(isSpace(c)) {
      while( isSpace(c) ) {
        c = it.next;
      }
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
      case ENDCH => END;
      case _     =>
        if( isNameStart( c ) )      name; // NAME
        else {
          error("unexpected character:"+c); END
        }
    }

  final def name = {
    val sb = new compat.StringBuilder();
    do { sb.append( c ); next } while ( isNameChar( c ) ) ;
    value = sb.toString();
    NAME
  }

}
