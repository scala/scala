/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2004, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac._;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.SourceRepresentation;

package scala.tools.scalac.ast.parser {

import scala.tools.util.Position;
import scala.tools.util.SourceFile;


/** A scanner for the programming language Scala.
 *
 *  @author     Matthias Zenger, Martin Odersky, Burak Emir
 *  @version    1.1
 */
class Scanner(_unit: Unit) extends TokenData {

  import Tokens._;
  import java.lang.{Integer, Long, Float, Double}

  val unit = _unit;

  val cbuf = new StringBuffer();

  /** buffer for the documentation comment
  */
  var docBuffer: StringBuffer = null;

  /** add the given character to the documentation buffer
  */
  protected def addCharToDoc(ch: char): unit =
      if (docBuffer != null) docBuffer.append(ch);

  /** layout & character constants
  */
  val tabinc = 8;
  val LF = SourceFile.LF;
  val FF = SourceFile.FF;
  val CR = SourceFile.CR;
  val SU = SourceFile.SU;

  /** we need one token lookahead
  */
  val next = new TokenData();
  val prev = new TokenData();

  /** the first character position after the previous token
  */
  var lastpos = 0;

  /** the last error position
  */
  var errpos = -1;

  /** the input buffer:
  var buf: Array[char] = unit.source.getContent();
  var bp: int = -1;
  */

  class SourceIterator(charArray:Array[char]) extends  Iterator[char] {
    val buf:Array[char] = charArray;
    var bp: int = -1;
    /* inv: true if buf( bp ) is last of an odd number of ASCII '\' */
    var odd = false;
    var unicode1:int = 0;
    var unicode2:int = 0;
    def hasMore( i:int, j:int ) = i + j < buf.length;
    def hasMore( j:int ) = bp + j < buf.length;
    def hasNext = hasMore( 1 );

    /** gets next char, handles unicode transform */
    def next:char = {
      bp = bp + 1;
      val ch = buf( bp );
      odd = ( ch == '\\' ) && !odd;
      if( odd && hasNext && 'u' == buf( bp + 1 )) {
        val Pair( newch, offset ) = nextUnicode( bp + 1 );
        bp = bp + offset;
        ccol = ccol + offset;
        odd = false;
        newch
      } else {
        ch
      }
    }
    def raw:char = { bp = bp + 1; buf( bp ) }

    /** precondition: hasNext
    */
    def lookahead1 = {
      val ahead1 = buf( bp + 1 );
      if( ahead1 == '\\' && !odd && hasMore( 2 ) && 'u' == buf( bp + 1 )) {
        val Pair( newch, offset ) = nextUnicode( bp + 1 );
        unicode1 = offset;
        bp = bp + offset;
        newch;
      } else {
        unicode1 = 0;
        ahead1
      }
    }

    /** precondition: hasMore( 2 ) */
    def lookahead2 = {
      val j = bp + unicode1 + 1 + 1;
      val ahead2 = buf( j );
      val even1 = unicode1 > 0 || '\\' != buf( j - 1 ) || odd;
      if( ahead2 == '\\' && even1 && hasMore( j, 1 ) && 'u' == buf( j + 1 )) {
        val Pair( newch, offset ) = nextUnicode( j + 1 );
        unicode2 = offset;
        newch;
      } else {
        ahead2
      }
    }

    /** precondition: hasMore( 3 ) */
    def lookahead3 = {
      val j = bp + unicode1 + 1 + unicode2 + 1 + 1;
      val ahead3 = buf( j );
      var even2 = unicode2 > 0 || '\\' != buf( j - 1 ) || '\\' == buf( j - 2 );

      if( ahead3 == '\\' && even2 && hasMore( j, 1 ) && 'u' == buf( j + 1 )) {
        val Pair( newch, offset ) = nextUnicode( j + 1 );
        newch;
      } else {
        ahead3
      }
    }

    import SourceRepresentation.digit2int;

    /** returns unicode and offset of next character */
    def nextUnicode( p:int ):Pair[char,int] = {
      var j = p;
      while ( buf( j ) == 'u' ) { j = j + 1 };
      if ( j + 4 >= buf.length ) syntaxError("incomplete unicode escape");
      def munch = {
        val i = digit2int( buf( j ), 16 );
        if( i == -1 ) {
          syntaxError("error in unicode escape");
        };
        j = j + 1;
        i
      }
      var code:int = munch;
      //Console.println("nextUnicode2, code ="+code);
      code = (code << 4) + munch;
      code = (code << 4) + munch;
      code = (code << 4) + munch;
      Pair( code.asInstanceOf[char], j - p )
    }

  } /* class SourceIterator */

  /** the input iterator. Converts unicode characters as in Java spec (3.3)
  */
  var srcIterator:SourceIterator = new SourceIterator(unit.source.getContent());

  /** the current character
  */
  var ch: char = _;

  /** the line and column position of the current character
  */
  var cline: int = 1;
  var ccol: int = 0;

  /** a buffer for character and string literals
  */
  var lit = new Array[char](64);


  /** INIT: Construct a scanner from a file input stream.
  */
  token = EMPTY;
  nextch();
  Tokens; // initialize tokens
  nextToken();

  def nextch(): unit = {
    ch = srcIterator.next;
    ccol = ccol + 1;
    //System.out.print("[" + ch + "]");//DEBUG
  }

  /** read next token and return last position
  */
  def skipToken(): int = {
    val p = pos; nextToken(); p
  }

  def nextToken(): unit = {
    if (token == RBRACE) {
      val prevpos = pos;
      fetchToken();
      token match {
        case ELSE | EXTENDS | WITH | YIELD | CATCH | FINALLY |
          COMMA | SEMI | DOT | COLON | EQUALS | ARROW |
          LARROW | SUBTYPE | SUPERTYPE | HASH | AT |
          RPAREN | RBRACKET | RBRACE =>
        case _ =>
          if (token == EOF ||
              ((pos >>> Position.COLUMN_BITS) >
               (prevpos >>> Position.COLUMN_BITS))) {
                 next.copyFrom(this);
                 this.token = SEMI;
                 this.pos = prevpos;
               }
      }
    } else {
      if (next.token == EMPTY) {
        fetchToken();
      } else {
        copyFrom(next);
        next.token = EMPTY
      }
      if (token == CASE) {
        prev.copyFrom(this);
        fetchToken();
        if (token == CLASS) {
          token = CASECLASS;
        } else if (token == OBJECT) {
          token = CASEOBJECT;
        } else {
          next.copyFrom(this);
          this.copyFrom(prev);
        }
      } else if (token == SEMI) {
        prev.copyFrom(this);
        fetchToken();
        if (token != ELSE) {
          next.copyFrom(this);
          this.copyFrom(prev);
        }
      }
      //Console.println("<" + token2string(token) + ":" + name + ">");//DEBUG
    }
  }

  /** read next token
  */
  private def fetchToken(): unit = {
    if (token == EOF) return;
    lastpos = Position.encode(cline, ccol);
    //var index = bp;
    while (true) {
      ch match {
        case ' ' =>
          nextch();
        case '\t' =>
          ccol = ((ccol - 1) / tabinc * tabinc) + tabinc;
          nextch();
        case CR =>
          cline = cline + 1;
          ccol = 0;
          nextch();
          if (ch == LF) {
            ccol = 0;
            nextch();
	  }
        case LF | FF =>
          cline = cline + 1;
          ccol = 0;
          nextch();
        case _ =>
	  pos = Position.encode(cline, ccol);
	  //index = bp;
	  ch match {
            case '\u21D2' =>
              nextch(); token = ARROW;
              return;
	    case 'A' | 'B' | 'C' | 'D' | 'E' |
		 'F' | 'G' | 'H' | 'I' | 'J' |
		 'K' | 'L' | 'M' | 'N' | 'O' |
		 'P' | 'Q' | 'R' | 'S' | 'T' |
		 'U' | 'V' | 'W' | 'X' | 'Y' |
		 'Z' | '$' | '_' |
		 'a' | 'b' | 'c' | 'd' | 'e' |
		 'f' | 'g' | 'h' | 'i' | 'j' |
		 'k' | 'l' | 'm' | 'n' | 'o' |
		 'p' | 'q' | 'r' | 's' | 't' |
		 'u' | 'v' | 'w' | 'x' | 'y' |  // scala-mode: need to understand multi-line case patterns
		 'z' =>
              putChar( ch );
	      nextch();
	      getIdentRest;  // scala-mode: wrong indent for multi-line case blocks
	      return;
            case _ if (java.lang.Character.isUnicodeIdentifierStart(ch)) =>
              putChar( ch );
	      nextch();
	      getIdentRest;
              return;
	    case '~' | '!' | '@' | '#' | '%' |
		 '^' | '*' | '+' | '-' | '<' |
		 '>' | '?' | ':' | '=' | '&' |
                 '|' | '\\' =>
              putChar( ch );
	      nextch();
	      getOperatorRest; // XXX
	      return;
	    case '/' =>
	      nextch();
	      if (!skipComment()) {
                putChar( '/' );
		getOperatorRest;
		return;
	      }
	    case '0' =>
              putChar( ch );
	      nextch();
	      if (ch == 'x' || ch == 'X') {
		nextch();
		base = 16;
		getNumber;
	      } else {
		base = 8;
		getNumber;
	      }
	      return;       // scala-mode: return is a keyword
	    case '1' | '2' | '3' | '4' |
		 '5' | '6' | '7' | '8' | '9' =>
	      base = 10;
	      getNumber;
	      return;
	    case '`' => //"   scala-mode: need to understand literals
	      getStringLit('`');
	      token = IDENTIFIER;
	      return;
	    case '\"' => //"   scala-mode: need to understand literals
	      getStringLit('\"');
	      return;
	    case '\'' =>
	      nextch();
	      ch match {
		case 'A' | 'B' | 'C' | 'D' | 'E' |
		     'F' | 'G' | 'H' | 'I' | 'J' |
		     'K' | 'L' | 'M' | 'N' | 'O' |
		     'P' | 'Q' | 'R' | 'S' | 'T' |
		     'U' | 'V' | 'W' | 'X' | 'Y' |
		     'Z' | '$' | '_' |
		     'a' | 'b' | 'c' | 'd' | 'e' |
		     'f' | 'g' | 'h' | 'i' | 'j' |
		     'k' | 'l' | 'm' | 'n' | 'o' |
		     'p' | 'q' | 'r' | 's' | 't' |
		     'u' | 'v' | 'w' | 'x' | 'y' |
		     'z' =>
                       cbuf.setLength( 0 );
		  putChar( ch );
		  nextch();
		  if (ch != '\'') {
		    getIdentRest;
		    token = SYMBOLLIT;
		    return;
		  }
                case _ if(java.lang.Character.isUnicodeIdentifierStart(ch)) =>
                  cbuf.setLength( 0 );
		  putChar( ch );
		  nextch();
		  if (ch != '\'') {
		    getIdentRest;
		    token = SYMBOLLIT;
		    return;
		  }

		case _ =>
		  getlitch();
		}
	      if (ch == '\'') {
		nextch();
		token = CHARLIT;
                name = Name.fromString( cbuf.toString() );
                cbuf.setLength( 0 );
	      } else {
		syntaxError("unclosed character literal");
	      }
	      return;
	    case '.' =>
	      nextch();
	      if (('0' <= ch) && (ch <= '9')) {
                putChar( '.' );
                getFraction;
              } else token = DOT;
	      return;
	    case ';' =>
	      nextch(); token = SEMI;
	      return;
	    case ',' =>
	      nextch(); token = COMMA;
	      return;
	    case '(' =>   //scala-mode: need to understand character quotes
	      nextch(); token = LPAREN;
	      return;
	    case '{' =>
	      nextch(); token = LBRACE;
	      return;
	    case ')' =>
	      nextch(); token = RPAREN;
	      return;
	    case '}' =>
	      nextch(); token = RBRACE;
	      return;
	    case '[' =>
	      nextch(); token = LBRACKET;
	      return;
	    case ']' =>
	      nextch(); token = RBRACKET;
	      return;
	    case SU =>
              if( !srcIterator.hasNext )
	        token = EOF;
              else
	        syntaxError("illegal character");
	      return;
	    case _ =>

              if( java.lang.Character.getType( ch ).asInstanceOf[Byte] match {
                case java.lang.Character.MATH_SYMBOL => true;
                case java.lang.Character.OTHER_SYMBOL => true;
                case _ => false;}) {
                  putChar( ch );
                  getOperatorRest;
                } else {
	          nextch();
	          syntaxError("illegal character");
                };
	      return;
	  }
      }
    }
  }

  private def skipComment(): boolean = {
    if (ch == '/') {
      do {
        nextch();
      } while ((ch != CR) && (ch != LF) && (ch != SU));
      true
    } else if (ch == '*') {
      docBuffer = null;
      var openComments = 1;
      nextch();
      if (ch == '*') {
        docBuffer = new StringBuffer("/**");
      }
      while (openComments > 0) {
        do {
	  do {
            if (ch == CR) {
              cline = cline + 1;
              ccol = 0;
              nextch(); addCharToDoc(ch);
              if (ch == LF) {
                ccol = 0;
                nextch(); addCharToDoc(ch);
              }
            } else if (ch == LF) {
              cline = cline + 1;
              ccol = 0;
              nextch(); addCharToDoc(ch);
            } else if (ch == '\t') {
              ccol = ((ccol - 1) / tabinc * tabinc) + tabinc;
              nextch(); addCharToDoc(ch);
            } else if (ch == '/') {
              nextch(); addCharToDoc(ch);
              if (ch == '*') {
                nextch(); addCharToDoc(ch);
                openComments = openComments + 1;
              }
            } else if (ch != SU) {
              nextch(); addCharToDoc(ch);
            }
          } while ((ch != '*') && (ch != SU));
          while (ch == '*') {
            nextch(); addCharToDoc(ch);
          }
        } while (ch != '/' && ch != SU);
        if (ch == '/') nextch();
	else syntaxError("unclosed comment");
        openComments = openComments - 1;
      }
      true
    } else {
      false
    }
  }

  def isIdentStart( c:char ) = c.match {
    case 'A' | 'B' | 'C' | 'D' | 'E' |
    'F' | 'G' | 'H' | 'I' | 'J' |
    'K' | 'L' | 'M' | 'N' | 'O' |
    'P' | 'Q' | 'R' | 'S' | 'T' |
    'U' | 'V' | 'W' | 'X' | 'Y' |
    'Z' | '$' | '_' |
    'a' | 'b' | 'c' | 'd' | 'e' |
    'f' | 'g' | 'h' | 'i' | 'j' |
    'k' | 'l' | 'm' | 'n' | 'o' |
    'p' | 'q' | 'r' | 's' | 't' |
    'u' | 'v' | 'w' | 'x' | 'y' |
    'z'  =>
      true
    case _ if(java.lang.Character.isUnicodeIdentifierStart(c)) =>
      true;
    case _ => false;
  }

  def isIdentPart( c:char ) = isIdentStart( c ) || c.match {
    case '0' | '1' | '2' | '3' | '4' |
         '5' | '6' | '7' | '8' | '9' =>
           true
    case _ if(java.lang.Character.isUnicodeIdentifierPart(c)) =>
      true;
    case _ => false } ;


  private def getIdentRest: unit = {
    while (true) {
      ch match {
        case 'A' | 'B' | 'C' | 'D' | 'E' |
	     'F' | 'G' | 'H' | 'I' | 'J' |
	     'K' | 'L' | 'M' | 'N' | 'O' |
	     'P' | 'Q' | 'R' | 'S' | 'T' |
	     'U' | 'V' | 'W' | 'X' | 'Y' |
	     'Z' | '$' |
	     'a' | 'b' | 'c' | 'd' | 'e' |
	     'f' | 'g' | 'h' | 'i' | 'j' |
	     'k' | 'l' | 'm' | 'n' | 'o' |
	     'p' | 'q' | 'r' | 's' | 't' |
	     'u' | 'v' | 'w' | 'x' | 'y' |
	     'z' |
	     '0' | '1' | '2' | '3' | '4' |
	     '5' | '6' | '7' | '8' | '9' =>
          putChar( ch );
          nextch();
        case '_' =>
          putChar( ch );
          nextch();
          getIdentOrOperatorRest;
          return;
        case _ if(java.lang.Character.isUnicodeIdentifierPart(ch)) =>
          putChar( ch );
          nextch();
        case _ =>
          treatIdent;
	  return;
      }
    }
  }

  private def getOperatorRest: unit = {
    while (true) {
      ch match {
        case '~' | '!' | '@' | '#' | '%' |
	     '^' | '*' | '+' | '-' | '<' |
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' =>
          putChar( ch );
	  nextch();
        case '/' =>
          nextch();
          if (skipComment()) {
            treatIdent;
            return;
          } else {
            putChar( '/' );
          }

        case _ =>
          if( java.lang.Character.getType( ch ).asInstanceOf[byte] match {
            case java.lang.Character.MATH_SYMBOL => true;
            case java.lang.Character.OTHER_SYMBOL => true;
            case _ => false;
          }) {
            putChar( ch );
            nextch();
          } else {
            treatIdent;
            return;
          }
      }
    }
  }

  private def getIdentOrOperatorRest: unit = {
    if( isIdentPart( ch ) )
      getIdentRest
    else ch match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' | '/' =>
        getOperatorRest;
      case _ =>
         if( java.lang.Character.getType( ch ).asInstanceOf[byte] match {
            case java.lang.Character.MATH_SYMBOL => true;
            case java.lang.Character.OTHER_SYMBOL => true;
            case _ => false;
          }) {
            getOperatorRest;
          } else {
            treatIdent;
          }
    }
  }

  private def getStringLit(delimiter: char): unit = {
    nextch();
    while (srcIterator.hasNext && ch != delimiter && ch != CR && ch != LF ) {
      getlitch();
    }
    if (ch == delimiter) {
      token = STRINGLIT;
      name = Name.fromString( cbuf.toString() );
      cbuf.setLength( 0 );;
      nextch();
    } else {
      syntaxError("unclosed string literal");
    }
  }

  def treatIdent = {
    name = Name.fromString( cbuf.toString() );
    token = name2token( name );
    cbuf.setLength( 0 );;
  }

  /** generate an error at the given position
  */
  def syntaxError(pos: int, msg: String) = {
    unit.error(pos, msg);
    token = ERROR;
    errpos = pos;
  }

  /** generate an error at the current token position
  */
  def syntaxError(msg: String): unit = syntaxError(pos, msg);

  /** append Unicode character to "lit" buffer
  */
  private def putChar(c: char) = cbuf.append( c );
    /*
    if (litlen == lit.length) {
      val newlit = new Array[char](lit.length * 2);
      System.arraycopy(lit, 0, newlit, 0, lit.length);
      lit = newlit;
    }
    lit(litlen) = c;
    litlen = litlen + 1;
  }*/

  /** return true iff next 6 characters are a valid unicode sequence:
  protected def isUnicode() =
    (bp + 6) < buf.length &&
    (buf(bp) == '\\') &&
    (buf(bp+1) == 'u') &&
    (SourceRepresentation.digit2int(buf(bp+2), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+3), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+4), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+5), 16) >= 0);
  */

  /** precondition: isUnicode() == true
  protected def getUnicodeChar():char = {
    var i : int = 0;
    var k = bp + 2;
    while( k < bp + 6 ) {
      i = 16 * i +  SourceRepresentation.digit2int(buf( k ), 16);
      k = k + 1;
    };
    i.asInstanceOf[char]
  }
*/
  /** read next character in character or string literal:
  */
  protected def getlitch() =
    if (ch == '\\') {
        /*
      if (isUnicode()) {
        nextch();
        nextch();
        var code: int = 0;
        code = (code << 4) + SourceRepresentation.digit2int(ch, 16); nextch();
        code = (code << 4) + SourceRepresentation.digit2int(ch, 16); nextch();
        code = (code << 4) + SourceRepresentation.digit2int(ch, 16); nextch();
        code = (code << 4) + SourceRepresentation.digit2int(ch, 16); nextch();
        putChar(code.asInstanceOf[char]);
      } else {
        */
        nextch();
        if ('0' <= ch && ch <= '7') {
          val leadch: char = ch;
          var oct: int = SourceRepresentation.digit2int(ch, 8);
          nextch();
          if ('0' <= ch && ch <= '7') {
            oct = oct * 8 + SourceRepresentation.digit2int(ch, 8);
            nextch();
            if (leadch <= '3' && '0' <= ch && ch <= '7') {
              oct = oct * 8 + SourceRepresentation.digit2int(ch, 8);
              nextch();
            }
          }
          putChar(oct.asInstanceOf[char]);
        } else if (ch != SU) {
          ch match {
            case 'b'  => putChar('\b')
            case 't'  => putChar('\t')
            case 'n'  => putChar('\n')
            case 'f'  => putChar('\f')
            case 'r'  => putChar('\r')
            case '\"' => putChar('\"')
            case '\'' => putChar('\'')
            case '\\' => putChar('\\')
            case _    =>
              syntaxError(Position.encode(cline, ccol) - 1, "invalid escape character");
              putChar(ch);
          }
          nextch();
        }
      /* } */
    } else  {
      putChar(ch);
      nextch();
    }

  /** read fractional part of floating point number;
  */
  protected def getFraction = {
    while (SourceRepresentation.digit2int(ch, 10) >= 0) {
      putChar( ch );
      nextch();
    }
    token = DOUBLELIT;
    if ((ch == 'e') || (ch == 'E')) {
      putChar( ch );
      val c1 = srcIterator.lookahead1;
      val c2 = srcIterator.lookahead2;
      if ((c1 == '+') || (c1 == '-')  && ('0' >= c2) || (c2 <= '9')) {
        nextch();
        putChar( ch );
        nextch();
      } else
        nextch();
      while (SourceRepresentation.digit2int(ch, 10) >= 0) {
        putChar( ch );
        nextch();
      }
    }
    if ((ch == 'd') || (ch == 'D')) {
      putChar( ch );
      nextch();
    } else if ((ch == 'f') || (ch == 'F')) {
      putChar( ch );
      token = FLOATLIT;
      nextch();
    }
    name = Name.fromString( cbuf.toString() );
    cbuf.setLength( 0 );
  }

  /** convert name, base to long value
  *  base = the base of the number; one of 8, 10, 16.
  */
  def intVal(negated: boolean): long = {
    import SourceRepresentation.digit2int;

    if (token == CHARLIT && !negated) {
      if (name.length() > 0)
        name.charAt( 0 )
      else
	0
    } else {

      var value: long = 0;
      val divider = if (base == 10) 1 else 2;
      val limit: long = if (token == LONGLIT) Long.MAX_VALUE else Integer.MAX_VALUE;
      var i = 0;
      val len = name.length();
      while (i < len) {
	val d = digit2int( name.charAt(i), base );
	if (d < 0) {
          syntaxError("malformed integer number");
          return 0;
	}
	if (value < 0 ||
            limit / (base / divider) < value ||
            limit - (d / divider) < value * (base / divider) &&
	    !(negated && limit == value * base - 1 + d)) {
              syntaxError("integer number too large");
              return 0;
	    }
	value = value * base + d;
	i = i + 1;
      }
      if (negated) -value else value
    }
  }

  def intVal: long = intVal(false);

  /** convert name, base to double value
  */
  def floatVal(negated: boolean): double = {
    val limit: double =
      if (token == DOUBLELIT) Double.MAX_VALUE else Float.MAX_VALUE;
    try {
      val value = Double.valueOf(name.toString()).doubleValue();
      if (value > limit)
        syntaxError("floating point number too large");
      if (negated) -value else value
    } catch {
      case _: NumberFormatException =>
	syntaxError("malformed floating point number");
	0.0
    }
  }

  def floatVal: double = floatVal(false);


  /** see Java spec 3.10.2 */
  def exponentPart(c1:char,c2:char) =
    (c1 == 'e' || c1 == 'E') &&
  ((c2 >= '0' && c2 <= '9') || (c2 == '+' || c2 == '-')) ;

  /** see Java spec 3.10.2 */
  def floatTypeSuffix(c1:char) =
    (c1 == 'f' || c1 == 'F' ||
     c1 == 'd' || c1 == 'D');

  /** read a number into name and set base
  */
  protected def getNumber:unit = {
    import SourceRepresentation.digit2int;
    while (digit2int(ch, if (base == 8) 10 else base) >= 0) {
      putChar( ch );
      nextch();
    }
    if (base <= 10 && ch == '.') { // '.' c1 c2
      val c1 = srcIterator.lookahead1;
      if (c1 >= '0' && c1 <= '9') {
        putChar(ch);
        nextch();
        getFraction;
      } else {
        val c2 = srcIterator.lookahead2;
        if(exponentPart(c1,c2)) {
          putChar(ch);// +
          getFraction;
          return;
        }
        if(isIdentStart(c1)&&isIdentPart(c2)) { // is select
	  name = Name.fromString( cbuf.toString() );
          cbuf.setLength( 0 );
          token = INTLIT;
          return;
        } else {
          putChar(ch);
          nextch();
          getFraction;
        }
      }
    } else if (base <= 10 &&
               (ch == 'e' || ch == 'E' ||
                ch == 'f' || ch == 'F' ||
                ch == 'd' || ch == 'D')) {
      getFraction;
    } else {
      if (ch == 'l' || ch == 'L') {
	name = Name.fromString( cbuf.toString());
        cbuf.setLength( 0 );
        nextch();
        token = LONGLIT;
      } else {
	name = Name.fromString( cbuf.toString() );
        cbuf.setLength( 0 );
        token = INTLIT;
      }
    }
  }

  // start XML tokenizing.
  // prod. [i] refers to productions in http://www.w3.org/TR/REC-xml

  def xSyntaxError(s:String) = {
    syntaxError("in XML literal: "+s);
    xNext;
  }

  var xScalaBlock = false;

  /** read the next character. do not skip whitespace.
  *   treat CR LF as single LF. update ccol and cline
  *
  *   @todo: may XML contain SU, in CDATA sections ?
  */
  def xNext = {
    lastpos = pos;
    ch = srcIterator.raw; ccol = ccol + 1; // = nextch() without unicode
    ch match {
      case SU =>
	syntaxError( lastpos, "unclosed XML literal" );
	token = EOF;
      case LF =>
        ccol = 0; cline = cline + 1;
      case CR =>
        if( LF == srcIterator.lookahead1 ) {
	  srcIterator.raw; ccol = 0; cline = cline + 1;
	}
      case _ =>
        //Console.print(ch.asInstanceOf[char]); // DEBUG
    }
    pos = Position.encode(cline, ccol);
  }

  /** scan [S] '=' [S]
  */
  def xEQ = {
    xSpaceOpt; xToken('='); xSpaceOpt
  }

  final val LT   = Name.fromString("<");

  def xStartsXML = {
    unit.global.xmlMarkup && ( token == IDENTIFIER )&&( name == LT );
  }

  def xIsSpace = ch match {
    case ' ' | '\t' | CR | LF => true
    case _ => false;
  }

  /** skip optional space S?
  */
  def xSpaceOpt = {
    while( xIsSpace ) { xNext; }
  }

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+
  */
  def xSpace = {
    if( xIsSpace ) {
      xNext; xSpaceOpt
    } else {
      xSyntaxError("whitespace expected");
    }
  }

  /** NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *             | CombiningChar | Extender
   *
   * see [4] and Appendix B of XML 1.0 specification
  */
  def xIsNameChar = xIsNameStart || (ch match {
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

  /** NameStart == Unicode general category in { Ll, Lu, Lo, Lt, Nl }
   *
   *  We do not allow a name to start with ':'.
   *  see [3] and Appendix B of XML 1.0 specification
   */
  def xIsNameStart =
    java.lang.Character.getType( ch ).asInstanceOf[Byte] match {
      case  java.lang.Character.LOWERCASE_LETTER => true;
      case  java.lang.Character.UPPERCASE_LETTER => true;
      case  java.lang.Character.OTHER_LETTER     => true;
      case  java.lang.Character.TITLECASE_LETTER => true;
      case  java.lang.Character.LETTER_NUMBER    => true;
      case _ => ch match {
        case '_' => true
        case _ => false;
      }
    }

  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def xName:Name = {
    if( xIsNameStart ) {
      do {
        putChar( ch );
        xNext;
      } while( xIsNameChar );
      val n = Name.fromString( cbuf.toString() );
      cbuf.setLength( 0 );
      n
    } else {
      xSyntaxError( "name expected" );
      Names.EMPTY
    }
  }


  /** returns string up to next character endch.
   *
   *  @param endch the character to which we skip
   */
  def xSkipToNext( endch:char ):String = {
    lastpos = pos;
    while (( ch != endch )&&( ch != '{' )) {
      putChar( ch );
      xNext;
    };
    val s = cbuf.toString();
    cbuf.setLength( 0 );
    s
  }

  /** attribute value, terminated by either ' or ". value may not contain <.
   *  @param endch either ' or "
   */
  def xAttributeValue( endch:char ):String = {
    val s = xSkipToNext( endch );
    // well-formedness constraint
    if( s.indexOf('<') != -1 ) {
      xSyntaxError( "'<' not allowed in attrib value" ); ""
    } else {
      s
    }
  }

  /* move forward one char
   *
   * @return true if next character  starts a scala block
   */
  def xxNext:boolean = {
    xNext;
    xCheckScalaBlock
  }

  /* checks whether next character starts a Scala block, if yes, skip it.
   *
   * @return true if next character starts a scala block
   */
  def xCheckScalaBlock:boolean = {
    xScalaBlock = ( ch == '{' ) && { xNext;( ch != '{' ) };
    return xScalaBlock;
  }

  /** parse character data.
  *   precondition: xScalaBlock == false (we are not in a scala block)
  */
  def xText:String = {
    if( xScalaBlock ) throw new ApplicationError(); // assert

    if( xCheckScalaBlock )
      return ""
    else {
      lastpos = pos;
      var exit = false;
      while( !exit ) {
        putChar( ch );
        exit = xxNext || ( ch == '<' ) || ( ch == '&' );
      }
      val s = cbuf.toString();
      cbuf.setLength( 0 );
      s
    }
  }


  /** CharRef ::= "&#" '0'..'9' {'0'..'9'} ";"
   *            | "&#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   *
   * see [66]
   */
  def xCharRef:String = {
    val hex  = ( ch == 'x' ) && { xNext; true };
    val base = if (hex) 16 else 10;
    var i = 0;
    while( ch != ';' ) {
      ch match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + Character.digit( ch, base );
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if( !hex )
            xSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + Character.digit( ch, base );
        case _ =>
          xSyntaxError("character '"+ch+" not allowed in char ref\n");
      }
      xNext;
    }
    new String( Predef.Array[char]( i.asInstanceOf[char] ))
  }

  /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * see [15]
   */
  def xComment:String = {
    val sb:StringBuffer = new StringBuffer();
    xToken('!');
    xToken('-');
    xToken('-');
    while (true) {
      xNext;
      if( ch=='-'  && { sb.append( ch ); xNext; ch == '-' } ) {
        sb.setLength( sb.length() - 1 );
        xNext;
        xToken('>');
        return sb.toString();
      } else sb.append( ch );
    }
    return ""; // this cannot happen;
  };

  /** munch expected XML token, report syntax error for unexpected
  */
  def xToken(that:char):unit = {
    if( ch == that )
      xNext;
    else
      xSyntaxError("'" + that + "' expected instead of '" + ch + "'");
  }

  // end XML tokenizing


  override def toString() = token match {
    case IDENTIFIER =>
      "id(" + name + ")"
    case CHARLIT =>
      "char(" + intVal + ")"
    case INTLIT =>
      "int(" + intVal + ")"
    case LONGLIT =>
      "long(" + intVal + ")"
    case FLOATLIT =>
      "float(" + floatVal + ")"
    case DOUBLELIT =>
      "double(" + floatVal + ")"
    case STRINGLIT =>
      "string(" + name + ")"
    case SEMI =>
      ";"
    case COMMA =>
      ","
    case _ =>
      token2string(token)
  }
}
}
