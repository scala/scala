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

  /** buffer for the documentation comment
  */
  var docBuffer: StringBuffer = null;

  /** add the given character to the documentation buffer
  */
  protected def addCharToDoc(ch: byte): unit =
      if (docBuffer != null)
        docBuffer.append(ch.asInstanceOf[char]);

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
  */
  var buf: Array[byte] = unit.source.getContent();
  var bp: int = -1;

  /** the current character
  */
  var ch: byte = _;

  /** the line and column position of the current character
  */
  var cline: int = 1;
  var ccol: int = 0;

  /** a buffer for character and string literals
  */
  var lit = new Array[byte](64);
  var litlen: int = _;


  /** INIT: Construct a scanner from a file input stream.
  */
  token = EMPTY;
  nextch();
  Tokens; // initialize tokens
  nextToken();

  def nextch(): unit = {
    bp = bp + 1; ch = buf(bp); ccol = ccol + 1;
    //System.out.print(bp + "[" + (ch.asInstanceOf[char]) + "]");//DEBUG
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
      //System.out.println("<" + token2string(token) + ":" + name + ">");//DEBUG
    }
  }

  /** read next token
  */
  private def fetchToken(): unit = {
    if (token == EOF) return;
    lastpos = Position.encode(cline, ccol);
    var index = bp;
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
	  index = bp;
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
		 'u' | 'v' | 'w' | 'x' | 'y' |  // scala-mode: need to understand multi-line case patterns
		 'z' =>
	      nextch();
	      getIdentRest(index);  // scala-mode: wrong indent for multi-line case blocks
	      return;
	    case '~' | '!' | '@' | '#' | '%' |
		 '^' | '*' | '+' | '-' | '<' |
		 '>' | '?' | ':' | '=' | '&' |
                 '|' | '\\' =>
	      nextch();
	      getOperatorRest(index);
	      return;
	    case '/' =>
	      nextch();
	      if (!skipComment()) {
		getOperatorRest(index);
		return;
	      }
	    case '0' =>
	      nextch();
	      if (ch == 'x' || ch == 'X') {
		nextch();
		base = 16;
		getNumber(index + 2);
	      } else {
		base = 8;
		getNumber(index);
	      }
	      return;       // scala-mode: return is a keyword
	    case '1' | '2' | '3' | '4' |
		 '5' | '6' | '7' | '8' | '9' =>
	      base = 10;
	      getNumber(index);
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
	      litlen = 0;
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
		  index = bp;
		  putAscii(ch);
		  nextch();
		  if (ch != '\'') {
		    getIdentRest(index);
		    token = SYMBOLLIT;
		    return;
		  }
		case _ =>
		  getlitch();
		}
	      if (ch == '\'') {
		nextch();
		token = CHARLIT;
	      } else {
		syntaxError("unclosed character literal");
	      }
	      return;
	    case '.' =>
	      nextch();
	      if (('0' <= ch) && (ch <= '9')) getFraction(index);
	      else token = DOT;
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
	      token = EOF;
	      return;
	    case _ =>
	      nextch();
	      syntaxError("illegal character");
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

  private def getIdentRest(index: int): unit = {
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
          nextch();
        case '_' =>
          nextch();
          getIdentOrOperatorRest(index);
	  return;
        case _ =>
          treatIdent(index, bp);
	  return;
      }
    }
  }

  private def getOperatorRest(index: int): unit = {
    while (true) {
      ch match {
        case '~' | '!' | '@' | '#' | '%' |
	     '^' | '*' | '+' | '-' | '<' |
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' =>
	  nextch();
        case '/' =>
          val lastbp = bp;
          nextch();
          if (skipComment()) {
            treatIdent(index, lastbp);
            return;
          }
        case _ =>
          treatIdent(index, bp);
          return;
      }
    }
  }

  private def getIdentOrOperatorRest(index: int): unit = {
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
	   'z' |
	   '0' | '1' | '2' | '3' | '4' |
	   '5' | '6' | '7' | '8' | '9' =>
        getIdentRest(index);
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' | '/' =>
        getOperatorRest(index);
      case _ =>
        treatIdent(index, bp);
    }
  }

  private def getStringLit(delimiter: char): unit = {
    nextch();
    litlen = 0;
    while (ch != delimiter && ch != CR && ch != LF && ch != SU)
      getlitch();
    if (ch == delimiter) {
      token = STRINGLIT;
      name = Name.fromAscii(lit, 0, litlen);
      nextch();
    } else {
      syntaxError("unclosed character literal");
    }
  }

  def treatIdent(start: int, end: int) = {
    name = Name.fromAscii(buf, start, end - start);
    token = name2token(name);
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

  /** append ascii character to "lit" buffer
  */
  private def putAscii(c: byte) = {
    if (litlen == lit.length) {
      val newlit = new Array[byte](lit.length * 2);
      System.arraycopy(lit, 0, newlit, 0, lit.length);
      lit = newlit;
    }
    lit(litlen) = c;
    litlen = litlen + 1;
  }
  /** append Unicode character to "lit" buffer
  */
  private def putChar(c: char) = {
    if (c <= 0x7F) {
      putAscii(c.asInstanceOf[byte]);
    } else if (c <= 0x3FF) {
      putAscii((0xC0 | (c >> 6)).asInstanceOf[byte]);
      putAscii((0x80 | (c & 0x3F)).asInstanceOf[byte]);
    } else {
      putAscii((0xE0 | (c >> 12)).asInstanceOf[byte]);
      putAscii((0x80 | ((c >> 6) & 0x3F)).asInstanceOf[byte]);
      putAscii((0x80 | (c & 0x3F)).asInstanceOf[byte]);
    }
  }

  /** return true iff next 6 characters are a valid unicode sequence:
  */
  protected def isUnicode() =
    (bp + 6) < buf.length &&
    (buf(bp) == '\\') &&
    (buf(bp+1) == 'u') &&
    (SourceRepresentation.digit2int(buf(bp+2), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+3), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+4), 16) >= 0) &&
    (SourceRepresentation.digit2int(buf(bp+5), 16) >= 0);

  /** read next character in character or string literal:
  */
  protected def getlitch() =
    if (ch == '\\') {
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
        nextch();
        if ('0' <= ch && ch <= '7') {
          val leadch: byte = ch;
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
              putAscii(ch);
          }
          nextch();
        }
      }
    } else if (ch != SU) {
      putAscii(ch);
      nextch();
    }

  /** read fractional part of floating point number;
  */
  protected def getFraction(index: int) = {
    while (SourceRepresentation.digit2int(ch, 10) >= 0) {
      nextch();
    }
    token = DOUBLELIT;
    if ((ch == 'e') || (ch == 'E')) {
      nextch();
      if ((ch == '+') || (ch == '-')) {
        val sign: byte = ch;
        nextch();
        if (('0' > ch) || (ch > '9')) {
          ch = sign;
          bp = bp - 1;
          ccol = ccol - 1;
        }
      }
      while (SourceRepresentation.digit2int(ch, 10) >= 0) {
        nextch();
      }
    }
    if ((ch == 'd') || (ch == 'D')) {
      nextch();
    } else if ((ch == 'f') || (ch == 'F')) {
      token = FLOATLIT;
      nextch();
    }
    name = Name.fromAscii(buf, index, bp - index);
  }

  /** convert name, base to long value
  *  base = the base of the number; one of 8, 10, 16.
  */
  def intVal(negated: boolean): long = {
    if (token == CHARLIT && !negated) {
      if (litlen > 0)
	SourceRepresentation.ascii2string(lit, 0, litlen).charAt(0)
      else
	0
    } else {
      var value: long = 0;
      val divider = if (base == 10) 1 else 2;
      val limit: long = if (token == LONGLIT) Long.MAX_VALUE else Integer.MAX_VALUE;
      var i = 0;
      val len = name.length();
      while (i < len) {
	val d = SourceRepresentation.digit2int(name.charAt(i).asInstanceOf[byte], base);
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

  /** read a number into name and set base
  */
  protected def getNumber(index: int) = {
    while (SourceRepresentation.digit2int(ch, if (base == 8) 10 else base) >= 0) {
      nextch();
    }
    if (base <= 10 && ch == '.') {
      nextch();
      if (ch >= '0' && ch <= '9')
        getFraction(index);
      else if (((bp + 2) < buf.length) && // guard for character lookahead
               (ch == 'e' || ch == 'E' ||
                ch == 'f' || ch == 'F' ||
                ch == 'd' || ch == 'D')) {
        val ch1 = buf(bp + 1); // lookahead
        if ((ch1 >= 'a' && ch1 <= 'z') ||
            (ch1 >= 'A' && ch1 <= 'Z') ||
            (ch1 >= '0' && ch1 <= '9') ||
             ch1 == '$' || ch1 == '_') {
	  name = Name.fromAscii(buf, index, bp - index);
          token = INTLIT;
        } else
          getFraction(index);
      } else if ((ch >= 'a' && ch <= 'z') ||
                 (ch >= 'A' && ch <= 'Z') ||
                  ch == '$' || ch == '_') {
        bp = bp - 1;
        ch = buf(bp);
	ccol = ccol - 1;
	name = Name.fromAscii(buf, index, bp - index);
        token = INTLIT;
      } else
        getFraction(index);
    } else if (base <= 10 &&
               (ch == 'e' || ch == 'E' ||
                ch == 'f' || ch == 'F' ||
                ch == 'd' || ch == 'D')) {
      getFraction(index);
    } else {
      if (ch == 'l' || ch == 'L') {
	name = Name.fromAscii(buf, index, bp - index);
        nextch();
        token = LONGLIT;
      } else {
	name = Name.fromAscii(buf, index, bp - index);
        token = INTLIT;
      }
    }
  }

  /*       X               M                 L
  */

  /* methods for XML tokenizing, see XML 1.0 rec http://www.w3.org/xml  */

  def xSyntaxError(s:String) = {
    syntaxError("in XML literal: "+s); xNext;
  }

  /** read the next character. do not skip whitespace.
  *   treat CR LF as single LF. update ccol and cline
  *
  *   @todo: may XML contain SU, in CDATA sections ?
  */
  def xNext = {
    lastpos = pos;
    nextch();
    ch match {
      case SU =>
	syntaxError( lastpos, "unclosed XML literal" );
	token = EOF;
      case LF =>
        ccol = 0; cline = cline + 1;
      case CR =>
        nextch();
        if( LF == ch ) {
	  ccol = 0; cline = cline + 1;
	} else {
	  bp = bp - 1; ch = CR
	}
      case _ =>
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

  /** scan [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
   *                      | CombiningChar | Extender
   * partial implementation
   * @todo: add unicode letters, add CombiningChar Extender
  **/
  def xIsNameChar = ch match {
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
    'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
    'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' |
    'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' |
    'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
    '.' | '-' | '_' | ':'  => true;
    case _ => false
  }

  /** scan (Letter | '_' | ':') */
  def xIsNameStart = ch match {
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
    'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
    'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' |
    'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' |
    'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
    '_' | ':'  => true;
    case _ => false
  }
  /** scan [5] Name ::= (Letter | '_' | ':') (NameChar)*
  */
  def xName:Name = {
    if( xIsNameStart ) {
      val index = bp;
      while( xIsNameChar ) { xNext; }
      Name.fromAscii( buf, index, bp - index );
    } else {
      xSyntaxError( "name expected" );
      Names.EMPTY
    }
  }

  /** @see xSkipToNext( char, boolean ) */
  def xSkipToNext( endch:char ):String = xSkipToNext( endch, true );

  /** move forward to next endch. return a string if needed.
   *  @param endch the character to which we skip
   *  @param keep  if true, this function returns the string, otherwise null
   */
  def xSkipToNext( endch:char, keep:boolean ):String = {
    lastpos = pos;
    val index = bp;
    while (( ch != endch )&&( ch != '{' )) { xNext; };
    if( keep )
      new String( buf, index, bp-index );
    else
      null
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

  /** character data. WRONG
  */
  def xText:String = xSkipToNext( '<' );

  /** scan XML comment.
  */
  def xComment = {
    xToken('!');
    xToken('-');
    xToken('-');
    while ( ch != '-' ) { xNext; };
    xToken('-');
    xToken('-');
    xToken('>');
  };

  /** scan an exected XML token
  */
  def xToken(that:char):unit = {
    if( ch == that ) {
      xNext;
    } else {
      xSyntaxError("'"+that+"' expected instead of '"+ch.asInstanceOf[char]+"'");
    }
  }
  /* end XML tokenizing */

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
