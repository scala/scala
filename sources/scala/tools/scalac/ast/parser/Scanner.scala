/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

import scalac._;
import scalac.util.Name;
import scalac.ast.parser.SourceRepresentation;

package scala.tools.scalac.ast.parser {

import ch.epfl.lamp.util.Position;
import ch.epfl.lamp.util.SourceFile;


/** A scanner for the programming language Scala.
 *
 *  @author     Matthias Zenger, Martin Odersky
 *  @version    1.0
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

  /** the names of all tokens
  */
  var tokenName = new Array[Name](128);
  var numToken = 0;

    /** keyword array; maps from name indices to tokens
     */
  var key: Array[byte] = _;
  var maxKey = 0;

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
  var buf: Array[byte] = unit.source.bytes();
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
  init();
  nextToken();

  def nextch(): unit = {
    bp = bp + 1; ch = buf(bp); ccol = ccol + 1;
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
      //System.out.println("<" + token2string(token) + ">");//DEBUG
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
		getNumber(index + 2, 16);
	      } else {
		getNumber(index, 8);
	      }
	      return;       // scala-mode: return is a keyword
	    case '1' | '2' | '3' | '4' |
		 '5' | '6' | '7' | '8' | '9' =>
	      getNumber(index, 10);
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
		  putch(ch);
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
		val ascii = new Array[byte](litlen * 2);
		val alen = SourceRepresentation.source2ascii(lit, 0, litlen, ascii);
		if (alen > 0)
		  intVal = SourceRepresentation.ascii2string(ascii, 0, alen).charAt(0);
		else
		  intVal = 0;
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
      name = Name.fromSource(lit, 0, litlen);
      nextch();
    } else {
      syntaxError("unclosed character literal");
    }
  }

  /** returns true if argument corresponds to a keyword.
  *  Used in dtd2scala tool.
  */
  def isKeyword(str: String) = Name.fromString(str).index <= maxKey;

  def treatIdent(start: int, end: int) = {
    name = Name.fromAscii(buf, start, end - start);
    token = if (name.index <= maxKey) key(name.index) else IDENTIFIER;
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

  /** append characteter to "lit" buffer
  */
  protected def putch(c: byte) = {
    if (litlen == lit.length) {
      val newlit = new Array[byte](lit.length * 2);
      System.arraycopy(lit, 0, newlit, 0, lit.length);
      lit = newlit;
    }
    lit(litlen) = c;
    litlen = litlen + 1;
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
        putch(ch); nextch();
        putch(ch); nextch();
        putch(ch); nextch();
        putch(ch); nextch();
        putch(ch); nextch();
        putch(ch); nextch();
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
          putch(oct.asInstanceOf[byte]);
        } else if (ch != SU) {
          ch match {
            case 'b' | 't' | 'n' | 'f' | 'r' | '\"' /*"*/ | '\'' | '\\' =>
              putch('\\'.asInstanceOf[byte]);
              putch(ch);
            case _ =>
              syntaxError(Position.encode(cline, ccol) - 1, "invalid escape character");
              putch(ch);
          }
          nextch();
        }
      }
    } else if (ch != SU) {
      putch(ch);
      nextch();
    }

  /** read fractional part of floating point number;
  *  Then floatVal := buf[index..], converted to a floating point number.
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
    var limit = Double.MAX_VALUE;
    if ((ch == 'd') || (ch == 'D')) {
      nextch();
    } else if ((ch == 'f') || (ch == 'F')) {
      token = FLOATLIT;
      limit = Float.MAX_VALUE;
      nextch();
    }
    try {
      floatVal = Double.valueOf(new String(buf, index, bp - index)).doubleValue();
      if (floatVal > limit)
        syntaxError("floating point number too large");
    } catch {
      case _: NumberFormatException =>
	syntaxError("malformed floating point number");
    }
  }

  /** intVal := buf(index..index+len-1), converted to an integer number.
  *  base = the base of the number; one of 8, 10, 16.
  *  max  = the maximal number before an overflow.
  */
  protected def makeInt (index: int, len: int, base: int, max: long): unit = {
    intVal = 0;
    val divider = if (base == 10) 1 else 2;
    var i = 0;
    while (i < len) {
      val d = SourceRepresentation.digit2int(buf(index + i), base);
      if (d < 0) {
        syntaxError("malformed integer number");
        return;
      }
      if (intVal < 0 ||
          max / (base / divider) < intVal ||
          max - (d / divider) < (intVal * (base / divider) - 0)) {
        syntaxError("integer number too large");
        return;
      }
      intVal = intVal * base + d;
      i = i + 1;
    }
  }

  /** read a number,
  *  and convert buf[index..], setting either intVal or floatVal.
  *  base = the base of the number; one of 8, 10, 16.
  */
  protected def getNumber(index: int, base: int) = {
    while (SourceRepresentation.digit2int(ch, if (base == 8) 10 else base) >= 0) {
      nextch();
    }
    if (base <= 10 && ch == '.') {
      nextch();
      if ((ch >= '0') && (ch <= '9'))
        getFraction(index);
      else {
	bp = bp - 1;
        ch = buf(bp);
	ccol = ccol - 1;
        makeInt(index, bp - index, base, Integer.MAX_VALUE);
        intVal = intVal.asInstanceOf[int];
        token = INTLIT;
      }
    } else if (base <= 10 &&
               (ch == 'e' || ch == 'E' ||
		ch == 'f' || ch == 'F' ||
		ch == 'd' || ch == 'D')) {
      getFraction(index);
    } else {
      if (ch == 'l' || ch == 'L') {
        makeInt(index, bp - index, base, Long.MAX_VALUE);
        nextch();
        token = LONGLIT;
      } else {
        makeInt(index, bp - index, base, Integer.MAX_VALUE);
        intVal = intVal.asInstanceOf[int];
        token = INTLIT;
      }
    }
  }

  /*       X                                L
  */

  /* methods for XML tokenizing, see XML 1.0 rec, available from www.w3.org/xml  */

  /*                       M
  */

  def xml_syntaxError(s:String) = {
    syntaxError("in XML literal: "+s);
    xml_nextch();
  }

  /* this helper functions updates ccol and cline, only necessary in whitespace
  *  production
  */
  def xml_nextch() = {
    nextch();
    ch match {
      case '\r' => {
        cline = cline + 1;
        ccol = 0;
        nextch(); /* in compliance with XML spec */
        if( ch == '\n' ) {
          ccol = 0;
        }
      }
      case '\n' => {
        cline = cline + 1;
        ccol = 0;
      }
      case _ =>
    }
  }

  def xml_isSpace() = ch match {
    case ' ' | '\t' | '\r' | '\n' => true
    case _ => false;
  }

  def xmlSpaceOpt() = {
    while( xml_isSpace() ) {
      xml_nextch();
    }
    pos = Position.encode(cline, ccol);
  }

  /** [3] S ::= (#x20 | #x9 | #xD | #xA)+
  */
  def xmlSpace() = {
    if( xml_isSpace() ) {
      xml_nextch();
      xmlSpaceOpt()
    } else {
      pos = Position.encode(cline, ccol);
      xml_syntaxError("whitespace expected");
    }
  }

  /** a subset of
   *  [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
   *
   * todo: add unicode letters and digits as well as combining chars and extenders
  **/
  def xml_isNameChar() = ch match {
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
    'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
    'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' |
    'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' |
    'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
    '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
    '.' | '-' | '_' | ':'  => true;
    case _ => false
  }

  def xml_isNameStart() = ch match {
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
    'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
    'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' | 'a' | 'b' | 'c' | 'd' | 'e' |
    'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' |
    'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
    '_' | ':'  => true;
    case _ => false
  }
  /** a subset of (see isNameChar() )
  *   [5] Name ::= (Letter | '_' | ':') (NameChar)*
  */
  def xmlName():Name = {
    if( xml_isNameStart() ) {
      val index = bp;
      while( xml_isNameChar() ) {
        nextch();
      }
      pos = Position.encode(cline, ccol);
      Name.fromAscii(buf, index, bp - index);
    } else {
      xml_syntaxError("name expected");
      Name.fromString("-error-");
    }
  }

  def xmlValue(endch:char):String = {
    pos = Position.encode(cline, ccol);
    val index = bp;
    while ( ch != endch ) {
      if(( ch == '<' )||( ch == '&' ))
        syntaxError(ch.asInstanceOf[char]+" not allowed here");
      xml_nextch();
    };
    pos = Position.encode(cline, ccol);
    new String(buf, index, bp-index);
  }

  def xmlText():String = xmlValue('<');

  def xmlToken(that:char):unit = {
    if( ch == that ) {
      xml_nextch();
      pos = Position.encode(cline, ccol);
    } else {
      pos = Position.encode(cline, ccol);
      xml_syntaxError("'"+that+"' expected instead of '"+ch.asInstanceOf[char]+"'");
    }
  }
  /* end XML tokenizing */

  def name2token(name: Name): int =
    if (name.index <= maxKey) key(name.index) else IDENTIFIER;

  def token2string(token: int): String = token match {
    case IDENTIFIER =>
      "identifier"/* + \""+name+"\""*/
    case CHARLIT =>
      "character literal"
    case INTLIT =>
      "integer literal"
    case LONGLIT =>
      "long literal"
    case FLOATLIT =>
      "float literal"
    case DOUBLELIT =>
      "double literal"
    case STRINGLIT =>
      "string literal"
    case SYMBOLLIT =>
      "symbol literal"
    case LPAREN =>
      "'('"
    case RPAREN =>
      "')'"
    case LBRACE =>
      "'{'"
    case RBRACE =>
      "'}'"
    case LBRACKET =>
      "'['"
    case RBRACKET =>
      "']'"
    case EOF =>
      "eof"
    case ERROR =>
      "something"
    case SEMI =>
      "';'"
    case COMMA =>
      "','"
    case CASECLASS =>
      "case class"
    case CASEOBJECT =>
      "case object"
    case _ =>
      try {
        "'" + tokenName(token).toString() + "'"
      } catch {
	case _: ArrayIndexOutOfBoundsException =>
          "'<" + token + ">'"
	case _: NullPointerException =>
          "'<(" + token + ")>'"
      }
  }

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

  protected def enterKeyword(s: String, tokenId: int) = {
    while (tokenId > tokenName.length) {
      val newTokName = new Array[Name](tokenName.length * 2);
      System.arraycopy(tokenName, 0, newTokName, 0, newTokName.length);
      tokenName = newTokName;
    }
    val n = Name.fromString(s);
    tokenName(tokenId) = n;
    if (n.index > maxKey) maxKey = n.index;
    if (tokenId >= numToken) numToken = tokenId + 1;
  }

  protected def init() = {
    initKeywords();
    key = new Array[byte](maxKey+1);
    for (val i <- Iterator.range(0, maxKey))
      key(i) = IDENTIFIER;
    for (val j <- Iterator.range(0, numToken))
      if (tokenName(j) != null) key(tokenName(j).index) = j.asInstanceOf[byte];
  }

  protected def initKeywords() = {
    enterKeyword("abstract", ABSTRACT);
    enterKeyword("case", CASE);
    enterKeyword("class", CLASS);
    enterKeyword("catch", CATCH);
    enterKeyword("def", DEF);
    enterKeyword("do", DO);
    enterKeyword("else", ELSE);
    enterKeyword("extends", EXTENDS);
    enterKeyword("false", FALSE);
    enterKeyword("final", FINAL);
    enterKeyword("finally", FINALLY);
    enterKeyword("for", FOR);
    enterKeyword("if", IF);
    enterKeyword("import", IMPORT);
    enterKeyword("new", NEW);
    enterKeyword("null", NULL);
    enterKeyword("object", OBJECT);
    enterKeyword("override", OVERRIDE);
    enterKeyword("package", PACKAGE);
    enterKeyword("private", PRIVATE);
    enterKeyword("protected", PROTECTED);
    enterKeyword("return", RETURN);
    enterKeyword("sealed", SEALED);
    enterKeyword("super", SUPER);
    enterKeyword("this", THIS);
    enterKeyword("throw", THROW);
    enterKeyword("trait", TRAIT);
    enterKeyword("true", TRUE);
    enterKeyword("try", TRY);
    enterKeyword("type", TYPE);
    enterKeyword("val", VAL);
    enterKeyword("var", VAR);
    enterKeyword("with", WITH);
    enterKeyword("while", WHILE);
    enterKeyword("yield", YIELD);
    enterKeyword(".", DOT);
    enterKeyword("_", USCORE);
    enterKeyword(":", COLON);
    enterKeyword("=", EQUALS);
    enterKeyword("=>", ARROW);
    enterKeyword("<-", LARROW);
    enterKeyword("<:", SUBTYPE);
    enterKeyword(">:", SUPERTYPE);
    enterKeyword("#", HASH);
    enterKeyword("@", AT);
  }
}
}
