/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast.parser;

import Tokens._;
import scala.tools.util.{Position, SourceFile}
import SourceFile.{LF, FF, CR, SU}
import scala.tools.nsc.util.CharArrayReader;

abstract class Scanners: ParserPhase {

  import global._;

  /** A class for representing a token's data. */
  class TokenData {

    /** the next token */
    var token: int = EMPTY;

    /** the token's position */
    var pos: int = 0;

    /** the name of an identifier or token */
    var name: Name = null;

    /** the base of a number */
    var base: int = 0;

    def copyFrom(td: TokenData) = {
      this.token = td.token;
      this.pos = td.pos;
      this.name = td.name;
      this.base = td.base;
    }
  }

  /** A scanner for the programming language Scala.
   *
   *  @author     Matthias Zenger, Martin Odersky, Burak Emir
   *  @version    1.1
   */
  class Scanner(unit: CompilationUnit) extends TokenData {

    import Tokens._;
    import java.lang.{Integer, Long, Float, Double, Character}

    /** Character input reader
     */
    val in = new CharArrayReader(unit.source.getContent(), true, syntaxError);

    /** character buffer for literals
     */
    val cbuf = new StringBuffer();

    /** append Unicode character to "lit" buffer
    */
    protected def putChar(c: char) = cbuf.append(c);

    /** Clear buffer and set name */
    private def setName = {
      name = newTermName(cbuf.toString());
      cbuf.setLength(0)
    }

    /** buffer for the documentation comment
     */
    var docBuffer: StringBuffer = null;

    /** add the given character to the documentation buffer
     */
    protected def putDocChar(c: char): unit =
      if (docBuffer != null) docBuffer.append(c);

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

// Get next token ------------------------------------------------------------

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
               LARROW | SUBTYPE | VIEWBOUND | SUPERTYPE | HASH | AT |
               RPAREN | RBRACKET | RBRACE =>
          case _ =>
            if (token == EOF || Position.line(pos) > Position.line(prevpos)) {
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
        //Console.println("<" + this + ">");//DEBUG
      }
    }

    /** read next token
     */
    private def fetchToken(): unit = {
      if (token == EOF) return;
      lastpos = Position.encode(in.cline, in.ccol);
      //var index = bp;
      while (true) {
        in.ch match {
          case ' ' | '\t' | CR | LF | FF =>
            in.next;
          case _ =>
	    pos = Position.encode(in.cline, in.ccol);
	    in.ch match {
              case '\u21D2' =>
                in.next; token = ARROW;
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
                putChar(in.ch);
	        in.next;
	        getIdentRest;  // scala-mode: wrong indent for multi-line case blocks
	        return;
              case '~' | '!' | '@' | '#' | '%' |
                   '^' | '*' | '+' | '-' | '<' |
                   '>' | '?' | ':' | '=' | '&' |
                   '|' | '\\' =>
                putChar(in.ch);
                in.next;
                getOperatorRest; // XXX
                return;
              case '/' =>
                in.next;
                if (!skipComment()) {
                  putChar('/');
                  getOperatorRest;
                  return;
                }

              case '0' =>
                putChar(in.ch);
                in.next;
                if (in.ch == 'x' || in.ch == 'X') {
                  in.next;
                  base = 16
                } else {
                  base = 8;
                }
		getNumber;
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
                in.next;
                in.ch match {
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
                    putChar(in.ch);
                    in.next;
                    if (in.ch != '\'') {
                      getIdentRest;
                      token = SYMBOLLIT;
                      return;
                    }
                  case _ =>
		    if (Character.isUnicodeIdentifierStart(in.ch)) {
                      putChar(in.ch);
                      in.next;
                      if (in.ch != '\'') {
			getIdentRest;
			token = SYMBOLLIT;
			return;
                      }
		    } else {
                      getlitch()
		    }
                }
                if (in.ch == '\'') {
                  in.next;
                  token = CHARLIT;
                  setName
                } else {
                  syntaxError("unclosed character literal");
                }
                return;
              case '.' =>
		in.next;
		if ('0' <= in.ch && in.ch <= '9') {
		  putChar('.'); getFraction;
		} else {
		  token = DOT
		}
                return;
              case ';' =>
                in.next; token = SEMI;
                return;
              case ',' =>
                in.next; token = COMMA;
                return;
              case '(' =>   //scala-mode: need to understand character quotes
                in.next; token = LPAREN;
                return;
              case '{' =>
                in.next; token = LBRACE;
                return;
              case ')' =>
                in.next; token = RPAREN;
                return;
              case '}' =>
                in.next; token = RBRACE;
                return;
              case '[' =>
                in.next; token = LBRACKET;
                return;
              case ']' =>
                in.next; token = RBRACKET;
                return;
              case SU =>
                if (!in.hasNext) token = EOF;
                else syntaxError("illegal character");
                return;
              case _ =>
		if (Character.isUnicodeIdentifierStart(in.ch)) {
                  putChar(in.ch);
		  in.next;
                  getIdentRest;
		} else if (isSpecial(in.ch)) {
                  putChar(in.ch);
                  getOperatorRest;
                } else {
                  syntaxError("illegal character");
                  in.next;
                }
                return;
	    }
        }
      }
    }

    private def skipComment(): boolean =
      if (in.ch == '/') {
        do {
          in.next;
        } while ((in.ch != CR) && (in.ch != LF) && (in.ch != SU));
        true
      } else if (in.ch == '*') {
        docBuffer = null;
        var openComments = 1;
        in.next;
        if (in.ch == '*') docBuffer = new StringBuffer("/**");
        while (openComments > 0) {
          do {
	    do {
              if (in.ch == '/') {
                in.next; putDocChar(in.ch);
                if (in.ch == '*') {
                  in.next; putDocChar(in.ch);
                  openComments = openComments + 1;
                }
              }
              in.next; putDocChar(in.ch);
            } while (in.ch != '*' && in.ch != SU);
            while (in.ch == '*') {
              in.next; putDocChar(in.ch);
            }
          } while (in.ch != '/' && in.ch != SU);
          if (in.ch == '/') in.next;
	  else syntaxError("unclosed comment");
          openComments = openComments - 1;
        }
        true
      } else {
        false
      }

// Identifiers ---------------------------------------------------------------

    def isIdentStart(c: char): boolean =
      ('A' <= c && c <= 'Z') ||
      ('a' <= c && c <= 'a') ||
      (c == '_') || (c == '$') ||
      Character.isUnicodeIdentifierStart(c);

    def isIdentPart(c: char) =
      isIdentStart(c) ||
      ('0' <= c && c <= '9') ||
      Character.isUnicodeIdentifierPart(c);

    def isSpecial(c: char) = {
      val chtp = Character.getType(c);
      chtp == Character.MATH_SYMBOL || chtp == Character.OTHER_SYMBOL;
    }

    private def getIdentRest: unit =
      while (true) {
	in.ch match {
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
	    putChar(in.ch);
	    in.next;
	  case '_' =>
	    putChar(in.ch);
	    in.next;
	    getIdentOrOperatorRest;
	    return;
	  case _ =>
	    if(java.lang.Character.isUnicodeIdentifierPart(in.ch)) {
	      putChar(in.ch);
	      in.next
	    } else {
	      setName;
	      token = name2token(name);
	      return
	    }
	}
      }

    private def getOperatorRest: unit =
      while (true) {
	in.ch match {
	  case '~' | '!' | '@' | '#' | '%' |
	       '^' | '*' | '+' | '-' | '<' |
	       '>' | '?' | ':' | '=' | '&' |
	       '|' | '\\' =>
	    putChar(in.ch);
	    in.next
	  case '/' =>
	    in.next;
	    if (skipComment()) {
	      setName;
	      token = name2token(name);
	      return;
	    } else {
	      putChar('/');
	    }
	  case _ =>
	    if (isSpecial(in.ch)) {
	      putChar(in.ch);
	      in.next;
	    } else {
	      setName;
	      token = name2token(name);
	      return;
	    }
	}
      }

    private def getIdentOrOperatorRest: unit =
      if (isIdentPart(in.ch))
	getIdentRest
      else in.ch match {
	case '~' | '!' | '@' | '#' | '%' |
	     '^' | '*' | '+' | '-' | '<' |
	     '>' | '?' | ':' | '=' | '&' |
	     '|' | '\\' | '/' =>
	  getOperatorRest;
	case _ =>
	  if (isSpecial(in.ch)) getOperatorRest
	  else {
	    setName;
	    token = name2token(name)
	  }
      }

    private def getStringLit(delimiter: char): unit = {
      in.next;
      while (in.ch != delimiter && in.ch != CR && in.ch != LF && in.ch != SU) {
	getlitch();
      }
      if (in.ch == delimiter) {
	token = STRINGLIT;
	setName;
	in.next
      } else {
	syntaxError("unclosed string literal");
      }
    }

// Literals -----------------------------------------------------------------

    /** read next character in character or string literal:
    */
    protected def getlitch() =
      if (in.ch == '\\') {
	in.next;
	if ('0' <= in.ch && in.ch <= '7') {
	  val leadch: char = in.ch;
	  var oct: int = in.digit2int(in.ch, 8);
	  in.next;
	  if ('0' <= in.ch && in.ch <= '7') {
	    oct = oct * 8 + in.digit2int(in.ch, 8);
	    in.next;
	    if (leadch <= '3' && '0' <= in.ch && in.ch <= '7') {
	      oct = oct * 8 + in.digit2int(in.ch, 8);
	      in.next;
	    }
	  }
	  putChar(oct.asInstanceOf[char]);
	} else
	  in.ch match {
	    case 'b'  => putChar('\b')
	    case 't'  => putChar('\t')
	    case 'n'  => putChar('\n')
	    case 'f'  => putChar('\f')
	    case 'r'  => putChar('\r')
	    case '\"' => putChar('\"')
	    case '\'' => putChar('\'')
	    case '\\' => putChar('\\')
	    case _    =>
	      syntaxError(Position.encode(in.cline, in.ccol - 1),
			  "invalid escape character");
	      putChar(in.ch);
	  }
	  in.next;
      } else  {
	putChar(in.ch);
	in.next;
      }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction = {
      while ('0' <= in.ch && in.ch <= '9') {
	putChar(in.ch);
	in.next;
	token = DOUBLELIT;
      }
      if (in.ch == 'e' || in.ch == 'E') {
	val lookahead = in.copy;
	lookahead.next;
	if (lookahead.ch == '+' || lookahead.ch == '-') {
	  lookahead.next;
	}
	if ('0' <= lookahead.ch && lookahead.ch <= '9') {
	  putChar(in.ch);
	  in.next;
	  if (in.ch == '+' || in.ch == '-') {
	    putChar(in.ch);
	    in.next;
	  }
	  while ('0' <= in.ch && in.ch <= '9') {
	    putChar(in.ch);
	    in.next;
	  }
	}
	token = DOUBLELIT;
      }
      if ((in.ch == 'd') || (in.ch == 'D')) {
	putChar(in.ch);
	in.next;
	token = DOUBLELIT;
      } else if ((in.ch == 'f') || (in.ch == 'F')) {
	putChar(in.ch);
	in.next;
	token = FLOATLIT;
      }
      setName
    }

    /** convert name to long value
     */
    def intVal(negated: boolean): long = {
      if (token == CHARLIT && !negated) {
	if (name.length > 0) name(0) else 0
      } else {
	var value: long = 0;
	val divider = if (base == 10) 1 else 2;
	val limit: long =
	  if (token == LONGLIT) Long.MAX_VALUE else Integer.MAX_VALUE;
	var i = 0;
	val len = name.length;
	while (i < len) {
	  val d = in.digit2int(name(i), base);
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
    protected def getNumber:unit = {
      while (in.digit2int(in.ch, if (base < 10) 10 else base) >= 0) {
	putChar(in.ch);
	in.next;
      }
      token = INTLIT;
      if (base <= 10 && in.ch == '.') {
	putChar(in.ch);
	in.next;
	getFraction
      } else if (base <= 10 &&
		 (in.ch == 'e' || in.ch == 'E' ||
		  in.ch == 'f' || in.ch == 'F' ||
		  in.ch == 'd' || in.ch == 'D')) {
	getFraction
      } else {
	setName;
	if (in.ch == 'l' || in.ch == 'L') {
	  in.next;
	  token = LONGLIT;
	}
      }
    }

// XML lexing----------------------------------------------------------------

/*
    // start XML tokenizing methods
    // prod. [i] refers to productions in http://www.w3.org/TR/REC-xml

    /** calls nextToken, starting the scanning of Scala tokens,
    *   after XML tokens.
    */
    def xSync = {
      token = SEMI; // avoid getting SEMI from nextToken if last was RBRACE
      //in.next;
      nextToken();
    }

    def xSync2 = fetchToken();

    def xLookahead = srcIterator.lookahead1;

    /** read the next character. do not skip whitespace.
    *   treat CR LF as single LF. update ccol and cline
    *
    *   @todo: may XML contain SU, in CDATA sections ?
    */
    def xNext = {
      lastpos = pos;
      ch = srcIterator.raw; ccol = ccol + 1; // = in.next without unicode
      ch match {
	case SU =>
	  syntaxError(lastpos, "unclosed XML literal");
	  token = EOF;
	case LF =>
	  ccol = 0; cline = cline + 1;
	case CR =>
	  if (LF == srcIterator.lookahead1) {
	    srcIterator.raw; ccol = 0; cline = cline + 1;
	  }
	case _ =>
	  //Console.print(ch.asInstanceOf[char]); // DEBUG
      }
      pos = Position.encode(cline, ccol);
      //Console.print(ch);
    }

    final val LT   = Name.fromString("<");

    def xStartsXML = {
      /* unit.global.xmlMarkup && */ (token == IDENTIFIER) &&(name == LT);
      /* ||  // support for proc instr, cdata, comment... ?
	 {val s = name.toString();
	  s.charAt(0) == '<' && (s.charAt(1)=='?' || s.charAt(1)=='!')}) */
    }

    // end XML tokenizing

*/
// Errors -----------------------------------------------------------------

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

// Keywords -----------------------------------------------------------------

    /** Keyword array; maps from name indices to tokens */
    private var key: Array[byte] = _;
    private var maxKey = 0;
    private var tokenName = new Array[Name](128);

    {
      var tokenCount = 0;

      // Enter keywords

      def enterKeyword(s: String, tokenId: int) = {
	while (tokenId >= tokenName.length) {
	  val newTokName = new Array[Name](tokenName.length * 2);
	  System.arraycopy(tokenName, 0, newTokName, 0, newTokName.length);
	  tokenName = newTokName;
	}
	val n = newTermName(s);
	tokenName(tokenId) = n;
	if (n.start > maxKey) maxKey = n.start;
	if (tokenId >= tokenCount) tokenCount = tokenId + 1;
      }

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
      enterKeyword("implicit", IMPLICIT);
      enterKeyword("import", IMPORT);
      enterKeyword("match", MATCH);
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
      enterKeyword("<%", VIEWBOUND);
      enterKeyword(">:", SUPERTYPE);
      enterKeyword("#", HASH);
      enterKeyword("@", AT);

      // Build keyword array
      key = new Array[byte](maxKey+1);
      for (val i <- Iterator.range(0, maxKey + 1))
	key(i) = IDENTIFIER;
      for (val j <- Iterator.range(0, tokenCount))
	if (tokenName(j) != null)
	  key(tokenName(j).start) = j.asInstanceOf[byte];

    }

// Token representation -----------------------------------------------------

    /** Convert name to token */
    def name2token(name: Name): int =
      if (name.start <= maxKey) key(name.start) else IDENTIFIER;

    /** Returns the string representation of given token. */
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
	  "'" + tokenName(token) + "'"
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

    /** INIT: read lookahead character and token.
     */
    in.next;
    nextToken();
  }
}
