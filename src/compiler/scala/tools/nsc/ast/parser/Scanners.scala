/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast.parser

import compat.StringBuilder
import Tokens._
import scala.tools.nsc.util.{Position, SourceFile}
import SourceFile.{LF, FF, CR, SU}
import scala.tools.nsc.util.CharArrayReader

trait Scanners requires SyntaxAnalyzer {

  import global._

  /** A class for representing a token's data. */
  class TokenData {

    /** the next token */
    var token: int = EMPTY

    /** the token's position */
    var pos: int = 0

    /** the first character position after the previous token */
    var lastPos: int = 0

    def currentPos = pos - 1


    /** the name of an identifier or token */
    var name: Name = null

    /** the base of a number */
    var base: int = 0

    def copyFrom(td: TokenData) = {
      this.token = td.token
      this.pos = td.pos
      this.lastPos = td.lastPos
      this.name = td.name
      this.base = td.base
    }
  }

  /** A scanner for the programming language Scala.
   *
   *  @author     Matthias Zenger, Martin Odersky, Burak Emir
   *  @version    1.1
   */
  class Scanner(unit: CompilationUnit) extends TokenData {

    import Tokens._
    import java.lang.{Integer, Long, Float, Double, Character}

    /** Character input reader
     */
    val in = new CharArrayReader(unit.source.getContent(), !settings.nouescape.value, syntaxError)

    /** character buffer for literals
     */
    val cbuf = new StringBuilder()

    /** append Unicode character to "lit" buffer
    */
    protected def putChar(c: char): unit = cbuf.append(c)

    /** Clear buffer and set name */
    private def setName: unit = {
      name = newTermName(cbuf.toString())
      cbuf.setLength(0)
    }

    /** buffer for the documentation comment
     */
    var docBuffer: StringBuilder = null

    /** add the given character to the documentation buffer
     */
    protected def putDocChar(c: char): unit =
      if (docBuffer ne null) docBuffer.append(c)

    /** we need one token lookahead
     */
    val next = new TokenData()
    val prev = new TokenData()

    /** the last error position
     */
    var errpos = -1

    /** a stack which indicates whether line-ends can be statement separators
     */
    var sepRegions: List[int] = List()

    /** A new line was inserted where in version 1.0 it would not be.
     *  Only significant if settings.migrate.value is set
     */
    var newNewLine = false

    /** Parser is currently skipping ahead because of an error.
     *  Only significant if settings.migrate.value is set
     */
    var skipping = false

// Get next token ------------------------------------------------------------

    /** read next token and return last position
     */
    def skipToken(): int = {
      val p = pos; nextToken()
      // XXX: account for off by one error //???
      p - 1
    }

    def nextToken(): unit = {
      if (token == LPAREN) {
        sepRegions = RPAREN :: sepRegions
      } else if (token == LBRACKET) {
        sepRegions = RBRACKET :: sepRegions
      } else if  (token == LBRACE) {
        sepRegions = RBRACE :: sepRegions
      } else if (token == CASE) {
        sepRegions = ARROW :: sepRegions
      } else if (token == RBRACE) {
        while (!sepRegions.isEmpty && sepRegions.head != RBRACE)
          sepRegions = sepRegions.tail
        if (!sepRegions.isEmpty)
          sepRegions = sepRegions.tail
      } else if (token == RBRACKET || token == RPAREN || token == ARROW) {
        if (!sepRegions.isEmpty && sepRegions.head == token)
          sepRegions = sepRegions.tail
      }

      if (newNewLine && !skipping) {
        unit.warning(pos, migrateMsg + "new line will start a new statement here;\n"
                     + "to suppress it, enclose expression in parentheses (...)")
        newNewLine = false
      }

      val lastToken = token
      if (next.token == EMPTY) {
        fetchToken()
      } else {
        this.copyFrom(next)
        next.token = EMPTY
      }

      if (token == CASE) {
        prev.copyFrom(this)
        fetchToken()
        if (token == CLASS) {
          token = CASECLASS
          lastPos = prev.lastPos
        } else if (token == OBJECT) {
          token = CASEOBJECT
          lastPos = prev.lastPos
        } else {
          next.copyFrom(this)
          this.copyFrom(prev)
        }
      } else if (token == SEMI) {
        prev.copyFrom(this)
        fetchToken()
        if (token != ELSE) {
          next.copyFrom(this)
          this.copyFrom(prev)
        }
      } else if (token == IDENTIFIER && name == nme.MIXINkw) { //todo: remove eventually
        prev.copyFrom(this)
        fetchToken()
        if (token == CLASS)
          unit.warning(prev.pos, "`mixin' is no longer a reserved word; you should use `trait' instead of `mixin class'");
        next.copyFrom(this)
        this.copyFrom(prev)
      }

      if (afterLineEnd() && inLastOfStat(lastToken) && inFirstOfStat(token) &&
          (sepRegions.isEmpty || sepRegions.head == RBRACE)) {
        next.copyFrom(this)
        pos = in.lineStartPos
        if (settings.migrate.value) newNewLine = lastToken != RBRACE && token != EOF;
        token = if (in.lastBlankLinePos > lastPos) NEWLINES else NEWLINE
      }
    }

    private def afterLineEnd() = (
      lastPos < in.lineStartPos &&
      (in.lineStartPos <= pos ||
       lastPos < in.lastLineStartPos && in.lastLineStartPos <= pos)
    )

    /** read next token
     */
    private def fetchToken(): unit = {
      if (token == EOF) return
      lastPos = in.cpos - 1 // Position.encode(in.cline, in.ccol)
      //var index = bp
      while (true) {
        in.ch match {
          case ' ' | '\t' | CR | LF | FF =>
            in.next
          case _ =>
            pos = in.cpos // Position.encode(in.cline, in.ccol)
            in.ch match {
              case '\u21D2' =>
                in.next; token = ARROW
                return
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
                putChar(in.ch)
                in.next
                getIdentRest  // scala-mode: wrong indent for multi-line case blocks
                return

            case '<' => // is XMLSTART?
              val last = in.last
              in.next
              last match {
                case ' '|'\t'|'\n'|'{'|'('|'>' if xml.Parsing.isNameStart(in.ch) || in.ch == '!' || in.ch == '?' =>
                  token = XMLSTART
                case _ =>
                  // Console.println("found '<', but last is '"+in.last+"'"); // DEBUG
                  putChar('<')
                  getOperatorRest
              }
              return

              case '~' | '!' | '@' | '#' | '%' |
                   '^' | '*' | '+' | '-' | /*'<' | */
                   '>' | '?' | ':' | '=' | '&' |
                   '|' | '\\' =>
                putChar(in.ch)
                in.next
                getOperatorRest; // XXX
                return
              case '/' =>
                in.next
                if (!skipComment()) {
                  putChar('/')
                  getOperatorRest
                  return
                }

              case '0' =>
                putChar(in.ch)
                in.next
                if (in.ch == 'x' || in.ch == 'X') {
                  in.next
                  base = 16
                } else {
                  base = 8
                }
                getNumber
                return
              case '1' | '2' | '3' | '4' |
                   '5' | '6' | '7' | '8' | '9' =>
                base = 10
                getNumber
                return
              case '`' =>
                in.next
                getStringLit('`', IDENTIFIER)
                return
              case '\"' =>
                in.next
                if (in.ch == '\"') {
                  in.next
                  if (in.ch == '\"') {
                    in.next
                    val saved = in.lineStartPos
                    getMultiLineStringLit
                    if (in.lineStartPos != saved) // ignore linestarts within a mulit-line string
                      in.lastLineStartPos = saved
                  } else {
                    token = STRINGLIT
                    name = nme.EMPTY
                  }
                } else {
                  getStringLit('\"', STRINGLIT)
                }
                return
              case '\'' =>
                in.next
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
                       'z' |
                       '0' | '1' | '2' | '3' | '4' |
                       '5' | '6' | '7' | '8' | '9' =>
                    putChar(in.ch)
                    in.next
                    if (in.ch != '\'') {
                      getIdentRest
                      token = SYMBOLLIT
                      return
                    }
                  case _ =>
                    if (Character.isUnicodeIdentifierStart(in.ch)) {
                      putChar(in.ch)
                      in.next
                      if (in.ch != '\'') {
                        getIdentRest
                        token = SYMBOLLIT
                        return
                      }
                    } else if (isSpecial(in.ch)) {
                      putChar(in.ch)
                      in.next
                      if (in.ch != '\'') {
                        getOperatorRest
                        token = SYMBOLLIT
                        return
                      }
                    } else {
                      getlitch()
                    }
                }
                if (in.ch == '\'') {
                  in.next
                  token = CHARLIT
                  setName
                } else {
                  syntaxError("unclosed character literal")
                }
                return
              case '.' =>
                in.next
                if ('0' <= in.ch && in.ch <= '9') {
                  putChar('.'); getFraction
                } else {
                  token = DOT
                }
                return
              case ';' =>
                in.next; token = SEMI
                return
              case ',' =>
                in.next; token = COMMA
                return
              case '(' =>   //scala-mode: need to understand character quotes
                in.next; token = LPAREN;
                return
              case '{' =>
                in.next; token = LBRACE
                return
              case ')' =>
                in.next; token = RPAREN
                return
              case '}' =>
                in.next; token = RBRACE
                return
              case '[' =>
                in.next; token = LBRACKET
                return
              case ']' =>
                in.next; token = RBRACKET
                return
              case SU =>
                if (!in.hasNext) token = EOF
                else {
                  syntaxError("illegal character")
                  in.next
                }
                return
              case _ =>
                if (Character.isUnicodeIdentifierStart(in.ch)) {
                  putChar(in.ch)
                  in.next
                  getIdentRest
                } else if (isSpecial(in.ch)) {
                  putChar(in.ch)
                  getOperatorRest
                } else {
                  syntaxError("illegal character")
                  in.next
                }
                return
            }
        }
      }
    }

    private def skipComment(): boolean =
      if (in.ch == '/') {
        do {
          in.next
        } while ((in.ch != CR) && (in.ch != LF) && (in.ch != SU))
        true
      } else if (in.ch == '*') {
        docBuffer = null
        var openComments = 1
        in.next
        if (in.ch == '*' && onlyPresentation)
          docBuffer = new StringBuilder("/**")
        while (openComments > 0) {
          do {
            do {
              if (in.ch == '/') {
                in.next; putDocChar(in.ch)
                if (in.ch == '*') {
                  in.next; putDocChar(in.ch)
                  openComments = openComments + 1
                }
              }
              if (in.ch != '*' && in.ch != SU) {
                in.next; putDocChar(in.ch)
              }
            } while (in.ch != '*' && in.ch != SU)
            while (in.ch == '*') {
              in.next; putDocChar(in.ch)
            }
          } while (in.ch != '/' && in.ch != SU)
          if (in.ch == '/') in.next
          else incompleteInputError("unclosed comment")
          openComments = openComments - 1
        }
        true
      } else {
        false
      }

    def inFirstOfStat(token: int) = token match {
      case EOF | CASE | CATCH | ELSE | EXTENDS | FINALLY | MATCH | REQUIRES | WITH | YIELD |
           COMMA | SEMI | NEWLINE | NEWLINES | DOT | USCORE | COLON | EQUALS | ARROW |
           LARROW | SUBTYPE | VIEWBOUND | SUPERTYPE | HASH | // todo: add LBRACKET
           RPAREN | RBRACKET | RBRACE =>
        false
      case _ =>
        true
    }

    def inLastOfStat(token: int) = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT |
           IDENTIFIER | BACKQUOTED_IDENT | THIS | NULL | TRUE | FALSE | RETURN | USCORE |
           TYPE | XMLSTART | RPAREN | RBRACKET | RBRACE =>
        true
      case _ =>
        false
    }

// Identifiers ---------------------------------------------------------------

    def isIdentStart(c: char): boolean = (
      ('A' <= c && c <= 'Z') ||
      ('a' <= c && c <= 'a') ||
      (c == '_') || (c == '$') ||
      Character.isUnicodeIdentifierStart(c)
    )

    def isIdentPart(c: char) = (
      isIdentStart(c) ||
      ('0' <= c && c <= '9') ||
      Character.isUnicodeIdentifierPart(c)
    )

    def isSpecial(c: char) = {
      val chtp = Character.getType(c)
      chtp == Character.MATH_SYMBOL || chtp == Character.OTHER_SYMBOL
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
            putChar(in.ch)
            in.next
          case '_' =>
            putChar(in.ch)
            in.next
            getIdentOrOperatorRest
            return
          case SU =>
            setName
            token = name2token(name)
            return
          case _ =>
            if (java.lang.Character.isUnicodeIdentifierPart(in.ch)) {
              putChar(in.ch)
              in.next
            } else {
              setName
              token = name2token(name)
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
            putChar(in.ch)
            in.next
          case '/' =>
            in.next
            if (skipComment()) {
              setName
              token = name2token(name)
              return
            } else {
              putChar('/')
            }
          case _ =>
            if (isSpecial(in.ch)) {
              putChar(in.ch)
              in.next
            } else {
              setName
              token = name2token(name)
              return
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
          getOperatorRest
        case _ =>
          if (isSpecial(in.ch)) getOperatorRest
          else {
            setName
            token = name2token(name)
          }
      }

    private def getStringLit(delimiter: char, litType: int): unit = {
      assert((litType==STRINGLIT) || (litType==IDENTIFIER))
      while (in.ch != delimiter && (in.isUnicode || in.ch != CR && in.ch != LF && in.ch != SU)) {
        getlitch()
      }
      if (in.ch == delimiter) {
        token = litType
        setName
        in.next
      } else {
        val typeDesc = if(litType == STRINGLIT) "string literal" else "quoted identifier"
        syntaxError("unclosed " + typeDesc)
      }
    }

    private def getMultiLineStringLit: unit =
      if (in.ch == '\"') {
        in.next
        if (in.ch == '\"') {
          in.next
          if (in.ch == '\"') {
            in.next
            token = STRINGLIT
            setName
          } else {
            putChar('\"')
            putChar('\"')
            getMultiLineStringLit
          }
        } else {
          putChar('\"')
          getMultiLineStringLit
        }
      } else if (in.ch == SU) {
        incompleteInputError("unclosed multi-line string literal")
      } else {
        putChar(in.ch)
        in.next
        getMultiLineStringLit
      }

// Literals -----------------------------------------------------------------

    /** read next character in character or string literal:
    */
    protected def getlitch() =
      if (in.ch == '\\') {
        in.next
        if ('0' <= in.ch && in.ch <= '7') {
          val leadch: char = in.ch
          var oct: int = in.digit2int(in.ch, 8)
          in.next
          if ('0' <= in.ch && in.ch <= '7') {
            oct = oct * 8 + in.digit2int(in.ch, 8)
            in.next
            if (leadch <= '3' && '0' <= in.ch && in.ch <= '7') {
              oct = oct * 8 + in.digit2int(in.ch, 8)
              in.next
            }
          }
          putChar(oct.asInstanceOf[char])
        } else {
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
              syntaxError(in.cpos - 1, // Position.encode(in.cline, in.ccol - 1),
                          "invalid escape character")
              putChar(in.ch)
          }
          in.next
        }
      } else  {
        putChar(in.ch)
        in.next
      }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction = {
      token = DOUBLELIT
      while ('0' <= in.ch && in.ch <= '9') {
        putChar(in.ch)
        in.next
      }
      if (in.ch == 'e' || in.ch == 'E') {
        val lookahead = in.copy
        lookahead.next
        if (lookahead.ch == '+' || lookahead.ch == '-') {
          lookahead.next
        }
        if ('0' <= lookahead.ch && lookahead.ch <= '9') {
          putChar(in.ch)
          in.next
          if (in.ch == '+' || in.ch == '-') {
            putChar(in.ch)
            in.next
          }
          while ('0' <= in.ch && in.ch <= '9') {
            putChar(in.ch)
            in.next
          }
        }
        token = DOUBLELIT
      }
      if (in.ch == 'd' || in.ch == 'D') {
        putChar(in.ch)
        in.next
        token = DOUBLELIT
      } else if (in.ch == 'f' || in.ch == 'F') {
        putChar(in.ch)
        in.next
        token = FLOATLIT
      }
      setName
    }

    /** convert name to long value
     */
    def intVal(negated: boolean): long = {
      if (token == CHARLIT && !negated) {
        if (name.length > 0) name(0) else 0
      } else {
        var value: long = 0
        val divider = if (base == 10) 1 else 2
        val limit: long =
          if (token == LONGLIT) Long.MAX_VALUE else Integer.MAX_VALUE
        var i = 0
        val len = name.length
        while (i < len) {
          val d = in.digit2int(name(i), base)
          if (d < 0) {
            syntaxError("malformed integer number")
            return 0
          }
          if (value < 0 ||
              limit / (base / divider) < value ||
              limit - (d / divider) < value * (base / divider) &&
              !(negated && limit == value * base - 1 + d)) {
                syntaxError("integer number too large")
                return 0
              }
          value = value * base + d
          i = i + 1
        }
        if (negated) -value else value
      }
    }

    def intVal: long = intVal(false)

    /** convert name, base to double value
    */
    def floatVal(negated: boolean): double = {
      val limit: double =
        if (token == DOUBLELIT) Double.MAX_VALUE else Float.MAX_VALUE
      try {
        val value = Double.valueOf(name.toString()).doubleValue()
        if (value > limit)
          syntaxError("floating point number too large")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed floating point number")
          0.0
      }
    }

    def floatVal: double = floatVal(false)

    /** read a number into name and set base
    */
    protected def getNumber:unit = {
      while (in.digit2int(in.ch, if (base < 10) 10 else base) >= 0) {
        putChar(in.ch)
        in.next
      }
      token = INTLIT
      if (base <= 10 && in.ch == '.') {
        val lookahead = in.copy
        lookahead.next
        lookahead.ch match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |
               '8' | '9' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' =>
            putChar(in.ch)
            in.next
            return getFraction
          case _ =>
            if (!isIdentStart(lookahead.ch)) {
              putChar(in.ch)
              in.next
              return getFraction
            }
        }
      }
      if (base <= 10 &&
          (in.ch == 'e' || in.ch == 'E' ||
           in.ch == 'f' || in.ch == 'F' ||
           in.ch == 'd' || in.ch == 'D')) {
        return getFraction
      }
      setName
      if (in.ch == 'l' || in.ch == 'L') {
        in.next
        token = LONGLIT
      }
    }

// XML lexing----------------------------------------------------------------
    def xSync = {
      token = NEWLINE; // avoid getting NEWLINE from nextToken if last was RBRACE
      //in.next
      nextToken()
    }

// Errors -----------------------------------------------------------------

    /** generate an error at the given position
    */
    def syntaxError(pos: int, msg: String): unit = {
      unit.error(pos, msg)
      token = ERROR
      errpos = pos
    }

    /** generate an error at the current token position
    */
    def syntaxError(msg: String): unit = syntaxError(pos, msg)

    /** signal an error where the input ended in the middle of a token */
    def incompleteInputError(msg: String): unit = {
      unit.incompleteInputError(pos, msg)
      token = EOF
      errpos = pos
    }

// Keywords -----------------------------------------------------------------

    /** Keyword array; maps from name indices to tokens */
    private var key: Array[byte] = _
    private var maxKey = 0
    private var tokenName = new Array[Name](128)

    {
      var tokenCount = 0

      // Enter keywords

      def enterKeyword(n: Name, tokenId: int): unit = {
        while (tokenId >= tokenName.length) {
          val newTokName = new Array[Name](tokenName.length * 2)
          Array.copy(tokenName, 0, newTokName, 0, newTokName.length)
          tokenName = newTokName
        }
        tokenName(tokenId) = n
        if (n.start > maxKey) maxKey = n.start
        if (tokenId >= tokenCount) tokenCount = tokenId + 1
      }

      enterKeyword(nme.ABSTRACTkw, ABSTRACT)
      enterKeyword(nme.CASEkw, CASE)
      enterKeyword(nme.CATCHkw, CATCH)
      enterKeyword(nme.CLASSkw, CLASS)
      enterKeyword(nme.DEFkw, DEF)
      enterKeyword(nme.DOkw, DO)
      enterKeyword(nme.ELSEkw, ELSE)
      enterKeyword(nme.EXTENDSkw, EXTENDS)
      enterKeyword(nme.FALSEkw, FALSE)
      enterKeyword(nme.FINALkw, FINAL)
      enterKeyword(nme.FINALLYkw, FINALLY)
      enterKeyword(nme.FORkw, FOR)
      enterKeyword(nme.IFkw, IF)
      enterKeyword(nme.IMPLICITkw, IMPLICIT)
      enterKeyword(nme.IMPORTkw, IMPORT)
      enterKeyword(nme.MATCHkw, MATCH)
      enterKeyword(nme.NEWkw, NEW)
      enterKeyword(nme.NULLkw, NULL)
      enterKeyword(nme.OBJECTkw, OBJECT)
      enterKeyword(nme.OVERRIDEkw, OVERRIDE)
      enterKeyword(nme.PACKAGEkw, PACKAGE)
      enterKeyword(nme.PRIVATEkw, PRIVATE)
      enterKeyword(nme.PROTECTEDkw, PROTECTED)
      enterKeyword(nme.REQUIRESkw, REQUIRES)
      enterKeyword(nme.RETURNkw, RETURN)
      enterKeyword(nme.SEALEDkw, SEALED)
      enterKeyword(nme.SUPERkw, SUPER)
      enterKeyword(nme.THISkw, THIS)
      enterKeyword(nme.THROWkw, THROW)
      enterKeyword(nme.TRAITkw, TRAIT)
      enterKeyword(nme.TRUEkw, TRUE)
      enterKeyword(nme.TRYkw, TRY)
      enterKeyword(nme.TYPEkw, TYPE)
      enterKeyword(nme.VALkw, VAL)
      enterKeyword(nme.VARkw, VAR)
      enterKeyword(nme.WHILEkw, WHILE)
      enterKeyword(nme.WITHkw, WITH)
      enterKeyword(nme.YIELDkw, YIELD)
      enterKeyword(nme.DOTkw, DOT)
      enterKeyword(nme.USCOREkw, USCORE)
      enterKeyword(nme.COLONkw, COLON)
      enterKeyword(nme.EQUALSkw, EQUALS)
      enterKeyword(nme.ARROWkw, ARROW)
      enterKeyword(nme.LARROWkw, LARROW)
      enterKeyword(nme.SUBTYPEkw, SUBTYPE)
      enterKeyword(nme.VIEWBOUNDkw, VIEWBOUND)
      enterKeyword(nme.SUPERTYPEkw, SUPERTYPE)
      enterKeyword(nme.HASHkw, HASH)
      enterKeyword(nme.ATkw, AT)

      // Build keyword array
      key = new Array[byte](maxKey+1)
      for (val i <- 0 to maxKey)
        key(i) = IDENTIFIER
      for (val j <- 0 until tokenCount)
        if (tokenName(j) ne null)
          key(tokenName(j).start) = j.asInstanceOf[byte]

    }

// Token representation -----------------------------------------------------

    /** Convert name to token */
    def name2token(name: Name): int =
      if (name.start <= maxKey) key(name.start) else IDENTIFIER

    /** Returns the string representation of given token. */
    def token2string(token: int): String = token match {
      case IDENTIFIER | BACKQUOTED_IDENT =>
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
      case NEWLINE =>
        "';'"
      case NEWLINES =>
        "';'"
      case COMMA =>
        "','"
      case CASECLASS =>
        "case class"
      case CASEOBJECT =>
        "case object"
      case XMLSTART =>
        "$XMLSTART$<"
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
      case IDENTIFIER | BACKQUOTED_IDENT =>
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
      case NEWLINE =>
        ";"
      case NEWLINES =>
        ";;"
      case COMMA =>
        ","
      case _ =>
        token2string(token)
    }

    /** INIT: read lookahead character and token.
     */
    in.next
    nextToken()
  }
}
