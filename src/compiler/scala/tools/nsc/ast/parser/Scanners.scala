/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Scanners.scala 17285 2009-03-11 13:51:56Z rytz $
package scala.tools.nsc
package ast.parser

import scala.tools.nsc.util._
import SourceFile.{LF, FF, CR, SU}
import Tokens._
import scala.annotation.switch
import scala.collection.mutable.{ListBuffer, ArrayBuffer}

trait Scanners {
  val global : Global
  import global._

  /** Offset into source character array */
  type Offset = Int

  /** An undefined offset */
  val NoOffset: Offset = -1

  trait TokenData {

    /** the next token */
    var token: Int = EMPTY

    /** the offset of the first character of the current token */
    var offset: Offset = 0

    /** the offset of the character following the token preceding this one */
    var lastOffset: Offset = 0

    /** the name of an identifier */
    var name: Name = null

    /** the string value of a literal */
    var strVal: String = null

    /** the base of a number */
    var base: Int = 0

    def copyFrom(td: TokenData) = {
      this.token = td.token
      this.offset = td.offset
      this.lastOffset = td.lastOffset
      this.name = td.name
      this.strVal = td.strVal
      this.base = td.base
    }
  }

  abstract class Scanner extends CharArrayReader with TokenData {

    def flush = { charOffset = offset; nextChar(); this }

    def resume(lastCode: Int) = {
      token = lastCode
      assert(next.token == EMPTY)
      nextToken()
    }

    // things to fill in, in addition to buf, decodeUni
    def warning(off: Offset, msg: String): Unit
    def error  (off: Offset, msg: String): Unit
    def incompleteInputError(off: Offset, msg: String): Unit
    def deprecationWarning(off: Offset, msg: String): Unit

    /** the last error offset
     */
    var errOffset: Offset = NoOffset

    /** A character buffer for literals
     */
    val cbuf = new StringBuilder

    /** append Unicode character to "cbuf" buffer
     */
    protected def putChar(c: Char) {
//      assert(cbuf.size < 10000, cbuf)
      cbuf.append(c)
    }

    /** Clear buffer and set name and token */
    private def finishNamed() {
      name = newTermName(cbuf.toString)
      token = name2token(name)
      cbuf.clear()
    }

    /** Clear buffer and set string */
    private def setStrVal() {
      strVal = cbuf.toString
      cbuf.clear()
    }

    /** Should doc comments be built? */
    def buildDocs: Boolean = onlyPresentation

    /** buffer for the documentation comment
     */
    var docBuffer: StringBuilder = null

    /** Return current docBuffer and set docBuffer to null */
    def flushDoc = {
      val ret = if (docBuffer != null) docBuffer.toString else null
      docBuffer = null
      ret
    }

    /** add the given character to the documentation buffer
     */
    protected def putDocChar(c: Char) {
      if (docBuffer ne null) docBuffer.append(c)
    }

    private class TokenData0 extends TokenData

    /** we need one token lookahead and one token history
     */
    val next : TokenData = new TokenData0
    val prev : TokenData = new TokenData0

    /** a stack of tokens which indicates whether line-ends can be statement separators
     */
    var sepRegions: List[Int] = List()

// Get next token ------------------------------------------------------------

    /** read next token and return last offset
     */
    def skipToken(): Offset = {
      val off = offset
      nextToken()
      off
    }

    /** Produce next token, filling TokenData fields of Scanner.
     */
    def nextToken() {
      val lastToken = token
      // Adapt sepRegions according to last token
      (lastToken: @switch) match {
        case LPAREN =>
          sepRegions = RPAREN :: sepRegions
        case LBRACKET =>
          sepRegions = RBRACKET :: sepRegions
        case LBRACE =>
          sepRegions = RBRACE :: sepRegions
        case CASE =>
          sepRegions = ARROW :: sepRegions
        case RBRACE =>
          sepRegions = sepRegions dropWhile (_ != RBRACE)
          if (!sepRegions.isEmpty) sepRegions = sepRegions.tail
        case RBRACKET | RPAREN | ARROW =>
          if (!sepRegions.isEmpty && sepRegions.head == lastToken)
            sepRegions = sepRegions.tail
        case _ =>
      }

      // Read a token or copy it from `next` tokenData
      if (next.token == EMPTY) {
        lastOffset = charOffset - 1
        fetchToken()
      } else {
        this copyFrom next
        next.token = EMPTY
      }

      /** Insert NEWLINE or NEWLINES if
       *  - we are after a newline
       *  - we are within a { ... } or on toplevel (wrt sepRegions)
       *  - the current token can start a statement and the one before can end it
       *  insert NEWLINES if we are past a blank line, NEWLINE otherwise
       */
      if (!applyBracePatch() && afterLineEnd() && inLastOfStat(lastToken) && inFirstOfStat(token) &&
          (sepRegions.isEmpty || sepRegions.head == RBRACE)) {
        next copyFrom this
        offset = if (lineStartOffset <= offset) lineStartOffset else lastLineStartOffset
        token = if (pastBlankLine()) NEWLINES else NEWLINE
      }

      // Join CASE + CLASS => CASECLASS, CASE + OBJECT => CASEOBJECT, SEMI + ELSE => ELSE
      if (token == CASE) {
        prev copyFrom this
        val nextLastOffset = charOffset - 1
        fetchToken()
        if (token == CLASS) {
          token = CASECLASS
        } else if (token == OBJECT) {
          token = CASEOBJECT
        } else {
          lastOffset = nextLastOffset
          next copyFrom this
          this copyFrom prev
        }
      } else if (token == SEMI) {
        prev copyFrom this
        fetchToken()
        if (token != ELSE) {
          next copyFrom this
          this copyFrom prev
        }
      }

//      print("["+this+"]")
    }

    /** Is current token first one after a newline? */
    private def afterLineEnd(): Boolean =
      lastOffset < lineStartOffset &&
      (lineStartOffset <= offset ||
       lastOffset < lastLineStartOffset && lastLineStartOffset <= offset)

    /** Is there a blank line between the current token and the last one?
     *  @pre  afterLineEnd().
     */
    private def pastBlankLine(): Boolean = {
      var idx = lastOffset
      var ch = buf(idx)
      val end = offset
      while (idx < end) {
        if (ch == LF || ch == FF) {
          do {
            idx += 1; ch = buf(idx)
            if (ch == LF || ch == FF) {
//              println("blank line found at "+lastOffset+":"+(lastOffset to idx).map(buf(_)).toList)
              return true
            }
          } while (idx < end && ch <= ' ')
        }
        idx += 1; ch = buf(idx)
      }
      false
    }

    /** read next token, filling TokenData fields of Scanner.
     */
    protected final def fetchToken() {
      offset = charOffset - 1
      (ch: @switch) match {

        case ' ' | '\t' | CR | LF | FF =>
          nextChar()
          fetchToken()
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
          putChar(ch)
          nextChar()
          getIdentRest()  // scala-mode: wrong indent for multi-line case blocks
        case '<' => // is XMLSTART?
          val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
          nextChar()
          last match {
            case ' '|'\t'|'\n'|'{'|'('|'>' if xml.Parsing.isNameStart(ch) || ch == '!' || ch == '?' =>
              token = XMLSTART
            case _ =>
              // Console.println("found '<', but last is '"+in.last+"'"); // DEBUG
              putChar('<')
              getOperatorRest()
          }
        case '~' | '!' | '@' | '#' | '%' |
             '^' | '*' | '+' | '-' | /*'<' | */
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' =>
          putChar(ch)
          nextChar()
          getOperatorRest()
        case '/' =>
          nextChar()
          if (skipComment()) {
            fetchToken()
          } else {
            putChar('/')
            getOperatorRest()
          }
        case '0' =>
          putChar(ch)
          nextChar()
          if (ch == 'x' || ch == 'X') {
            nextChar()
            base = 16
          } else {
            base = 8
          }
          getNumber()
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          base = 10
          getNumber()
        case '`' =>
          nextChar()
          if (getStringLit('`')) {
            finishNamed();
            if (name.length == 0) syntaxError("empty quoted identifier")
            token = BACKQUOTED_IDENT
          }
          else syntaxError("unclosed quoted identifier")
        case '\"' =>
          nextChar()
          if (ch == '\"') {
            nextChar()
            if (ch == '\"') {
              nextChar()
              val saved = lineStartOffset
              getMultiLineStringLit()
              if (lineStartOffset != saved) // ignore linestarts within a multi-line string
                lastLineStartOffset = saved
            } else {
              token = STRINGLIT
              strVal = ""
            }
          } else if (getStringLit('\"')) {
            setStrVal()
            token = STRINGLIT
          } else {
            syntaxError("unclosed string literal")
          }
        case '\'' =>
          nextChar()
          if (isIdentifierStart(ch))
            charLitOr(getIdentRest)
          else if (isSpecial(ch))
            charLitOr(getOperatorRest)
          else {
            getLitChar()
            if (ch == '\'') {
              nextChar()
              token = CHARLIT
              setStrVal()
            } else {
              syntaxError("unclosed character literal")
            }
          }
        case '.' =>
          nextChar()
          if ('0' <= ch && ch <= '9') {
            putChar('.'); getFraction()
          } else {
            token = DOT
          }
        case ';' =>
          nextChar(); token = SEMI
        case ',' =>
          nextChar(); token = COMMA
        case '(' =>
          nextChar(); token = LPAREN
        case '{' =>
          nextChar(); token = LBRACE
        case ')' =>
          nextChar(); token = RPAREN
        case '}' =>
          nextChar(); token = RBRACE
        case '[' =>
          nextChar(); token = LBRACKET
        case ']' =>
          nextChar(); token = RBRACKET
        case SU =>
          if (charOffset >= buf.length) token = EOF
          else {
            syntaxError("illegal character")
            nextChar()
          }
        case _ =>
          if (ch == '\u21D2') {
            nextChar(); token = ARROW
          } else if (ch == '\u2190') {
            nextChar(); token = LARROW
          } else if (Character.isUnicodeIdentifierStart(ch)) {
            putChar(ch)
            nextChar()
            getIdentRest()
          } else if (isSpecial(ch)) {
            putChar(ch)
            getOperatorRest()
          } else {
            syntaxError("illegal character")
            nextChar()
          }
      }
    }

    private def skipComment(): Boolean = {
      if (ch == '/') {
        do {
          nextChar()
        } while ((ch != CR) && (ch != LF) && (ch != SU))
        true
      } else if (ch == '*') {
        docBuffer = null
        var openComments = 1
        nextChar()
        if (ch == '*' && buildDocs)
          docBuffer = new StringBuilder("/**")
        while (openComments > 0) {
          do {
            do {
              if (ch == '/') {
                nextChar(); putDocChar(ch)
                if (ch == '*') {
                  nextChar(); putDocChar(ch)
                  openComments += 1
                }
              }
              if (ch != '*' && ch != SU) {
                nextChar(); putDocChar(ch)
              }
            } while (ch != '*' && ch != SU)
            while (ch == '*') {
              nextChar(); putDocChar(ch)
            }
          } while (ch != '/' && ch != SU)
          if (ch == '/') nextChar()
          else incompleteInputError("unclosed comment")
          openComments -= 1
        }
        true
      } else {
        false
      }
    }

    /** Can token start a statement? */
    def inFirstOfStat(token: Int) = token match {
      case EOF | CATCH | ELSE | EXTENDS | FINALLY | FORSOME | MATCH | WITH | YIELD |
           COMMA | SEMI | NEWLINE | NEWLINES | DOT | COLON | EQUALS | ARROW | LARROW |
           SUBTYPE | VIEWBOUND | SUPERTYPE | HASH | RPAREN | RBRACKET | RBRACE | LBRACKET =>
        false
      case _ =>
        true
    }

    /** Can token end a statement? */
    def inLastOfStat(token: Int) = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT |
           IDENTIFIER | BACKQUOTED_IDENT | THIS | NULL | TRUE | FALSE | RETURN | USCORE |
           TYPE | XMLSTART | RPAREN | RBRACKET | RBRACE =>
        true
      case _ =>
        false
    }

// Identifiers ---------------------------------------------------------------

    private def getIdentRest(): Unit = (ch: @switch) match {
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
        putChar(ch)
        nextChar()
        getIdentRest()
      case '_' =>
        putChar(ch)
        nextChar()
        getIdentOrOperatorRest()
      case SU => // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
        finishNamed()
      case _ =>
        if (Character.isUnicodeIdentifierPart(ch)) {
          putChar(ch)
          nextChar()
          getIdentRest()
        } else {
          finishNamed()
        }
    }

    private def getOperatorRest(): Unit = (ch: @switch) match {
      case '~' | '!' | '@' | '#' | '%' |
           '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' |
           '|' | '\\' =>
        putChar(ch); nextChar(); getOperatorRest()
      case '/' =>
        nextChar()
        if (skipComment()) finishNamed()
        else { putChar('/'); getOperatorRest() }
      case _ =>
        if (isSpecial(ch)) { putChar(ch); nextChar(); getOperatorRest() }
        else finishNamed()
    }

    private def getIdentOrOperatorRest() {
      if (isIdentifierPart(ch))
        getIdentRest()
      else ch match {
        case '~' | '!' | '@' | '#' | '%' |
             '^' | '*' | '+' | '-' | '<' |
             '>' | '?' | ':' | '=' | '&' |
             '|' | '\\' | '/' =>
          getOperatorRest()
        case _ =>
          if (isSpecial(ch)) getOperatorRest()
          else finishNamed()
      }
    }

    private def getStringLit(delimiter: Char): Boolean = {
      while (ch != delimiter && (isUnicodeEscape || ch != CR && ch != LF && ch != SU)) {
        getLitChar()
      }
      if (ch == delimiter) { nextChar(); true }
      else false
    }

    private def getMultiLineStringLit() {
      if (ch == '\"') {
        nextChar()
        if (ch == '\"') {
          nextChar()
          if (ch == '\"') {
            nextChar()
            token = STRINGLIT
            setStrVal()
          } else {
            putChar('\"')
            putChar('\"')
            getMultiLineStringLit()
          }
        } else {
          putChar('\"')
          getMultiLineStringLit()
        }
      } else if (ch == SU) {
        incompleteInputError("unclosed multi-line string literal")
      } else {
        putChar(ch)
        nextChar()
        getMultiLineStringLit()
      }
    }

// Literals -----------------------------------------------------------------

    /** read next character in character or string literal:
    */
    protected def getLitChar() =
      if (ch == '\\') {
        nextChar()
        if ('0' <= ch && ch <= '7') {
          val leadch: Char = ch
          var oct: Int = digit2int(ch, 8)
          nextChar()
          if ('0' <= ch && ch <= '7') {
            oct = oct * 8 + digit2int(ch, 8)
            nextChar()
            if (leadch <= '3' && '0' <= ch && ch <= '7') {
              oct = oct * 8 + digit2int(ch, 8)
              nextChar()
            }
          }
          putChar(oct.toChar)
        } else {
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
              syntaxError(charOffset - 1, "invalid escape character")
              putChar(ch)
          }
          nextChar()
        }
      } else  {
        putChar(ch)
        nextChar()
      }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction() {
      token = DOUBLELIT
      while ('0' <= ch && ch <= '9') {
        putChar(ch)
        nextChar()
      }
      if (ch == 'e' || ch == 'E') {
        val lookahead = lookaheadReader
        lookahead.nextChar()
        if (lookahead.ch == '+' || lookahead.ch == '-') {
          lookahead.nextChar()
        }
        if ('0' <= lookahead.ch && lookahead.ch <= '9') {
          putChar(ch)
          nextChar()
          if (ch == '+' || ch == '-') {
            putChar(ch)
            nextChar()
          }
          while ('0' <= ch && ch <= '9') {
            putChar(ch)
            nextChar()
          }
        }
        token = DOUBLELIT
      }
      if (ch == 'd' || ch == 'D') {
        putChar(ch)
        nextChar()
        token = DOUBLELIT
      } else if (ch == 'f' || ch == 'F') {
        putChar(ch)
        nextChar()
        token = FLOATLIT
      }
      checkNoLetter()
      setStrVal()
    }

    /** Convert current strVal to char value
     */
    def charVal: Char = if (strVal.length > 0) strVal.charAt(0) else 0

    /** Convert current strVal, base to long value
     *  This is tricky because of max negative value.
     */
    def intVal(negated: Boolean): Long = {
      if (token == CHARLIT && !negated) {
        charVal
      } else {
        var value: Long = 0
        val divider = if (base == 10) 1 else 2
        val limit: Long =
          if (token == LONGLIT) Math.MAX_LONG else Math.MAX_INT
        var i = 0
        val len = strVal.length
        while (i < len) {
          val d = digit2int(strVal charAt i, base)
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
          i += 1
        }
        if (negated) -value else value
      }
    }

    def intVal: Long = intVal(false)

    /** Convert current strVal, base to double value
    */
    def floatVal(negated: Boolean): Double = {
      val limit: Double =
        if (token == DOUBLELIT) Math.MAX_DOUBLE else Math.MAX_FLOAT
      try {
        val value: Double = java.lang.Double.valueOf(strVal).doubleValue()
        if (value > limit)
          syntaxError("floating point number too large")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed floating point number")
          0.0
      }
    }

    def floatVal: Double = floatVal(false)

    def checkNoLetter() {
      if (isIdentifierPart(ch) && ch >= ' ')
        syntaxError("Invalid literal number")
    }

    /** Read a number into strVal and set base
    */
    protected def getNumber() {
      val base1 = if (base < 10) 10 else base
        // read 8,9's even if format is octal, produce a malformed number error afterwards.
      while (digit2int(ch, base1) >= 0) {
        putChar(ch)
        nextChar()
      }
      token = INTLIT
      if (base <= 10 && ch == '.') {
        val lookahead = lookaheadReader
        lookahead.nextChar()
        lookahead.ch match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |
               '8' | '9' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' =>
            putChar(ch)
            nextChar()
            return getFraction()
          case _ =>
            if (!isIdentifierStart(lookahead.ch)) {
              putChar(ch)
              nextChar()
              return getFraction()
            }
        }
      }
      if (base <= 10 &&
          (ch == 'e' || ch == 'E' ||
           ch == 'f' || ch == 'F' ||
           ch == 'd' || ch == 'D')) {
        return getFraction()
      }
      setStrVal()
      if (ch == 'l' || ch == 'L') {
        nextChar()
        token = LONGLIT
      } else checkNoLetter()
    }

    /** Parse character literal if current character is followed by \',
     *  or follow with given op and return a symol literal token
     */
    def charLitOr(op: () => Unit) {
      putChar(ch)
      nextChar()
      if (ch == '\'') {
        nextChar()
        token = CHARLIT
        setStrVal()
      } else {
        op()
        token = SYMBOLLIT
        strVal = name.toString
      }
    }

// Errors -----------------------------------------------------------------

    /** generate an error at the given offset
    */
    def syntaxError(off: Offset, msg: String) {
      error(off, msg)
      token = ERROR
      errOffset = off
    }

    /** generate an error at the current token offset
    */
    def syntaxError(msg: String): Unit = syntaxError(offset, msg)

    /** signal an error where the input ended in the middle of a token */
    def incompleteInputError(msg: String) {
      incompleteInputError(offset, msg)
      token = EOF
      errOffset = offset
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
        "string(" + strVal + ")"
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

    // ------------- brace counting and healing ------------------------------

    /** overridden in UnitScanners:
     *  apply brace patch if one exists for this offset
     *  return true if subsequent end of line handling should be suppressed.
     */
    def applyBracePatch(): Boolean = false

    /** overridden in UnitScanners */
    def parenBalance(token: Int) = 0

    /** overridden in UnitScanners */
    def healBraces(): List[BracePatch] = List()

    /** Initialization method: read first char, then first token
     */
    def init() {
      nextChar()
      nextToken()
    }
  } // end Scanner

  // ------------- character classification --------------------------------

  def isIdentifierStart(c: Char): Boolean =
    ('A' <= c && c <= 'Z') ||
    ('a' <= c && c <= 'a') ||
    (c == '_') || (c == '$') ||
    Character.isUnicodeIdentifierStart(c)

  def isIdentifierPart(c: Char) =
    isIdentifierStart(c) ||
    ('0' <= c && c <= '9') ||
    Character.isUnicodeIdentifierPart(c)

  def isSpecial(c: Char) = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL || chtp == Character.OTHER_SYMBOL
  }

  def isOperatorPart(c : Char) : Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }

  // ------------- keyword configuration -----------------------------------

  /** Keyword array; maps from name indices to tokens */
  private var keyCode: Array[Byte] = _
  /** The highest name index of a keyword token */
  private var maxKey = 0
  /** An array of all keyword token names */
  private var keyName = new Array[Name](128)
  /** The highest keyword token plus one */
  private var tokenCount = 0

  /** Enter keyword with given name and token id */
  protected def enterKeyword(n: Name, tokenId: Int) {
    while (tokenId >= keyName.length) {
      val newTokName = new Array[Name](keyName.length * 2)
      compat.Platform.arraycopy(keyName, 0, newTokName, 0, newTokName.length)
      keyName = newTokName
    }
    keyName(tokenId) = n
    if (n.start > maxKey) maxKey = n.start
    if (tokenId >= tokenCount) tokenCount = tokenId + 1
  }

  /** Enter all keywords */
  protected def enterKeywords() {
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
    enterKeyword(nme.FORSOMEkw, FORSOME)
    enterKeyword(nme.IFkw, IF)
    enterKeyword(nme.IMPLICITkw, IMPLICIT)
    enterKeyword(nme.IMPORTkw, IMPORT)
    enterKeyword(nme.LAZYkw, LAZY)
    enterKeyword(nme.MATCHkw, MATCH)
    enterKeyword(nme.NEWkw, NEW)
    enterKeyword(nme.NULLkw, NULL)
    enterKeyword(nme.OBJECTkw, OBJECT)
    enterKeyword(nme.OVERRIDEkw, OVERRIDE)
    enterKeyword(nme.PACKAGEkw, PACKAGE)
    enterKeyword(nme.PRIVATEkw, PRIVATE)
    enterKeyword(nme.PROTECTEDkw, PROTECTED)
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
  }

  { // initialization
    enterKeywords()
    // Build keyword array
    keyCode = Array.make(maxKey + 1, IDENTIFIER)
    for (j <- 0 until tokenCount if keyName(j) ne null)
      keyCode(keyName(j).start) = j.toByte
  }

  /** Convert name to token */
  def name2token(name: Name): Int =
    if (name.start <= maxKey) keyCode(name.start) else IDENTIFIER

// Token representation ----------------------------------------------------

  /** Returns the string representation of given token. */
  def token2string(token: Int): String = (token: @switch) match {
    case IDENTIFIER | BACKQUOTED_IDENT => "identifier"
    case CHARLIT => "character literal"
    case INTLIT => "integer literal"
    case LONGLIT => "long literal"
    case FLOATLIT => "float literal"
    case DOUBLELIT => "double literal"
    case STRINGLIT => "string literal"
    case SYMBOLLIT => "symbol literal"
    case LPAREN => "'('"
    case RPAREN => "')'"
    case LBRACE => "'{'"
    case RBRACE => "'}'"
    case LBRACKET => "'['"
    case RBRACKET => "']'"
    case EOF => "eof"
    case ERROR => "something"
    case SEMI => "';'"
    case NEWLINE => "';'"
    case NEWLINES => "';'"
    case COMMA => "','"
    case CASECLASS => "case class"
    case CASEOBJECT => "case object"
    case XMLSTART => "$XMLSTART$<"
    case _ =>
      if (token <= maxKey) "'" + keyName(token) + "'"
      else "'<" + token + ">'"
  }

  /** A scanner over a given compilation unit
   */
  class UnitScanner(unit: CompilationUnit, patches: List[BracePatch]) extends Scanner {
    def this(unit: CompilationUnit) = this(unit, List())
    val buf = unit.source.asInstanceOf[BatchSourceFile].content
    val decodeUnit = !settings.nouescape.value

    def warning(off: Offset, msg: String) = unit.warning(unit.position(off), msg)
    def error  (off: Offset, msg: String) = unit.error(unit.position(off), msg)
    def incompleteInputError(off: Offset, msg: String) = unit.incompleteInputError(unit.position(off), msg)
    def deprecationWarning(off: Offset, msg: String) = unit.deprecationWarning(unit.position(off), msg)

    private var bracePatches: List[BracePatch] = patches

    lazy val parensAnalyzer = new ParensAnalyzer(unit, List())

    override def parenBalance(token: Int) = parensAnalyzer.balance(token)

    override def healBraces(): List[BracePatch] = {
      var patches: List[BracePatch] = List()
      if (!parensAnalyzer.tabSeen) {
        var bal = parensAnalyzer.balance(RBRACE)
        while (bal < 0) {
          patches = new ParensAnalyzer(unit, patches).insertRBrace()
          bal += 1
        }
        while (bal > 0) {
          patches = new ParensAnalyzer(unit, patches).deleteRBrace()
          bal -= 1
        }
      }
      patches
    }

    /** Insert or delete a brace, if a patch exists for this offset */
    override def applyBracePatch(): Boolean = {
      if (bracePatches.isEmpty || bracePatches.head.off != offset) false
      else {
        val patch = bracePatches.head
        bracePatches = bracePatches.tail
//        println("applying brace patch "+offset)//DEBUG
        if (patch.inserted) {
          next copyFrom this
          error(offset, "Missing closing brace `}' assumed here")
          token = RBRACE
          true
        } else {
          error(offset, "Unmatched closing brace '}' ignored here")
          fetchToken()
          false
        }
      }
    }
  }

  class ParensAnalyzer(unit: CompilationUnit, patches: List[BracePatch]) extends UnitScanner(unit, patches) {
    var balance = collection.mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

    init()

    /** The offset of the first token on this line, or next following line if blank
     */
    val lineStart = new ArrayBuffer[Int]

    /** The list of matching top-level brace pairs (each of which may contain nested brace pairs).
     */
    val bracePairs: List[BracePair] = {

      var lineCount = 1
      var lastOffset = 0

      def scan(bpbuf: ListBuffer[BracePair]): Int = {
        if (token != NEWLINE && token != NEWLINES) {
          while (lastOffset < offset) {
            if (buf(lastOffset) == LF) lineCount += 1
            lastOffset += 1
          }
          while (lineCount > lineStart.length)
            lineStart += offset
        }
        token match {
          case LPAREN =>
            balance(RPAREN) -= 1; nextToken(); scan(bpbuf)
          case LBRACKET =>
            balance(RBRACKET) -= 1; nextToken(); scan(bpbuf)
          case RPAREN =>
            balance(RPAREN) += 1; nextToken(); scan(bpbuf)
          case RBRACKET =>
            balance(RBRACKET) += 1; nextToken(); scan(bpbuf)
          case LBRACE =>
            balance(RBRACE) -= 1
            val lc = lineCount
            val loff = offset
            val bpbuf1 = new ListBuffer[BracePair]
            nextToken()
            val roff = scan(bpbuf1)
            if (lc != lineCount)
              bpbuf += BracePair(loff, roff, bpbuf1.toList)
            scan(bpbuf)
          case RBRACE =>
            balance(RBRACE) += 1
            val off = offset; nextToken(); off
          case EOF =>
            -1
          case _ =>
            nextToken(); scan(bpbuf)
        }
      }

      val bpbuf = new ListBuffer[BracePair]
      while (token != EOF) {
        val roff = scan(bpbuf)
        if (roff != -1) {
          val current = BracePair(-1, roff, bpbuf.toList)
          bpbuf.clear()
          bpbuf += current
        }
      }
//      println("lineStart = "+lineStart)//DEBUG
//      println("bracepairs = "+bpbuf.toList)//DEBUG
      bpbuf.toList
    }

    var tabSeen = false

    def line(offset: Int): Int = {
      def findLine(lo: Int, hi: Int): Int = {
        val mid = (lo + hi) / 2
        if (offset < lineStart(mid)) findLine(lo, mid - 1)
        else if (mid + 1 < lineStart.length && offset >= lineStart(mid + 1)) findLine(mid + 1, hi)
        else mid
      }
      findLine(0, lineStart.length - 1)
    }

    def column(offset: Int): Int = {
      var col = 0
      var i = offset - 1
      while (i >= 0 && buf(i) != CR && buf(i) != LF) {
        if (buf(i) == '\t') tabSeen = true
        col += 1
        i -= 1
      }
      col
    }

    def insertPatch(patches: List[BracePatch], patch: BracePatch): List[BracePatch] = patches match {
      case List() => List(patch)
      case bp :: bps => if (patch.off < bp.off) patch :: patches
                        else bp :: insertPatch(bps, patch)
    }

    def leftColumn(offset: Int) =
      if (offset == -1) -1 else column(lineStart(line(offset)))

    def rightColumn(offset: Int, default: Int) =
      if (offset == -1) -1
      else {
        val rlin = line(offset)
        if (lineStart(rlin) == offset) column(offset)
        else if (rlin + 1 < lineStart.length) column(lineStart(rlin + 1))
        else default
      }

    def insertRBrace(): List[BracePatch] = {
      def insert(bps: List[BracePair]): List[BracePatch] = bps match {
        case List() => patches
        case (bp @ BracePair(loff, roff, nested)) :: bps1 =>
          val lcol = leftColumn(loff)
          val rcol = rightColumn(roff, lcol)
          if (lcol <= rcol) insert(bps1)
          else {
//            println("patch inside "+bp+"/"+line(loff)+"/"+lineStart(line(loff))+"/"+lcol+"/"+rcol)//DEBUG
            val patches1 = insert(nested)
            if (patches1 ne patches) patches1
            else {
//              println("patch for "+bp)//DEBUG
              var lin = line(loff) + 1
              while (lin < lineStart.length && column(lineStart(lin)) > lcol)
                lin += 1
              if (lin < lineStart.length)
                insertPatch(patches, BracePatch(lineStart(lin), true))
              else patches
            }
          }
      }
      insert(bracePairs)
    }

    def deleteRBrace(): List[BracePatch] = {
      def delete(bps: List[BracePair]): List[BracePatch] = bps match {
        case List() => patches
        case BracePair(loff, roff, nested) :: bps1 =>
          val lcol = leftColumn(loff)
          val rcol = rightColumn(roff, lcol)
          if (lcol >= rcol) delete(bps1)
          else {
            val patches1 = delete(nested)
            if (patches1 ne patches) patches1
            else insertPatch(patches, BracePatch(roff, false))
          }
      }
      delete(bracePairs)
    }
    override def error(offset: Int, msg: String) {}
  }
}
