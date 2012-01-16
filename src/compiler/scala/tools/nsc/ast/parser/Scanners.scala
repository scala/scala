/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc
package ast.parser

import scala.tools.nsc.util._
import scala.reflect.internal.Chars._
import Tokens._
import scala.annotation.switch
import scala.collection.mutable.{ ListBuffer, ArrayBuffer }
import scala.xml.Utility.{ isNameStart }

/** See Parsers.scala / ParsersCommon for some explanation of ScannersCommon.
 */
trait ScannersCommon {
  val global : Global
  import global._

  trait CommonTokenData {
    def token: Int
    def name: TermName
  }

  trait ScannerCommon extends CommonTokenData {
    // things to fill in, in addition to buf, decodeUni which come from CharArrayReader
    def warning(off: Int, msg: String): Unit
    def error  (off: Int, msg: String): Unit
    def incompleteInputError(off: Int, msg: String): Unit
    def deprecationWarning(off: Int, msg: String): Unit
  }

  def createKeywordArray(keywords: Seq[(Name, Int)], defaultToken: Int): (Int, Array[Int]) = {
    val names = keywords sortBy (_._1.start) map { case (k, v) => (k.start, v) }
    val low   = names.head._1
    val high  = names.last._1
    val arr   = Array.fill(high - low + 1)(defaultToken)

    names foreach { case (k, v) => arr(k + low) = v }
    (low, arr)
  }
}

trait Scanners extends ScannersCommon {
  val global : Global
  import global._

  /** Offset into source character array */
  type Offset = Int

  /** An undefined offset */
  val NoOffset: Offset = -1

  trait TokenData extends CommonTokenData {

    /** the next token */
    var token: Int = EMPTY

    /** the offset of the first character of the current token */
    var offset: Offset = 0

    /** the offset of the character following the token preceding this one */
    var lastOffset: Offset = 0

    /** the name of an identifier */
    var name: TermName = null

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

  abstract class Scanner extends CharArrayReader with TokenData with ScannerCommon {
    private def isDigit(c: Char) = java.lang.Character isDigit c

    def flush = { charOffset = offset; nextChar(); this }

    def resume(lastCode: Int) = {
      token = lastCode
      if (next.token != EMPTY && !reporter.hasErrors)
        syntaxError("unexpected end of input: possible missing '}' in XML block")

      nextToken()
    }

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
    def buildDocs: Boolean = forScaladoc

    /** buffer for the documentation comment
     */
    var docBuffer: StringBuilder = null
    var docPos: Position = null

    /** Return current docBuffer and set docBuffer to null */
    def flushDoc: DocComment = {
      val ret = if (docBuffer != null) DocComment(docBuffer.toString, docPos) else null
      docBuffer = null
      ret
    }

    /** add the given character to the documentation buffer
     */
    protected def putDocChar(c: Char) {
      if (docBuffer ne null) docBuffer.append(c)
    }

    protected def foundComment(value: String, start: Int, end: Int) = ()
    protected def foundDocComment(value: String, start: Int, end: Int) = ()

    private class TokenData0 extends TokenData

    /** we need one token lookahead and one token history
     */
    val next : TokenData = new TokenData0
    val prev : TokenData = new TokenData0

    /** a stack of tokens which indicates whether line-ends can be statement separators
     *  also used for keeping track of nesting levels.
     *  We keep track of the closing symbol of a region. This can be
     *  RPAREN    if region starts with '('
     *  RBRACKET  if region starts with '['
     *  RBRACE    if region starts with '{'
     *  ARROW     if region starts with `case'
     *  STRINGLIT if region is a string interpolation expression starting with '${'
     *            (the STRINGLIT appears twice in succession on the stack iff the 
     *             expression is a multiline string literal).
     */
    var sepRegions: List[Int] = List()

// Get next token ------------------------------------------------------------

    /** Are we directly in a string interpolation expression?
     */
    @inline private def inStringInterpolation = 
      sepRegions.nonEmpty && sepRegions.head == STRINGLIT
    
    /** Are we directly in a multiline string interpolation expression?
     *  @pre: inStringInterpolation
     */
    @inline private def inMultiLineInterpolation = 
      sepRegions.tail.nonEmpty && sepRegions.tail.head == STRINGPART    
    
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
          while (!sepRegions.isEmpty && sepRegions.head != RBRACE) 
            sepRegions = sepRegions.tail
          if (!sepRegions.isEmpty) sepRegions = sepRegions.tail
          docBuffer = null
        case RBRACKET | RPAREN =>
          if (!sepRegions.isEmpty && sepRegions.head == lastToken)
            sepRegions = sepRegions.tail
          docBuffer = null
        case ARROW =>
          if (!sepRegions.isEmpty && sepRegions.head == lastToken)
            sepRegions = sepRegions.tail
        case STRINGLIT =>
          if (inStringInterpolation)
            sepRegions = sepRegions.tail
        case _ =>
      }
 
      // Read a token or copy it from `next` tokenData
      if (next.token == EMPTY) {
        lastOffset = charOffset - 1
        if (lastOffset > 0 && buf(lastOffset) == '\n' && buf(lastOffset - 1) == '\r') {
          lastOffset -= 1
        }
        if (inStringInterpolation) fetchStringPart() else fetchToken()
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
	    if (idx == end) return false
          } while (ch <= ' ')
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
          getIdentRest()  
          if (ch == '"' && token == IDENTIFIER && settings.Xexperimental.value) 
            token = INTERPOLATIONID
        case '<' => // is XMLSTART?
          val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
          nextChar()
          last match {
            case ' '|'\t'|'\n'|'{'|'('|'>' if isNameStart(ch) || ch == '!' || ch == '?' =>
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
          }
          else {
            /** What should leading 0 be in the future? It is potentially dangerous
             *  to let it be base-10 because of history.  Should it be an error? Is
             *  there a realistic situation where one would need it?
             */
            if (isDigit(ch)) {
              if (opt.future) syntaxError("Non-zero numbers may not have a leading zero.")
              else deprecationWarning("Treating numbers with a leading zero as octal is deprecated.")
            }
            base = 8
          }
          getNumber()
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          base = 10
          getNumber()
        case '`' =>
          getBackquotedIdent()
        case '\"' =>
          if (token == INTERPOLATIONID) {
            nextRawChar()
            if (ch == '\"') {
              nextRawChar()
              if (ch == '\"') {
                nextRawChar()
                getStringPart(multiLine = true)
                sepRegions = STRINGLIT :: sepRegions // indicate string part
                sepRegions = STRINGLIT :: sepRegions // once more to indicate multi line string part
              } else {
                token = STRINGLIT
                strVal = ""
              }
            } else {
              getStringPart(multiLine = false)
              sepRegions = STRINGLIT :: sepRegions // indicate single line string part
            }
          } else {
            nextChar()
            if (ch == '\"') {
              nextChar()
              if (ch == '\"') {
                nextRawChar()
                getRawStringLit()
              } else {
                token = STRINGLIT
                strVal = ""
              }
            } else { 
              getStringLit()
            }
          }
        case '\'' =>
          nextChar()
          if (isIdentifierStart(ch))
            charLitOr(getIdentRest)
          else if (isOperatorPart(ch) && (ch != '\\'))
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
            nextChar()
            getOperatorRest()
          } else {
            syntaxError("illegal character")
            nextChar()
          }
      }
    }

    private def skipComment(): Boolean = {

      if (ch == '/' || ch == '*') {

        val comment = new StringBuilder("/")
        def appendToComment() = comment.append(ch)

        if (ch == '/') {
          do {
        	appendToComment()
            nextChar()
          } while ((ch != CR) && (ch != LF) && (ch != SU))
        } else {
          docBuffer = null
          var openComments = 1
          appendToComment()
          nextChar()
          appendToComment()
          var buildingDocComment = false
          if (ch == '*' && buildDocs) {
            buildingDocComment = true
            docBuffer = new StringBuilder("/**")
          }
          while (openComments > 0) {
            do {
              do {
                if (ch == '/') {
                  nextChar(); putDocChar(ch); appendToComment()
                  if (ch == '*') {
                    nextChar(); putDocChar(ch); appendToComment()
                    openComments += 1
                  }
                }
                if (ch != '*' && ch != SU) {
                  nextChar(); putDocChar(ch); appendToComment()
                }
              } while (ch != '*' && ch != SU)
              while (ch == '*') {
                nextChar(); putDocChar(ch); appendToComment()
              }
            } while (ch != '/' && ch != SU)
            if (ch == '/') nextChar()
            else incompleteInputError("unclosed comment")
            openComments -= 1
          }

          if (buildingDocComment)
            foundDocComment(comment.toString, offset, charOffset - 2)
        }

        foundComment(comment.toString, offset, charOffset - 2)
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

    private def getBackquotedIdent() {
      nextChar()
      getLitChars('`')
      if (ch == '`') {
        nextChar()
        finishNamed()
        if (name.length == 0) syntaxError("empty quoted identifier")
        token = BACKQUOTED_IDENT
      }
      else syntaxError("unclosed quoted identifier")
    }

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
    
 
// Literals -----------------------------------------------------------------

    private def getStringLit() = {
      getLitChars('"')
      if (ch == '"') {
        setStrVal()
        nextChar()
        token = STRINGLIT
      } else syntaxError("unclosed string literal")
    }

    private def getRawStringLit(): Unit = {
      if (ch == '\"') {
        nextRawChar()
        if (isTripleQuote()) {
          setStrVal()
          token = STRINGLIT
        } else
          getRawStringLit()
      } else if (ch == SU) {
        incompleteInputError("unclosed multi-line string literal")
      } else {
        putChar(ch)
        nextRawChar()
        getRawStringLit()
      }
    }
   
    @annotation.tailrec private def getStringPart(multiLine: Boolean): Unit = {
      def finishStringPart() = {
        setStrVal()
        token = STRINGPART
        next.lastOffset = charOffset - 1
        next.offset = charOffset - 1
      }   
      if (ch == '"') {
        nextRawChar()
        if (!multiLine || isTripleQuote()) {
          setStrVal()
          token = STRINGLIT
        } else 
          getStringPart(multiLine)
      } else if (ch == '$') {
        nextRawChar()
        if (ch == '$') {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        } else if (ch == '{') {
          finishStringPart()
          nextRawChar()
          next.token = LBRACE
        } else if (Character.isUnicodeIdentifierStart(ch)) {
          finishStringPart()
          do {
            putChar(ch)
            nextRawChar()
          } while (Character.isUnicodeIdentifierPart(ch))
          next.token = IDENTIFIER
          next.name = newTermName(cbuf.toString)
          cbuf.clear()
        } else {
          syntaxError("invalid string interpolation")
        }
      } else if ((ch == CR || ch == LF || ch == SU) && !isUnicodeEscape) {
        syntaxError("unclosed string literal")
      } else {
        putChar(ch)
        nextRawChar()
        getStringPart(multiLine)
      }
    }
  
    private def fetchStringPart() = {
      offset = charOffset - 1
      getStringPart(multiLine = inMultiLineInterpolation)
    }
    
    private def isTripleQuote(): Boolean =
      if (ch == '"') {
        nextRawChar()
        if (ch == '"') {
          nextChar()
          while (ch == '"') {
            putChar('"')
            nextChar()
          }
          true
        } else {
          putChar('"')
          putChar('"')
          false
        }
      } else {
        putChar('"')
        false
      }

    /** copy current character into cbuf, interpreting any escape sequences, 
     *  and advance to next character.
     */
    protected def getLitChar(): Unit =
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
            case _    => invalidEscape()
          }
          nextChar()
        }
      } else  {
        putChar(ch)
        nextChar()
      }

    protected def invalidEscape(): Unit = {
      syntaxError(charOffset - 1, "invalid escape character")
      putChar(ch)
    }

    private def getLitChars(delimiter: Char) =
      while (ch != delimiter && (ch != CR && ch != LF && ch != SU || isUnicodeEscape)) {
        getLitChar()
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
          if (token == LONGLIT) Long.MaxValue else Int.MaxValue
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
        if (token == DOUBLELIT) Double.MaxValue else Float.MaxValue
      try {
        val value: Double = java.lang.Double.valueOf(strVal).doubleValue()
        def isDeprecatedForm = {
          val idx = strVal indexOf '.'
          (idx == strVal.length - 1) || (
               (idx >= 0)
            && (idx + 1 < strVal.length)
            && (!Character.isDigit(strVal charAt (idx + 1)))
          )
        }
        if (value > limit)
          syntaxError("floating point number too large")
        if (isDeprecatedForm) {
          deprecationWarning("This lexical syntax is deprecated.  From scala 2.11, a dot will only be considered part of a number if it is immediately followed by a digit.")
        }

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

      /** When we know for certain it's a number after using a touch of lookahead */
      def restOfNumber() = {
        putChar(ch)
        nextChar()
        getFraction()
      }
      def restOfUncertainToken() = {
        def isEfd = ch match { case 'e' | 'E' | 'f' | 'F' | 'd' | 'D' => true ; case _ => false }
        def isL   = ch match { case 'l' | 'L' => true ; case _ => false }

        if (base <= 10 && isEfd)
          getFraction()
        else {
          setStrVal()
          if (isL) {
            nextChar()
            token = LONGLIT
          }
          else checkNoLetter()
        }
      }

      if (base > 10 || ch != '.')
        restOfUncertainToken()
      else {
        val lookahead = lookaheadReader
        val c = lookahead.getc()

        /** As of scala 2.11, it isn't a number unless c here is a digit, so
         *  opt.future excludes the rest of the logic.
         */
        if (opt.future && !isDigit(c))
          return setStrVal()

        val isDefinitelyNumber = (c: @switch) match {
          /** Another digit is a giveaway. */
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'  =>
            true

          /** Backquoted idents like 22.`foo`. */
          case '`' =>
            return setStrVal()  /** Note the early return */

          /** These letters may be part of a literal, or a method invocation on an Int.
           */
          case 'd' | 'D' | 'f' | 'F' =>
            !isIdentifierPart(lookahead.getc())

          /** A little more special handling for e.g. 5e7 */
          case 'e' | 'E' =>
            val ch = lookahead.getc()
            !isIdentifierPart(ch) || (isDigit(ch) || ch == '+' || ch == '-')

          case x  =>
            !isIdentifierStart(x)
        }
        if (isDefinitelyNumber) restOfNumber()
        else restOfUncertainToken()
      }
    }

    /** Parse character literal if current character is followed by \',
     *  or follow with given op and return a symbol literal token
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

    def deprecationWarning(msg: String): Unit = deprecationWarning(offset, msg)

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
      case STRINGPART =>
        "stringpart(" + strVal + ")"
      case INTERPOLATIONID =>
        "interpolationid(" + name + ")"
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

  // ------------- keyword configuration -----------------------------------

  private val allKeywords = List[(Name, Int)](
    nme.ABSTRACTkw  -> ABSTRACT,
    nme.CASEkw      -> CASE,
    nme.CATCHkw     -> CATCH,
    nme.CLASSkw     -> CLASS,
    nme.DEFkw       -> DEF,
    nme.DOkw        -> DO,
    nme.ELSEkw      -> ELSE,
    nme.EXTENDSkw   -> EXTENDS,
    nme.FALSEkw     -> FALSE,
    nme.FINALkw     -> FINAL,
    nme.FINALLYkw   -> FINALLY,
    nme.FORkw       -> FOR,
    nme.FORSOMEkw   -> FORSOME,
    nme.IFkw        -> IF,
    nme.IMPLICITkw  -> IMPLICIT,
    nme.IMPORTkw    -> IMPORT,
    nme.LAZYkw      -> LAZY,
    nme.MATCHkw     -> MATCH,
    nme.NEWkw       -> NEW,
    nme.NULLkw      -> NULL,
    nme.OBJECTkw    -> OBJECT,
    nme.OVERRIDEkw  -> OVERRIDE,
    nme.PACKAGEkw   -> PACKAGE,
    nme.PRIVATEkw   -> PRIVATE,
    nme.PROTECTEDkw -> PROTECTED,
    nme.RETURNkw    -> RETURN,
    nme.SEALEDkw    -> SEALED,
    nme.SUPERkw     -> SUPER,
    nme.THISkw      -> THIS,
    nme.THROWkw     -> THROW,
    nme.TRAITkw     -> TRAIT,
    nme.TRUEkw      -> TRUE,
    nme.TRYkw       -> TRY,
    nme.TYPEkw      -> TYPE,
    nme.VALkw       -> VAL,
    nme.VARkw       -> VAR,
    nme.WHILEkw     -> WHILE,
    nme.WITHkw      -> WITH,
    nme.YIELDkw     -> YIELD,
    nme.DOTkw       -> DOT,
    nme.USCOREkw    -> USCORE,
    nme.COLONkw     -> COLON,
    nme.EQUALSkw    -> EQUALS,
    nme.ARROWkw     -> ARROW,
    nme.LARROWkw    -> LARROW,
    nme.SUBTYPEkw   -> SUBTYPE,
    nme.VIEWBOUNDkw -> VIEWBOUND,
    nme.SUPERTYPEkw -> SUPERTYPE,
    nme.HASHkw      -> HASH,
    nme.ATkw        -> AT
  )

  private var kwOffset: Int = -1
  private val kwArray: Array[Int] = {
    val (offset, arr) = createKeywordArray(allKeywords, IDENTIFIER)
    kwOffset = offset
    arr
  }

  final val token2name = allKeywords map (_.swap) toMap

  /** Convert name to token */
  final def name2token(name: Name) = {
    val idx = name.start - kwOffset
    if (idx >= 0 && idx < kwArray.length) kwArray(idx)
    else IDENTIFIER
  }

// Token representation ----------------------------------------------------

  /** Returns the string representation of given token. */
  def token2string(token: Int): String = (token: @switch) match {
    case IDENTIFIER | BACKQUOTED_IDENT => "identifier"
    case CHARLIT => "character literal"
    case INTLIT => "integer literal"
    case LONGLIT => "long literal"
    case FLOATLIT => "float literal"
    case DOUBLELIT => "double literal"
    case STRINGLIT | STRINGPART | INTERPOLATIONID => "string literal"
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
      (token2name get token) match {
        case Some(name) => "'" + name + "'"
        case _          => "'<" + token + ">'"
      }
  }

  class MalformedInput(val offset: Int, val msg: String) extends Exception

  /** A scanner for a given source file not necessarily attached to a compilation unit.
   *  Useful for looking inside source files that aren not currently compiled to see what's there
   */
  class SourceFileScanner(val source: SourceFile) extends Scanner {
    val buf = source.content
    override val decodeUni: Boolean = !settings.nouescape.value

    // suppress warnings, throw exception on errors
    def warning(off: Offset, msg: String): Unit = ()
    def deprecationWarning(off: Offset, msg: String): Unit = ()
    def error  (off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
    def incompleteInputError(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
  }

  /** A scanner over a given compilation unit
   */
  class UnitScanner(unit: CompilationUnit, patches: List[BracePatch]) extends SourceFileScanner(unit.source) {
    def this(unit: CompilationUnit) = this(unit, List())

    override def warning(off: Offset, msg: String)              = unit.warning(unit.position(off), msg)
    override def deprecationWarning(off: Offset, msg: String)   = unit.deprecationWarning(unit.position(off), msg)
    override def error  (off: Offset, msg: String)              = unit.error(unit.position(off), msg)
    override def incompleteInputError(off: Offset, msg: String) = unit.incompleteInputError(unit.position(off), msg)

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

    override def foundComment(value: String, start: Int, end: Int) {
      val pos = new RangePosition(unit.source, start, start, end)
      unit.comment(pos, value)
    }

    override def foundDocComment(value: String, start: Int, end: Int) {
      docPos = new RangePosition(unit.source, start, start, end)
      unit.comment(docPos, value)
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
      var indent = 0
      val oldBalance = collection.mutable.Map[Int, Int]()
      def markBalance() = for ((k, v) <- balance) oldBalance(k) = v
      markBalance()

      def scan(bpbuf: ListBuffer[BracePair]): (Int, Int) = {
        if (token != NEWLINE && token != NEWLINES) {
          while (lastOffset < offset) {
            if (buf(lastOffset) == LF) lineCount += 1
            lastOffset += 1
          }
          while (lineCount > lineStart.length) {
            lineStart += offset
            // reset indentation unless there are new opening brackets or
            // braces since last ident line and at the same time there
            // are no new braces.
            if (balance(RPAREN) >= oldBalance(RPAREN) &&
                balance(RBRACKET) >= oldBalance(RBRACKET) ||
                balance(RBRACE) != oldBalance(RBRACE)) {
              indent = column(offset)
              markBalance()
            }
          }
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
            val lindent = indent
            val bpbuf1 = new ListBuffer[BracePair]
            nextToken()
            val (roff, rindent) = scan(bpbuf1)
            if (lc != lineCount)
              bpbuf += BracePair(loff, lindent, roff, rindent, bpbuf1.toList)
            scan(bpbuf)
          case RBRACE =>
            balance(RBRACE) += 1
            val off = offset; nextToken(); (off, indent)
          case EOF =>
            (-1, -1)
          case _ =>
            nextToken(); scan(bpbuf)
        }
      }

      val bpbuf = new ListBuffer[BracePair]
      while (token != EOF) {
        val (roff, rindent) = scan(bpbuf)
        if (roff != -1) {
          val current = BracePair(-1, -1, roff, rindent, bpbuf.toList)
          bpbuf.clear()
          bpbuf += current
        }
      }

      def printBP(bp: BracePair, indent: Int) {
        println(" "*indent+line(bp.loff)+":"+bp.lindent+" to "+line(bp.roff)+":"+bp.rindent)
        if (bp.nested.nonEmpty)
          for (bp1 <- bp.nested) {
            printBP(bp1, indent + 2)
          }
      }
//      println("lineStart = "+lineStart)//DEBUG
//      println("bracepairs = ")
//      for (bp <- bpbuf.toList) printBP(bp, 0)
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
      if (offset <= 0) 0
      else findLine(0, lineStart.length - 1)
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
        case (bp @ BracePair(loff, lindent, roff, rindent, nested)) :: bps1 =>
          if (lindent <= rindent) insert(bps1)
          else {
//           println("patch inside "+bp+"/"+line(loff)+"/"+lineStart(line(loff))+"/"+lindent"/"+rindent)//DEBUG
            val patches1 = insert(nested)
            if (patches1 ne patches) patches1
            else {
              var lin = line(loff) + 1
              while (lin < lineStart.length && column(lineStart(lin)) > lindent)
                lin += 1
              if (lin < lineStart.length) {
                val patches1 = insertPatch(patches, BracePatch(lineStart(lin), true))
                //println("patch for "+bp+"/"+imbalanceMeasure+"/"+new ParensAnalyzer(unit, patches1).imbalanceMeasure)
                /*if (improves(patches1))*/
                patches1
                /*else insert(bps1)*/
                // (this test did not seem to work very well in practice)
              } else patches
            }
          }
      }
      insert(bracePairs)
    }

    def deleteRBrace(): List[BracePatch] = {
      def delete(bps: List[BracePair]): List[BracePatch] = bps match {
        case List() => patches
        case BracePair(loff, lindent, roff, rindent, nested) :: bps1 =>
          if (lindent >= rindent) delete(bps1)
          else {
            val patches1 = delete(nested)
            if (patches1 ne patches) patches1
            else insertPatch(patches, BracePatch(roff, false))
          }
      }
      delete(bracePairs)
    }

    def imbalanceMeasure: Int = {
      def measureList(bps: List[BracePair]): Int =
        (bps map measure).sum
      def measure(bp: BracePair): Int =
        (if (bp.lindent != bp.rindent) 1 else 0) + measureList(bp.nested)
      measureList(bracePairs)
    }

    def improves(patches1: List[BracePatch]): Boolean =
      imbalanceMeasure > new ParensAnalyzer(unit, patches1).imbalanceMeasure

    override def error(offset: Int, msg: String) {}
  }
}
