/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools.nsc
package ast.parser

import scala.tools.nsc.util.{ CharArrayReader, CharArrayReaderData }
import scala.reflect.internal.util._
import scala.reflect.internal.Chars._
import Tokens._
import scala.annotation.{ switch, tailrec }
import scala.collection.mutable
import mutable.{ ListBuffer, ArrayBuffer }
import scala.tools.nsc.ast.parser.xml.Utility.isNameStart
import scala.language.postfixOps

/** See Parsers.scala / ParsersCommon for some explanation of ScannersCommon.
 */
trait ScannersCommon {
  val global : Global
  import global._

  /** Offset into source character array */
  type Offset = Int

  type Token = Int

  trait CommonTokenData {
    def token: Token
    def name: TermName
  }

  trait ScannerCommon extends CommonTokenData {
    // things to fill in, in addition to buf, decodeUni which come from CharArrayReader
    def error(off: Offset, msg: String): Unit
    def incompleteInputError(off: Offset, msg: String): Unit
    def deprecationWarning(off: Offset, msg: String, since: String): Unit
  }

  def createKeywordArray(keywords: Seq[(Name, Token)], defaultToken: Token): (Token, Array[Token]) = {
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

  trait TokenData extends CommonTokenData {

    /** the next token */
    var token: Token = EMPTY

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

    def copyFrom(td: TokenData): this.type = {
      this.token = td.token
      this.offset = td.offset
      this.lastOffset = td.lastOffset
      this.name = td.name
      this.strVal = td.strVal
      this.base = td.base
      this
    }
  }

  /** An interface to most of mutable data in Scanner defined in TokenData
   *  and CharArrayReader (+ next, prev fields) with copyFrom functionality
   *  to backup/restore data (used by quasiquotes' lookingAhead).
   */
  trait ScannerData extends TokenData with CharArrayReaderData {
    /** we need one token lookahead and one token history
     */
    val next: TokenData = new TokenData{}
    val prev: TokenData = new TokenData{}

    def copyFrom(sd: ScannerData): this.type = {
      this.next copyFrom sd.next
      this.prev copyFrom sd.prev
      super[CharArrayReaderData].copyFrom(sd)
      super[TokenData].copyFrom(sd)
      this
    }
  }

  abstract class Scanner extends CharArrayReader with TokenData with ScannerData with ScannerCommon {
    private def isDigit(c: Char) = java.lang.Character isDigit c

    private var openComments = 0
    protected def putCommentChar(): Unit = nextChar()

    @tailrec private def skipLineComment(): Unit = ch match {
      case SU | CR | LF =>
      case _            => nextChar() ; skipLineComment()
    }
    private def maybeOpen(): Unit = {
      putCommentChar()
      if (ch == '*') {
        putCommentChar()
        openComments += 1
      }
    }
    private def maybeClose(): Boolean = {
      putCommentChar()
      (ch == '/') && {
        putCommentChar()
        openComments -= 1
        openComments == 0
      }
    }
    @tailrec final def skipNestedComments(): Unit = ch match {
      case '/' => maybeOpen() ; skipNestedComments()
      case '*' => if (!maybeClose()) skipNestedComments()
      case SU  => incompleteInputError("unclosed comment")
      case _   => putCommentChar() ; skipNestedComments()
    }
    def skipDocComment(): Unit = skipNestedComments()
    def skipBlockComment(): Unit = skipNestedComments()

    private def skipToCommentEnd(isLineComment: Boolean): Unit = {
      nextChar()
      if (isLineComment) skipLineComment()
      else {
        openComments = 1
        val isDocComment = (ch == '*') && { nextChar(); true }
        if (isDocComment) {
          // Check for the amazing corner case of /**/
          if (ch == '/')
            nextChar()
          else
            skipDocComment()
        }
        else skipBlockComment()
      }
    }

    /** @pre ch == '/'
     *  Returns true if a comment was skipped.
     */
    def skipComment(): Boolean = ch match {
      case '/' | '*' => skipToCommentEnd(isLineComment = ch == '/') ; true
      case _         => false
    }
    def flushDoc(): DocComment = null

    /** To prevent doc comments attached to expressions from leaking out of scope
     *  onto the next documentable entity, they are discarded upon passing a right
     *  brace, bracket, or parenthesis.
     */
    def discardDocBuffer(): Unit = ()

    def isAtEnd = charOffset >= buf.length

    def resume(lastCode: Token) = {
      token = lastCode
      if (next.token != EMPTY && !reporter.hasErrors)
        syntaxError("unexpected end of input: possible missing '}' in XML block")

      nextToken()
    }

    /** A character buffer for literals
     */
    val cbuf = new StringBuilder

    /** append Unicode character to "cbuf" buffer
     */
    protected def putChar(c: Char): Unit = {
//      assert(cbuf.size < 10000, cbuf)
      cbuf.append(c)
    }

    /** Determines whether this scanner should emit identifier deprecation warnings,
     *  e.g. when seeing `macro` or `then`, which are planned to become keywords in future versions of Scala.
     */
    protected def emitIdentifierDeprecationWarnings = true

    /** Clear buffer and set name and token */
    private def finishNamed(idtoken: Token = IDENTIFIER): Unit = {
      name = newTermName(cbuf.toString)
      cbuf.clear()
      token = idtoken
      if (idtoken == IDENTIFIER) {
        val idx = name.start - kwOffset
        if (idx >= 0 && idx < kwArray.length) {
          token = kwArray(idx)
          if (token == IDENTIFIER && allowIdent != name) {
            if (name == nme.MACROkw)
              syntaxError(s"$name is now a reserved word; usage as an identifier is disallowed")
            else if (emitIdentifierDeprecationWarnings)
              deprecationWarning(s"$name is a reserved word (since 2.10.0); usage as an identifier is deprecated", "2.10.0")
          }
        }
      }
    }

    /** Clear buffer and set string */
    private def setStrVal(): Unit = {
      strVal = cbuf.toString
      cbuf.clear()
    }

    /** a stack of tokens which indicates whether line-ends can be statement separators
     *  also used for keeping track of nesting levels.
     *  We keep track of the closing symbol of a region. This can be
     *  RPAREN    if region starts with '('
     *  RBRACKET  if region starts with '['
     *  RBRACE    if region starts with '{'
     *  ARROW     if region starts with 'case'
     *  STRINGLIT if region is a string interpolation expression starting with '${'
     *            (the STRINGLIT appears twice in succession on the stack iff the
     *             expression is a multiline string literal).
     */
    var sepRegions: List[Token] = List()

// Get next token ------------------------------------------------------------

    /** Are we directly in a string interpolation expression?
     */
    private def inStringInterpolation =
      sepRegions.nonEmpty && sepRegions.head == STRINGLIT

    /** Are we directly in a multiline string interpolation expression?
     *  @pre inStringInterpolation
     */
    private def inMultiLineInterpolation =
      inStringInterpolation && sepRegions.tail.nonEmpty && sepRegions.tail.head == STRINGPART

    /** read next token and return last offset
     */
    def skipToken(): Offset = {
      val off = offset
      nextToken()
      off
    }

    /** Allow an otherwise deprecated ident here */
    private var allowIdent: Name = nme.EMPTY

    /** Get next token, and allow the otherwise deprecated ident `name`  */
    def nextTokenAllow(name: Name) = {
      val prev = allowIdent
      allowIdent = name
      try {
        nextToken()
      } finally {
        allowIdent = prev
      }
    }

    /** Produce next token, filling TokenData fields of Scanner.
     */
    def nextToken(): Unit = {
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
          if (!sepRegions.isEmpty)
            sepRegions = sepRegions.tail

          discardDocBuffer()
        case RBRACKET | RPAREN =>
          if (!sepRegions.isEmpty && sepRegions.head == lastToken)
            sepRegions = sepRegions.tail

          discardDocBuffer()
        case ARROW =>
          if (!sepRegions.isEmpty && sepRegions.head == lastToken)
            sepRegions = sepRegions.tail
        case STRINGLIT =>
          if (inMultiLineInterpolation)
            sepRegions = sepRegions.tail.tail
          else if (inStringInterpolation)
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
        if(token == ERROR) {
          if (inMultiLineInterpolation)
            sepRegions = sepRegions.tail.tail
          else if (inStringInterpolation)
            sepRegions = sepRegions.tail
        }
      } else {
        this copyFrom next
        next.token = EMPTY
      }

      /* Insert NEWLINE or NEWLINES if
       * - we are after a newline
       * - we are within a { ... } or on toplevel (wrt sepRegions)
       * - the current token can start a statement and the one before can end it
       * insert NEWLINES if we are past a blank line, NEWLINE otherwise
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
        def resetOffset(): Unit = {
          offset = prev.offset
          lastOffset = prev.lastOffset
        }
        if (token == CLASS) {
          token = CASECLASS
          resetOffset()
        } else if (token == OBJECT) {
          token = CASEOBJECT
          resetOffset()
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
    protected final def fetchToken(): Unit = {
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
          if (ch == '"' && token == IDENTIFIER)
            token = INTERPOLATIONID
        case '<' => // is XMLSTART?
          def fetchLT() = {
            val last = if (charOffset >= 2) buf(charOffset - 2) else ' '
            nextChar()
            last match {
              case ' ' | '\t' | '\n' | '{' | '(' | '>' if isNameStart(ch) || ch == '!' || ch == '?' =>
                token = XMLSTART
              case _ =>
                // Console.println("found '<', but last is '"+in.last+"'"); // DEBUG
                putChar('<')
                getOperatorRest()
            }
          }
          fetchLT()
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
          def fetchLeadingZero(): Unit = {
            nextChar()
            ch match {
              case 'x' | 'X' => base = 16 ; nextChar()
              case _         => base = 8    // single decimal zero, perhaps
            }
          }
          fetchLeadingZero()
          getNumber()
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          base = 10
          getNumber()
        case '`' =>
          getBackquotedIdent()
        case '\"' =>
          def fetchDoubleQuote() = {
            if (token == INTERPOLATIONID) {
              nextRawChar()
              if (ch == '\"') {
                val lookahead = lookaheadReader
                lookahead.nextChar()
                if (lookahead.ch == '\"') {
                  nextRawChar()                        // now eat it
                  offset += 3
                  nextRawChar()
                  getStringPart(multiLine = true)
                  sepRegions = STRINGPART :: sepRegions // indicate string part
                  sepRegions = STRINGLIT :: sepRegions // once more to indicate multi line string part
                } else {
                  nextChar()
                  token = STRINGLIT
                  strVal = ""
                }
              } else {
                offset += 1
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
          }
          fetchDoubleQuote()
        case '\'' =>
          def fetchSingleQuote() = {
            nextChar()
            if (isIdentifierStart(ch))
              charLitOr(getIdentRest)
            else if (isOperatorPart(ch) && (ch != '\\'))
              charLitOr(getOperatorRest)
            else if (!isAtEnd && (ch != SU && ch != CR && ch != LF || isUnicodeEscape)) {
              getLitChar()
              if (ch == '\'') {
                nextChar()
                token = CHARLIT
                setStrVal()
              } else {
                syntaxError("unclosed character literal")
              }
            }
            else
              syntaxError("unclosed character literal")
          }
          fetchSingleQuote()
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
          if (isAtEnd) token = EOF
          else {
            syntaxError("illegal character")
            nextChar()
          }
        case _ =>
          def fetchOther() = {
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
              syntaxError("illegal character '" + ("" + '\\' + 'u' + "%04x".format(ch.toInt)) + "'")
              nextChar()
            }
          }
          fetchOther()
      }
    }

    /** Can token start a statement? */
    def inFirstOfStat(token: Token) = token match {
      case EOF | CATCH | ELSE | EXTENDS | FINALLY | FORSOME | MATCH | WITH | YIELD |
           COMMA | SEMI | NEWLINE | NEWLINES | DOT | COLON | EQUALS | ARROW | LARROW |
           SUBTYPE | VIEWBOUND | SUPERTYPE | HASH | RPAREN | RBRACKET | RBRACE | LBRACKET =>
        false
      case _ =>
        true
    }

    /** Can token end a statement? */
    def inLastOfStat(token: Token) = token match {
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT |
           IDENTIFIER | BACKQUOTED_IDENT | THIS | NULL | TRUE | FALSE | RETURN | USCORE |
           TYPE | XMLSTART | RPAREN | RBRACKET | RBRACE =>
        true
      case _ =>
        false
    }

// Identifiers ---------------------------------------------------------------

    private def getBackquotedIdent(): Unit = {
      nextChar()
      getLitChars('`')
      if (ch == '`') {
        nextChar()
        finishNamed(BACKQUOTED_IDENT)
        if (name.length == 0) syntaxError("empty quoted identifier")
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

    private def getIdentOrOperatorRest(): Unit = {
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
      } else unclosedStringLit()
    }

    private def unclosedStringLit(): Unit = syntaxError("unclosed string literal")

    @tailrec private def getRawStringLit(): Unit = {
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

    @tailrec private def getStringPart(multiLine: Boolean): Unit = {
      def finishStringPart() = {
        setStrVal()
        token = STRINGPART
        next.lastOffset = charOffset - 1
        next.offset = charOffset - 1
      }
      if (ch == '"') {
        if (multiLine) {
          nextRawChar()
          if (isTripleQuote()) {
            setStrVal()
            token = STRINGLIT
          } else
            getStringPart(multiLine)
        } else {
          nextChar()
          setStrVal()
          token = STRINGLIT
        }
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
        } else if (ch == '_') {
          finishStringPart()
          nextRawChar()
          next.token = USCORE
        } else if (Character.isUnicodeIdentifierStart(ch)) {
          finishStringPart()
          do {
            putChar(ch)
            nextRawChar()
          } while (ch != SU && Character.isUnicodeIdentifierPart(ch))
          next.token = IDENTIFIER
          next.name = newTermName(cbuf.toString)
          cbuf.clear()
          val idx = next.name.start - kwOffset
          if (idx >= 0 && idx < kwArray.length) {
            next.token = kwArray(idx)
          }
        } else {
          syntaxError("invalid string interpolation: `$$', `$'ident or `$'BlockExpr expected")
        }
      } else {
        val isUnclosedLiteral = !isUnicodeEscape && (ch == SU || (!multiLine && (ch == CR || ch == LF)))
        if (isUnclosedLiteral) {
          if (multiLine)
            incompleteInputError("unclosed multi-line string literal")
          else
            unclosedStringLit()
        }
        else {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine)
        }
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
          val start = charOffset - 2
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
          val alt = if (oct == LF) "\\n" else "\\u%04x" format oct
          def msg(what: String) = s"Octal escape literals are $what, use $alt instead."
          if (settings.future)
            syntaxError(start, msg("unsupported"))
          else
            deprecationWarning(start, msg("deprecated"), "2.11.0")
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

    private def getLitChars(delimiter: Char) = {
      while (ch != delimiter && !isAtEnd && (ch != SU && ch != CR && ch != LF || isUnicodeEscape))
        getLitChar()
    }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction(): Unit = {
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

    /** Convert current strVal, base to long value.
     *  This is tricky because of max negative value.
     *
     *  Conversions in base 10 and 16 are supported. As a permanent migration
     *  path, attempts to write base 8 literals except `0` emit a verbose error.
     */
    def intVal(negated: Boolean): Long = {
      def malformed: Long = {
        if (base == 8) syntaxError("Decimal integer literals may not have a leading zero. (Octal syntax is obsolete.)")
        else syntaxError("malformed integer number")
        0
      }
      def tooBig: Long = {
        syntaxError("integer number too large")
        0
      }
      def intConvert: Long = {
        val len = strVal.length
        if (len == 0) {
          if (base != 8) syntaxError("missing integer number")  // e.g., 0x;
          0
        } else {
          val divider     = if (base == 10) 1 else 2
          val limit: Long = if (token == LONGLIT) Long.MaxValue else Int.MaxValue
          @tailrec def convert(value: Long, i: Int): Long =
            if (i >= len) value
            else {
              val d = digit2int(strVal charAt i, base)
              if (d < 0)
                malformed
              else if (value < 0 ||
                  limit / (base / divider) < value ||
                  limit - (d / divider) < value * (base / divider) &&
                  !(negated && limit == value * base - 1 + d))
                tooBig
              else
                convert(value * base + d, i + 1)
            }
          val result = convert(0, 0)
          if (base == 8) malformed else if (negated) -result else result
        }
      }
      if (token == CHARLIT && !negated) charVal.toLong else intConvert
    }

    def intVal: Long = intVal(negated = false)

    /** Convert current strVal, base to double value
     */
    def floatVal(negated: Boolean): Double = {
      val limit: Double = if (token == DOUBLELIT) Double.MaxValue else Float.MaxValue
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

    def floatVal: Double = floatVal(negated = false)

    def checkNoLetter(): Unit = {
      if (isIdentifierPart(ch) && ch >= ' ')
        syntaxError("Invalid literal number")
    }

    /** Read a number into strVal.
     *
     *  The `base` can be 8, 10 or 16, where base 8 flags a leading zero.
     *  For ints, base 8 is legal only for the case of exactly one zero.
     */
    protected def getNumber(): Unit = {
      // consume digits of a radix
      def consumeDigits(radix: Int): Unit =
        while (digit2int(ch, radix) >= 0) {
          putChar(ch)
          nextChar()
        }
      // adding decimal point is always OK because `Double valueOf "0."` is OK
      def restOfNonIntegralNumber(): Unit = {
        putChar('.')
        if (ch == '.') nextChar()
        getFraction()
      }
      // after int: 5e7f, 42L, 42.toDouble but not 42b. Repair 0d.
      def restOfNumber(): Unit = {
        ch match {
          case 'e' | 'E' | 'f' | 'F' |
               'd' | 'D' => if (cbuf.isEmpty) putChar('0'); restOfNonIntegralNumber()
          case 'l' | 'L' => token = LONGLIT ; setStrVal() ; nextChar()
          case _         => token = INTLIT  ; setStrVal() ; checkNoLetter()
        }
      }

      // consume leading digits, provisionally an Int
      consumeDigits(if (base == 16) 16 else 10)

      val detectedFloat: Boolean = base != 16 && ch == '.' && isDigit(lookaheadReader.getc)
      if (detectedFloat) restOfNonIntegralNumber() else restOfNumber()
    }

    /** Parse character literal if current character is followed by \',
     *  or follow with given op and return a symbol literal token
     */
    def charLitOr(op: () => Unit): Unit = {
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

    /** generate an error at the given offset */
    def syntaxError(off: Offset, msg: String): Unit = {
      error(off, msg)
      token = ERROR
    }

    /** generate an error at the current token offset */
    def syntaxError(msg: String): Unit = syntaxError(offset, msg)

    def deprecationWarning(msg: String, since: String): Unit = deprecationWarning(offset, msg, since)

    /** signal an error where the input ended in the middle of a token */
    def incompleteInputError(msg: String): Unit = {
      incompleteInputError(offset, msg)
      token = EOF
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
    def parenBalance(token: Token) = 0

    /** overridden in UnitScanners */
    def healBraces(): List[BracePatch] = List()

    /** Initialization method: read first char, then first token
     */
    def init(): Unit = {
      nextChar()
      nextToken()
    }
  } // end Scanner

  // ------------- keyword configuration -----------------------------------

  private val allKeywords = List[(Name, Token)](
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
    nme.ATkw        -> AT,
    nme.MACROkw     -> IDENTIFIER,
    nme.THENkw      -> IDENTIFIER)

  private var kwOffset: Offset = -1
  private val kwArray: Array[Token] = {
    val (offset, arr) = createKeywordArray(allKeywords, IDENTIFIER)
    kwOffset = offset
    arr
  }

  final val token2name = (allKeywords map (_.swap)).toMap

// Token representation ----------------------------------------------------

  /** Returns the string representation of given token. */
  def token2string(token: Token): String = (token: @switch) match {
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

  class MalformedInput(val offset: Offset, val msg: String) extends Exception

  /** A scanner for a given source file not necessarily attached to a compilation unit.
   *  Useful for looking inside source files that aren not currently compiled to see what's there
   */
  class SourceFileScanner(val source: SourceFile) extends Scanner {
    val buf = source.content
    override val decodeUni: Boolean = !settings.nouescape

    // suppress warnings, throw exception on errors
    def deprecationWarning(off: Offset, msg: String, since: String): Unit = ()
    def error(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
    def incompleteInputError(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
  }

  /** A scanner over a given compilation unit
   */
  class UnitScanner(val unit: CompilationUnit, patches: List[BracePatch]) extends SourceFileScanner(unit.source) {
    def this(unit: CompilationUnit) = this(unit, List())

    override def deprecationWarning(off: Offset, msg: String, since: String) = currentRun.reporting.deprecationWarning(unit.position(off), msg, since)
    override def error(off: Offset, msg: String)                             = reporter.error(unit.position(off), msg)
    override def incompleteInputError(off: Offset, msg: String)              = currentRun.parsing.incompleteInputError(unit.position(off), msg)

    private var bracePatches: List[BracePatch] = patches

    lazy val parensAnalyzer = new ParensAnalyzer(unit, List())

    override def parenBalance(token: Token) = parensAnalyzer.balance(token)

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
    val balance = mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

    /** The source code with braces and line starts annotated with [NN] showing the index */
    private def markedSource = {
      val code   = unit.source.content
      val braces = code.indices filter (idx => "{}\n" contains code(idx)) toSet;
      val mapped = code.indices map (idx => if (braces(idx)) s"${code(idx)}[$idx]" else "" + code(idx))
      mapped.mkString("")
    }

    init()
    log(s"ParensAnalyzer for ${unit.source} of length ${unit.source.content.length}\n```\n$markedSource\n```")

    /** The offset of the first token on this line, or next following line if blank
     */
    val lineStart = new ArrayBuffer[Int]

    /** The list of matching top-level brace pairs (each of which may contain nested brace pairs).
     */
    val bracePairs: List[BracePair] = {

      var lineCount = 1
      var lastOffset = 0
      var indent = 0
      val oldBalance = scala.collection.mutable.Map[Int, Int]()
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
      def bracePairString(bp: BracePair, indent: Int): String = {
        val rangeString = {
          import bp._
          val lline = line(loff)
          val rline = line(roff)
          val tokens = List(lline, lindent, rline, rindent) map (n => if (n < 0) "??" else "" + n)
          "%s:%s to %s:%s".format(tokens: _*)
        }
        val outer  = (" " * indent) + rangeString
        val inners = bp.nested map (bracePairString(_, indent + 2))

        if (inners.isEmpty) outer
        else inners.mkString(outer + "\n", "\n", "")
      }
      def bpString    = bpbuf.toList map ("\n" + bracePairString(_, 0)) mkString ""
      def startString = lineStart.mkString("line starts: [", ", ", "]")

      log(s"\n$startString\n$bpString")
      bpbuf.toList
    }

    var tabSeen = false

    def line(offset: Offset): Int = {
      def findLine(lo: Int, hi: Int): Int = {
        val mid = (lo + hi) / 2
        if (offset < lineStart(mid)) findLine(lo, mid - 1)
        else if (mid + 1 < lineStart.length && offset >= lineStart(mid + 1)) findLine(mid + 1, hi)
        else mid
      }
      if (offset <= 0) 0
      else findLine(0, lineStart.length - 1)
    }

    def column(offset: Offset): Int = {
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
                val patches1 = insertPatch(patches, BracePatch(lineStart(lin), inserted = true))
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
            else insertPatch(patches, BracePatch(roff, inserted = false))
          }
      }
      delete(bracePairs)
    }

    // don't emit deprecation warnings about identifiers like `macro` or `then`
    // when skimming through the source file trying to heal braces
    override def emitIdentifierDeprecationWarnings = false

    override def error(offset: Offset, msg: String): Unit = ()
  }
}
