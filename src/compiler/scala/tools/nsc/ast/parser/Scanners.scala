/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package ast.parser

import scala.annotation.{switch, tailrec}
import scala.collection.mutable, mutable.{ArrayBuffer, ListBuffer}
import scala.reflect.internal.Chars._
import scala.reflect.internal.util._
import scala.tools.nsc.Reporting.WarningCategory, WarningCategory.Scala3Migration
import scala.tools.nsc.ast.parser.xml.Utility.isNameStart
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.util.{CharArrayReader, CharArrayReaderData}
import Tokens._
import java.lang.StringBuilder

object Cbuf {
  final val TargetCapacity = 256

  def create(): StringBuilder = new StringBuilder(TargetCapacity)

  implicit class StringBuilderOps(val sb: StringBuilder) extends AnyVal {
    def clear(): Unit = {
      if (sb.capacity() > TargetCapacity) {
        sb.setLength(TargetCapacity)
        sb.trimToSize()
      }
      sb.setLength(0)
    }
    def toCharArray: Array[Char] = {
      val n = sb.length()
      val res = new Array[Char](n)
      sb.getChars(0, n, res, 0)
      res
    }
    def isEmpty = sb.length() == 0
    def last = sb.charAt(sb.length() - 1)
  }
}

import Cbuf.StringBuilderOps

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
    /** Consume and discard the next token. */
    def nextToken(): Unit

    // things to fill in, in addition to buf, decodeUni which come from CharArrayReader
    def error(off: Offset, msg: String): Unit
    def incompleteInputError(off: Offset, msg: String): Unit
    def warning(off: Offset, msg: String, category: WarningCategory): Unit
    def deprecationWarning(off: Offset, msg: String, since: String, actions: List[CodeAction] = Nil): Unit

    // advance past COMMA NEWLINE RBRACE (to whichever token is the matching close bracket)
    def skipTrailingComma(right: Token): Boolean = false
  }

  // Hooks for ScaladocUnitScanner and ScaladocJavaUnitScanner
  trait DocScanner {
    protected def beginDocComment(prefix: String): Unit = {}
    protected def processCommentChar(): Unit = {}
    protected def finishDocComment(): Unit = {}

    private var lastDoc: DocComment = null
    // get last doc comment
    def flushDoc(): DocComment = try lastDoc finally lastDoc = null
    def registerDocComment(raw: String, pos: Position) = {
      lastDoc = DocComment(raw, pos)
      signalParsedDocComment(raw, pos)
    }

    /** To prevent doc comments attached to expressions from leaking out of scope
      *  onto the next documentable entity, they are discarded upon passing a right
      *  brace, bracket, or parenthesis.
      */
    def discardDocBuffer(): Unit = {}
  }

  def createKeywordArray(keywords: Seq[(Name, Token)], defaultToken: Token): (Token, Array[Token]) = {
    val names = keywords.sortBy(_._1.start).map { case (k, v) => (k.start, v) }
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

  abstract class Scanner extends CharArrayReader with TokenData with ScannerData with ScannerCommon with DocScanner {
    def unit: CompilationUnit

    /** A switch whether operators at the start of lines can be infix operators. */
    private var allowLeadingInfixOperators = true

    private def isDigit(c: Char) = Character.isDigit(c)

    import Character.{isHighSurrogate, isLowSurrogate, isUnicodeIdentifierPart, isUnicodeIdentifierStart, isValidCodePoint, toCodePoint}

    // given char (ch) is high surrogate followed by low, codepoint passes predicate.
    // true means supplementary chars were put to buffer.
    // strict to require low surrogate (if not in string literal).
    private def isSupplementary(high: Char, test: Int => Boolean, strict: Boolean = true): Boolean =
      isHighSurrogate(high) && {
        var res = false
        val low = lookaheadReader.getc()
        if (isLowSurrogate(low)) {
          val codePoint = toCodePoint(high, low)
          if (isValidCodePoint(codePoint)) {
            if (test(codePoint)) {
              putChar(high)
              putChar(low)
              nextChar()
              nextChar()
              res = true
            }
          }
          else syntaxError(f"illegal character '\\u$high%04x\\u$low%04x'")
        }
        else if (!strict) {
          putChar(high)
          nextChar()
          res = true
        }
        else syntaxError(f"illegal character '\\u$high%04x' missing low surrogate")
        res
      }
    private def atSupplementary(ch: Char, f: Int => Boolean): Boolean =
      isHighSurrogate(ch) && {
        val hi = ch
        val r = lookaheadReader
        r.nextRawChar()
        val lo = r.ch
        isLowSurrogate(lo) && {
          val codepoint = toCodePoint(hi, lo)
          isValidCodePoint(codepoint) && f(codepoint)
        }
      }

    private var openComments = 0
    final protected def putCommentChar(): Unit = { processCommentChar(); nextChar() }

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
          else {
            beginDocComment("/**")
            skipNestedComments()
          }
        }
        else skipNestedComments()
      }
    }

    /** Returns true if a comment was skipped.
     *  @note Pre-condition: ch == '/'
     */
    final def skipComment(): Boolean = ch match {
      case '/' | '*' => skipToCommentEnd(isLineComment = ch == '/') ; finishDocComment(); true
      case _         => false
    }


    def isAtEnd = charOffset >= buf.length

    def resume(lastCode: Token) = {
      token = lastCode
      if (next.token != EMPTY && !reporter.hasErrors)
        syntaxError("unexpected end of input: possible missing '}' in XML block")

      nextToken()
    }

    /** A character buffer for literals
     */
    val cbuf = Cbuf.create()

    /** append Unicode character to "cbuf" buffer
     */
    protected def putChar(c: Char): Unit = cbuf.append(c)

    /** Determines whether this scanner should emit identifier deprecation warnings,
     *  e.g. when seeing `macro` or `then`, which are planned to become keywords in future versions of Scala.
     */
    protected def emitIdentifierDeprecationWarnings = true

    /** Clear buffer and set name and token */
    private def finishNamed(idtoken: Token = IDENTIFIER): Unit = {
      name = newTermName(cbuf.toCharArray)
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

    /** Are we in a `${ }` block? such that RBRACE exits back into multiline string. */
    private def inMultiLineInterpolatedExpression = {
      sepRegions match {
        case RBRACE :: STRINGLIT :: STRINGPART :: _ => true
        case _ => false
      }
    }

    def lookingAhead[A](body: => A): A = {
      val saved = new ScannerData {} copyFrom this
      val aLIO = allowLeadingInfixOperators
      allowLeadingInfixOperators = false
      nextToken()
      try body finally {
        this copyFrom saved
        allowLeadingInfixOperators = aLIO
      }
    }

    /** read next token and return last offset
     */
    def skipToken(): Offset = {
      val off = offset
      nextToken()
      off
    }

    // used by parser to distinguish pattern P(_*, p) from trailing comma.
    // EOF is accepted for REPL, which can't look ahead past the current line.
    def isTrailingComma(right: Token): Boolean =
      token == COMMA && lookingAhead(afterLineEnd() && token == right || token == EOF)

    override def skipTrailingComma(right: Token): Boolean =
      if (token == COMMA) {
        // SIP-27 Trailing Comma (multi-line only) support
        // If a comma is followed by a new line & then a closing paren, bracket or brace
        // then it is a trailing comma and is ignored
        val saved = new ScannerData {} copyFrom this
        fetchToken()
        (afterLineEnd() && token == right || token == EOF) || { copyFrom(saved) ; false }
      } else false

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

    // Adapt sepRegions according to last token
    def adjustSepRegions(lastToken: Token): Unit = (lastToken: @switch) match {
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

    /** Advance beyond a case token without marking the CASE in sepRegions.
     *  This method should be called to skip beyond CASE tokens that are
     *  not part of matches, i.e. no ARROW is expected after them.
     */
    def skipCASE(): Unit = {
      assert(token == CASE, s"Internal error: skipCASE() called on non-case token $token")
      nextToken()
      sepRegions = sepRegions.tail
    }

    /** True to warn about migration change in infix syntax. */
    private val infixMigration = settings.Xmigration.value <= ScalaVersion("2.13.2")

    /** Produce next token, filling TokenData fields of Scanner.
     */
    def nextToken(): Unit = {
      val lastToken = token
      adjustSepRegions(lastToken)

      // Read a token or copy it from `next` tokenData
      if (next.token == EMPTY) {
        lastOffset = charOffset - 1
        if (lastOffset > 0 && buf(lastOffset) == '\n' && buf(lastOffset - 1) == '\r') {
          lastOffset -= 1
        }
        if (inStringInterpolation) fetchStringPart() else fetchToken()
        if (token == ERROR) {
          if (inMultiLineInterpolation)
            sepRegions = sepRegions.tail.tail
          else if (inStringInterpolation)
            sepRegions = sepRegions.tail
        }
      } else {
        this copyFrom next
        next.token = EMPTY
      }

      def isSimpleExprIntroToken(token: Token): Boolean = token match {
        case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
             STRINGLIT | INTERPOLATIONID | SYMBOLLIT | TRUE | FALSE | NULL | // literals
             IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER | NEW | USCORE |
             LPAREN | LBRACE | XMLSTART => true
        case _ => false
      }

      def insertNL(nl: Token): Unit = {
        next.copyFrom(this)
        //  todo: make offset line-end of previous line?
        offset = if (lineStartOffset <= offset) lineStartOffset else lastLineStartOffset
        token = nl
      }

      def isOperator: Boolean = token == BACKQUOTED_IDENT || token == IDENTIFIER && isOperatorPart(name.charAt(name.length - 1))

      /* A leading infix operator must be followed by a lexically suitable expression.
       * Usually any simple expr will do. However, a backquoted identifier may serve as
       * either an op or a reference. So the additional constraint is that the following
       * token can't be an assignment operator. (Dotty disallows binary ops, hence the
       * test for unary.) See run/multiLineOps.scala for 42 + `x` on 3 lines, where +
       * is not leading infix because backquoted x is non-unary op.
       */
      def followedByInfixRHS: Boolean = {
        //def isCandidateInfixRHS: Boolean = isSimpleExprIntroToken(token) && (!isOperator || nme.raw.isUnary(name) || token == BACKQUOTED_IDENT)
        def isAssignmentOperator: Boolean =
          name.endsWith('=') && !name.startsWith('=') && isOperatorPart(name.startChar) &&
          (name.length != 2 || (name.startChar match { case '!' | '<' | '>' => false case _ => true }))
        def isCandidateInfixRHS: Boolean = isSimpleExprIntroToken(token) && (!isOperator || token == BACKQUOTED_IDENT || !isAssignmentOperator)
        lookingAhead {
          isCandidateInfixRHS || token == NEWLINE && { nextToken() ; isCandidateInfixRHS }
        }
      }

      /* A leading symbolic or backquoted identifier is treated as an infix operator
       * if it is followed by at least one ' ' and a token on the same line
       * that can start an expression.
       */
      def isLeadingInfixOperator =
        allowLeadingInfixOperators &&
        isOperator &&
        (isWhitespace(ch) || ch == LF) &&
        followedByInfixRHS

      /* Insert NEWLINE or NEWLINES if
       * - we are after a newline
       * - we are within a { ... } or on toplevel (wrt sepRegions)
       * - the current token can start a statement and the one before can end it
       * insert NEWLINES if we are past a blank line, NEWLINE otherwise
       */
      if (!applyBracePatch() && afterLineEnd() && inLastOfStat(lastToken) && inFirstOfStat(token) &&
          (sepRegions.isEmpty || sepRegions.head == RBRACE)) {
        if (pastBlankLine()) insertNL(NEWLINES)
        else if (!isLeadingInfixOperator) insertNL(NEWLINE)
        else if (!currentRun.sourceFeatures.leadingInfix) {
          val msg =
            sm"""Lines starting with an operator are taken as an infix expression continued from the previous line in Scala 3 (or with -Xsource-features:leading-infix).
                |To force the current interpretation as a separate statement, add an explicit `;`, add an empty line, or remove spaces after the operator."""
          if (currentRun.isScala3) warning(offset, msg, Scala3Migration)
          else if (infixMigration) deprecationWarning(msg, "2.13.2")
          insertNL(NEWLINE)
        }
      }
      postProcessToken()
//      print("["+this+"]")
    } // end nextToken

    // Join CASE + CLASS => CASECLASS, CASE + OBJECT => CASEOBJECT, SEMI + ELSE => ELSE
    def postProcessToken(): Unit =
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
    @tailrec
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
          if (ch == '"' && token == IDENTIFIER) token = INTERPOLATIONID
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
              case 'b' | 'B' => base = 2  ; nextChar()
              case _         => base = 10 ; putChar('0')
            }
            if (base != 10 && !isNumberSeparator(ch) && digit2int(ch, base) < 0)
              syntaxError("invalid literal number")
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
          def unclosedCharLit() = {
            val unclosed = "unclosed character literal"
            // advise if previous token was Symbol contiguous with the orphan single quote at offset
            val msg = {
              val maybeMistakenQuote =
                this match {
                  case sfs: SourceFileScanner =>
                    val wholeLine = sfs.source.lineToString(sfs.source.offsetToLine(offset))
                    wholeLine.count(_ == '\'') > 1
                  case _ => false
                }
              if (token == SYMBOLLIT && offset == lastOffset) s"""$unclosed (or use " for string literal "$strVal")"""
              else if (maybeMistakenQuote) s"""$unclosed (or use " not ' for string literal)"""
              else unclosed
            }
            syntaxError(msg)
          }
          /** Either at closing quote of charlit
           *  or run the op and take it as a (deprecated) Symbol identifier.
           */
          def charLitOrSymbolAfter(op: () => Unit): Unit =
            if (ch == '\'') {
              nextChar()
              token = CHARLIT
              setStrVal()
            } else {
              op()
              token = SYMBOLLIT
              strVal = name.toString
            }
          def fetchSingleQuote() = {
            nextChar()
            if (isIdentifierStart(ch)) {
              putChar(ch)
              nextChar()
              charLitOrSymbolAfter(() => getIdentRest())
            }
            else if (isOperatorPart(ch) && (ch != '\\')) {
              putChar(ch)
              nextChar()
              charLitOrSymbolAfter(() => getOperatorRest())
            }
            else if (!isAtEnd && (ch != SU && ch != CR && ch != LF)) {
              val isEmptyCharLit = (ch == '\'')
              getLitChar()
              if (ch == '\'') {
                if (isEmptyCharLit)
                  syntaxError("empty character literal (use '\\'' for single quote)")
                else {
                  nextChar()
                  if (cbuf.length != 1)
                    syntaxError("illegal codepoint in Char constant: " + cbuf.toString.map(c => f"\\u$c%04x").mkString("'", "", "'"))
                  else {
                    token = CHARLIT
                    setStrVal()
                  }
                }
              }
              else if (isEmptyCharLit)
                syntaxError("empty character literal")
              else
                unclosedCharLit()
            }
            else unclosedCharLit()
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
          if (inMultiLineInterpolatedExpression) nextRawChar() else nextChar()
          token = RBRACE
        case '[' =>
          nextChar(); token = LBRACKET
        case ']' =>
          nextChar(); token = RBRACKET
        case SU =>
          if (isAtEnd) {
            bidiChars.foreach { case (char, offset) =>
              syntaxError(offset, f"found unicode bidirectional character '\\u$char%04x'; in a string or character literal, use a unicode escape instead")
            }
            token = EOF
          }
          else {
            syntaxError("illegal character")
            nextChar()
          }
        case _ =>
          def fetchOther() = {
            if (ch == '\u21D2') {
              val msg = "The unicode arrow `⇒` is deprecated, use `=>` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code."
              deprecationWarning(msg, "2.13.0",
                runReporting.codeAction("replace unicode arrow", unit.position(offset).withEnd(offset + 1), "=>", msg, expected = Some(("⇒", unit))))
              nextChar(); token = ARROW
            } else if (ch == '\u2190') {
              val msg = "The unicode arrow `←` is deprecated, use `<-` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code."
              deprecationWarning(msg, "2.13.0",
                runReporting.codeAction("replace unicode arrow", unit.position(offset).withEnd(offset + 1), "<-", msg, expected = Some(("←", unit))))
              nextChar(); token = LARROW
            } else if (isUnicodeIdentifierStart(ch)) {
              putChar(ch)
              nextChar()
              getIdentRest()
              if (ch == '"' && token == IDENTIFIER) token = INTERPOLATIONID
            } else if (isSpecial(ch)) {
              putChar(ch)
              nextChar()
              getOperatorRest()
            } else if (isSupplementary(ch, isUnicodeIdentifierStart)) {
              getIdentRest()
              if (ch == '"' && token == IDENTIFIER) token = INTERPOLATIONID
            } else if (isSupplementary(ch, isSpecial)) {
              getOperatorRest()
            } else {
              syntaxError(f"illegal character '\\u$ch%04x'")
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

    @tailrec
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
      case ' ' | LF |   // optimize for common whitespace
           SU =>        // strangely enough, Character.isUnicodeIdentifierPart(SU) returns true!
        finishNamed()
      case _ =>
        if (isUnicodeIdentifierPart(ch)) {
          putChar(ch)
          nextChar()
          getIdentRest()
        }
        else if (isSupplementary(ch, isUnicodeIdentifierPart))
          getIdentRest()
        else
          finishNamed()
    }

    @tailrec
    private def getOperatorRest(): Unit = (ch: @switch) match {
      case ' ' | LF   => finishNamed()          // optimize
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
        else if (isSupplementary(ch, isSpecial)) getOperatorRest()
        else finishNamed()
    }

    private def getIdentOrOperatorRest(): Unit =
      if (isIdentifierPart(ch) || isSupplementary(ch, isIdentifierPart)) getIdentRest() else getOperatorRest()

// Literals -----------------------------------------------------------------

    private def getStringLit() = {
      getLitChars('"')
      if (ch == '"') {
        setStrVal()
        nextChar()
        token = STRINGLIT
      } else unclosedStringLit()
    }

    private def unclosedStringLit(seenEscapedQuoteInInterpolation: Boolean = false): Unit = {
      val note =
        if (seenEscapedQuoteInInterpolation) "; note that `\\\"` no longer closes single-quoted interpolated string literals since 2.13.6, you can use a triple-quoted string instead"
        else ""
      syntaxError(s"unclosed string literal$note")
    }

    private def replaceUnicodeEscapesInTriple(): Unit = 
      if (strVal != null)
        try {
          val processed = StringContext.processUnicode(strVal)
          if (processed != strVal && !currentRun.sourceFeatures.unicodeEscapesRaw) {
            val diffPosition = processed.zip(strVal).zipWithIndex.collectFirst { case ((r, o), i) if r != o => i }.getOrElse(processed.length - 1)
            val pos = offset + 3 + diffPosition
            def msg(what: String) = s"Unicode escapes in triple quoted strings are $what; use the literal character instead"
            if (currentRun.isScala3)
              warning(pos, msg("ignored in Scala 3 (or with -Xsource-features:unicode-escapes-raw)"), WarningCategory.Scala3Migration)
            else
              deprecationWarning(pos, msg("deprecated"), since="2.13.2")
            strVal = processed
          }
        } catch {
          case ue: StringContext.InvalidUnicodeEscapeException =>
            if (!currentRun.sourceFeatures.unicodeEscapesRaw)
              syntaxError(offset + 3 + ue.index, ue.getMessage())
        }

    @tailrec private def getRawStringLit(): Unit = {
      if (ch == '\"') {
        nextRawChar()
        if (isTripleQuote()) {
          setStrVal()
          replaceUnicodeEscapesInTriple()
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

    // for interpolated strings
    @tailrec private def getStringPart(multiLine: Boolean, seenEscapedQuote: Boolean = false): Unit = {
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
            getStringPart(multiLine, seenEscapedQuote)
        } else {
          nextChar()
          setStrVal()
          token = STRINGLIT
        }
      } else if (ch == '\\' && !multiLine) {
        putChar(ch)
        nextRawChar()
        val q = ch == '"'
        if (q || ch == '\\') {
          putChar(ch)
          nextRawChar()
        }
        getStringPart(multiLine, seenEscapedQuote || q)
      } else if (ch == '$') {
        @tailrec def getInterpolatedIdentRest(): Unit =
          if (ch != SU && isUnicodeIdentifierPart(ch)) {
            putChar(ch)
            nextRawChar()
            getInterpolatedIdentRest()
          } else if (atSupplementary(ch, isUnicodeIdentifierPart)) {
            putChar(ch)
            nextRawChar()
            putChar(ch)
            nextRawChar()
            getInterpolatedIdentRest()
          } else {
            next.token = IDENTIFIER
            next.name = newTermName(cbuf.toCharArray)
            cbuf.clear()
            val idx = next.name.start - kwOffset
            if (idx >= 0 && idx < kwArray.length)
              next.token = kwArray(idx)
          }
        nextRawChar()
        if (ch == '$' || ch == '"') {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine, seenEscapedQuote)
        } else if (ch == '{') {
          finishStringPart()
          nextRawChar()
          next.token = LBRACE
        } else if (ch == '_') {
          finishStringPart()
          nextRawChar()
          next.token = USCORE
        } else if (isUnicodeIdentifierStart(ch)) {
          finishStringPart()
          putChar(ch)
          nextRawChar()
          getInterpolatedIdentRest()
        } else if (atSupplementary(ch, isUnicodeIdentifierStart)) {
          finishStringPart()
          getInterpolatedIdentRest()
        } else {
          val expectations = "$$, $\", $identifier or ${expression}"
          syntaxError(charOffset - 2, s"invalid string interpolation $$$ch, expected: $expectations")
          putChar('$')
          getStringPart(multiLine, seenEscapedQuote)  // consume rest of interpolation, taking $ as literal
        }
      } else {
        val isUnclosedLiteral = (ch == SU || (!multiLine && (ch == CR || ch == LF)))
        if (isUnclosedLiteral)
          if (multiLine)
            incompleteInputError("unclosed multi-line string literal")
          else
            unclosedStringLit(seenEscapedQuote)
        else {
          putChar(ch)
          nextRawChar()
          getStringPart(multiLine, seenEscapedQuote)
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

    /** Copy current character into cbuf, interpreting any escape sequences,
     *  and advance to next character. Surrogate pairs are consumed (see check
     *  at fetchSingleQuote), but orphan surrogate is allowed.
     */
    protected def getLitChar(): Unit =
      if (ch == '\\') {
        nextChar()
        charEscape()
      } else if (!isSupplementary(ch, _ => true, strict = false)) {
        putChar(ch)
        nextChar()
      }

    private def charEscape(): Unit = {
      var bump = true
      ch match {
        case 'b'  => putChar('\b')
        case 't'  => putChar('\t')
        case 'n'  => putChar('\n')
        case 'f'  => putChar('\f')
        case 'r'  => putChar('\r')
        case '\"' => putChar('\"')
        case '\'' => putChar('\'')
        case '\\' => putChar('\\')
        case 'u'  => bump = uEscape()
        case x if '0' <= x && x <= '7' => bump = octalEscape()
        case _    => invalidEscape()
      }
      if (bump) nextChar()
    }

    private def uEscape(): Boolean = {
      while (ch == 'u') nextChar()
      var codepoint = 0
      var digitsRead = 0
      while (digitsRead < 4) {
        if (digitsRead > 0) nextChar()
        val digit = digit2int(ch, 16)
        digitsRead += 1
        if (digit >= 0) {
          codepoint = codepoint << 4
          codepoint += digit
        }
        else {
          invalidUnicodeEscape(digitsRead)
          return false
        }
      }
      val found = codepoint.asInstanceOf[Char]
      putChar(found)
      true
    }

    private def octalEscape(): Boolean = {
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
      val alt = if (oct == LF) "\\n" else f"\\u$oct%04x"
      syntaxError(start, s"octal escape literals are unsupported: use $alt instead")
      putChar(oct.toChar)
      false
    }

    protected def invalidEscape(): Unit = {
      syntaxError(charOffset - 1, "invalid escape character")
      putChar(ch)
    }

    protected def invalidUnicodeEscape(n: Int): Unit = {
      syntaxError(charOffset - n, "invalid unicode escape")
      putChar(ch)
    }

    private def getLitChars(delimiter: Char) = {
      while (ch != delimiter && !isAtEnd && (ch != SU && ch != CR && ch != LF))
        getLitChar()
    }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction(): Unit = {
      while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
        putChar(ch)
        nextChar()
      }
      checkNoTrailingSeparator()
      if (ch == 'e' || ch == 'E') {
        val lookahead = lookaheadReader
        lookahead.nextChar()
        if (lookahead.ch == '+' || lookahead.ch == '-') {
          lookahead.nextChar()
        }
        if ('0' <= lookahead.ch && lookahead.ch <= '9' || isNumberSeparator(lookahead.ch)) {
          putChar(ch)
          nextChar()
          if (ch == '+' || ch == '-') {
            putChar(ch)
            nextChar()
          }
          if (isNumberSeparator(ch))
            syntaxError(offset + cbuf.length, "illegal separator")
          while ('0' <= ch && ch <= '9' || isNumberSeparator(ch)) {
            putChar(ch)
            nextChar()
          }
          checkNoTrailingSeparator()
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
      } else
        token = DOUBLELIT
      checkNoLetter()
      setStrVal()
    }

    /** Convert current strVal to char value.
     */
    def charVal: Char = if (!strVal.isEmpty) strVal.charAt(0) else 0

    /** Convert current strVal, base to long value.
     *  This is tricky because of max negative value.
     *
     *  Conversions in base 2, 10 and 16 are supported.
     *  Number separators are skipped on the fly.
     */
    def intVal(negated: Boolean): Long = {
      def intConvert: Long = {
        def convertIt: Long = {
          def malformed: Long = { syntaxError("malformed integer number") ; 0 }
          def tooBig: Long = { syntaxError("integer number too large") ; 0 }
          val divider     = if (base == 10) 1 else 2
          val limit: Long = if (token == LONGLIT) Long.MaxValue else Int.MaxValue
          @tailrec def convert(value: Long, i: Int): Long =
            if (i >= strVal.length) value
            else {
              val c = strVal.charAt(i)
              if (isNumberSeparator(c)) convert(value, i + 1)
              else {
                val d = digit2int(c, base)
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
            }
          val result = convert(0, 0)
          if (negated) -result else result
        }
        if (strVal.isEmpty) {
          syntaxError("missing integer number") // e.g., 0x; previous error shadows this one
          0L
        } else {
          if (settings.warnOctalLiteral.value && base == 10 && strVal.charAt(0) == '0' && strVal.length() > 1)
            deprecationWarning("Decimal integer literals should not have a leading zero. (Octal syntax is obsolete.)", since="2.10")
          convertIt
        }
      }
      if (token == CHARLIT && !negated) charVal.toLong else intConvert
    }

    @`inline` def intVal: Long = intVal(negated = false)

    private val zeroFloat = raw"[0.]+(?:[eE][+-]?[0-9]+)?[fFdD]?".r

    /** Convert current strVal, base to float value.
     */
    def floatVal(negated: Boolean): Float = {
      val text = removeNumberSeparators(strVal)
      try {
        val value: Float = java.lang.Float.parseFloat(text)
        if (value > Float.MaxValue)
          syntaxError("floating point number too large")
        if (value == 0.0f && !zeroFloat.pattern.matcher(text).matches)
          syntaxError("floating point number too small")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed floating point number")
          0.0f
      }
    }

    @`inline` def floatVal: Float = floatVal(negated = false)

    /** Convert current strVal, base to double value.
     */
    def doubleVal(negated: Boolean): Double = {
      val text = removeNumberSeparators(strVal)
      try {
        val value: Double = java.lang.Double.parseDouble(text)
        if (value > Double.MaxValue)
          syntaxError("double precision floating point number too large")
        if (value == 0.0d && !zeroFloat.pattern.matcher(text).matches)
          syntaxError("double precision floating point number too small")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed double precision floating point number")
          0.0
      }
    }

    @`inline` def doubleVal: Double = doubleVal(negated = false)

    @`inline` def checkNoLetter(): Unit = if (isIdentifierPart(ch) && ch >= ' ') syntaxError("invalid literal number")

    @`inline` private def isNumberSeparator(c: Char): Boolean = c == '_'

    @`inline` private def removeNumberSeparators(s: String): String = if (s.indexOf('_') > 0) s.replace("_", "") else s

    @`inline` private def numberOffset = offset + (if (base == 10) 0 else 2)

    // disallow trailing numeric separator char
    def checkNoTrailingSeparator(): Unit =
      if (!cbuf.isEmpty && isNumberSeparator(cbuf.last))
        syntaxError(numberOffset + cbuf.length - 1, "illegal separator")

    /** Read a number into strVal.
     *
     *  The `base` can be 2, 10 or 16.
     */
    protected def getNumber(): Unit = {
      // consume digits of the current radix
      def consumeDigits(): Unit =
        while (isNumberSeparator(ch) || digit2int(ch, base) >= 0) {
          putChar(ch)
          nextChar()
        }
      // at dot with digit following
      def restOfNonIntegralNumber(): Unit = {
        putChar('.')
        nextChar()
        getFraction()
      }
      // 1l is an acknowledged bad practice
      def lintel(): Unit = {
        val msg = "Lowercase el for long is not recommended because it is easy to confuse with numeral 1; use uppercase L instead"
        val o = numberOffset + cbuf.length
        if (ch == 'l') deprecationWarning(o, msg, since="2.13.0",
          runReporting.codeAction("use uppercase L", unit.position(o).withEnd(o + 1), "L", msg, expected = Some(("l", unit))))
      }
      // after int: 5e7f, 42L, 42.toDouble but not 42b.
      def restOfNumber(): Unit = {
        ch match {
          case 'e' | 'E' | 'f' | 'F' |
               'd' | 'D' => getFraction()
          case 'l' | 'L' => lintel() ; token = LONGLIT ; setStrVal() ; nextChar()
          case _         => token = INTLIT  ; setStrVal() ; checkNoLetter()
        }
      }

      // consume leading digits, provisionally an Int
      consumeDigits()

      checkNoTrailingSeparator()

      val detectedFloat: Boolean = base == 10 && ch == '.' && isDigit(lookaheadReader.getc())
      if (detectedFloat) restOfNonIntegralNumber() else restOfNumber()
    }

// Errors -----------------------------------------------------------------

    /** generate an error at the given offset */
    def syntaxError(off: Offset, msg: String): Unit = {
      error(off, msg)
      token = ERROR
    }

    /** generate an error at the current token offset */
    def syntaxError(msg: String): Unit = syntaxError(offset, msg)

    def deprecationWarning(msg: String, since: String): Unit = deprecationWarning(msg, since, Nil)
    def deprecationWarning(msg: String, since: String, actions: List[CodeAction]): Unit = deprecationWarning(offset, msg, since, actions)

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
    )

  private var kwOffset: Offset = -1
  private val kwArray: Array[Token] = {
    val (offset, arr) = createKeywordArray(allKeywords, IDENTIFIER)
    kwOffset = offset
    arr
  }

  final val token2name = (allKeywords map (_.swap)).toMap

  final val softModifierNames = Set(nme.open, nme.infix)

  final val scala3Keywords = Set(nme.`enum`, nme.`export`, nme.`given`, nme.`then`)

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
    case XMLSTART => s"$$XMLSTART$$<"
    case _ =>
      (token2name get token) match {
        case Some(name) => "'" + name + "'"
        case _          => "'<" + token + ">'"
      }
  }

  class MalformedInput(val offset: Offset, val msg: String) extends Exception

  /** A scanner for a given source file not necessarily attached to a compilation unit.
   *  Useful for looking inside source files that are not currently compiled to see what's there
   */
  class SourceFileScanner(val source: SourceFile) extends Scanner {
    def unit = global.currentUnit

    val buf = source.content

    // suppress warnings, throw exception on errors
    def warning(off: Offset, msg: String, category: WarningCategory): Unit = ()
    def deprecationWarning(off: Offset, msg: String, since: String, actions: List[CodeAction]): Unit = ()
    def error(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
    def incompleteInputError(off: Offset, msg: String): Unit = throw new MalformedInput(off, msg)
  }

  /** A scanner over a given compilation unit
   */
  class UnitScanner(override val unit: CompilationUnit, patches: List[BracePatch]) extends SourceFileScanner(unit.source) {
    def this(unit: CompilationUnit) = this(unit, List())

    override def warning(off: Offset, msg: String, category: WarningCategory): Unit =
      runReporting.warning(unit.position(off), msg, category, site = "")
    override def deprecationWarning(off: Offset, msg: String, since: String, actions: List[CodeAction]) =
      runReporting.deprecationWarning(unit.position(off), msg, since, site = "", origin = "", actions)
    override def error(off: Offset, msg: String) =
      reporter.error(unit.position(off), msg)
    override def incompleteInputError(off: Offset, msg: String) =
      currentRun.parsing.incompleteInputError(unit.position(off), msg)

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
          error(offset, "Missing closing brace `}` assumed here")
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
      val braces = code.indices.filter(idx => "{}\n" contains code(idx)).toSet
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
      val oldBalance = mutable.Map[Int, Int]()
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
      @tailrec
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
        case BracePair(loff, lindent, roff, rindent, nested) :: bps1 =>
          if (lindent <= rindent) insert(bps1)
          else {
//           println("patch inside "+bps.head+"/"+line(loff)+"/"+lineStart(line(loff))+"/"+lindent"/"+rindent)//DEBUG
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
