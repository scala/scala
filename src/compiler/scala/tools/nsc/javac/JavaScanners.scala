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
package javac

import scala.tools.nsc.util.JavaCharArrayReader
import scala.reflect.internal.util._
import scala.reflect.internal.Chars._
import JavaTokens._
import scala.annotation.{switch, tailrec}
import scala.language.implicitConversions
import scala.collection.immutable.ArraySeq
import scala.tools.nsc.Reporting.WarningCategory

// Todo merge these better with Scanners
trait JavaScanners extends ast.parser.ScannersCommon {
  val global : Global
  import global._

  abstract class AbstractJavaTokenData {
    def token: Int
    type ScanPosition
    val NoPos: ScanPosition
    def pos: ScanPosition
    def name: Name
  }

  /** A class for representing a token's data. */
  trait JavaTokenData extends AbstractJavaTokenData {
    type ScanPosition = Int

    val NoPos: Int = -1
    /** the next token */
    var token: Int = EMPTY
    /** the token's position */
    var pos: Int = 0

    /** the first character position after the previous token */
    var lastPos: Int = 0

    /** the name of an identifier or token */
    var name: TermName = null

    /** the base of a number */
    var base: Int = 0

    def copyFrom(td: JavaTokenData): this.type = {
      this.token = td.token
      this.pos = td.pos
      this.lastPos = td.lastPos
      this.name = td.name
      this.base = td.base
      this
    }
  }

  /** ...
   */
  abstract class AbstractJavaScanner extends AbstractJavaTokenData {
    implicit def g2p(pos: ScanPosition): Position

    def nextToken(): Unit
    def next: AbstractJavaTokenData
    def intVal(negated: Boolean): Long
    def floatVal(negated: Boolean): Double
    def intVal: Long = intVal(negated = false)
    def floatVal: Double = floatVal(negated = false)
    def currentPos: Position
  }

  object JavaScannerConfiguration {
//  Keywords -----------------------------------------------------------------

    private val allKeywords = List[(Name, Int)](
      javanme.ABSTRACTkw     -> ABSTRACT,
      javanme.ASSERTkw       -> ASSERT,
      javanme.BOOLEANkw      -> BOOLEAN,
      javanme.BREAKkw        -> BREAK,
      javanme.BYTEkw         -> BYTE,
      javanme.CASEkw         -> CASE,
      javanme.CATCHkw        -> CATCH,
      javanme.CHARkw         -> CHAR,
      javanme.CLASSkw        -> CLASS,
      javanme.CONSTkw        -> CONST,
      javanme.CONTINUEkw     -> CONTINUE,
      javanme.DEFAULTkw      -> DEFAULT,
      javanme.DOkw           -> DO,
      javanme.DOUBLEkw       -> DOUBLE,
      javanme.ELSEkw         -> ELSE,
      javanme.ENUMkw         -> ENUM,
      javanme.EXTENDSkw      -> EXTENDS,
      javanme.FALSEkw        -> FALSE,
      javanme.FINALkw        -> FINAL,
      javanme.FINALLYkw      -> FINALLY,
      javanme.FLOATkw        -> FLOAT,
      javanme.FORkw          -> FOR,
      javanme.IFkw           -> IF,
      javanme.GOTOkw         -> GOTO,
      javanme.IMPLEMENTSkw   -> IMPLEMENTS,
      javanme.IMPORTkw       -> IMPORT,
      javanme.INSTANCEOFkw   -> INSTANCEOF,
      javanme.INTkw          -> INT,
      javanme.INTERFACEkw    -> INTERFACE,
      javanme.LONGkw         -> LONG,
      javanme.NATIVEkw       -> NATIVE,
      javanme.NEWkw          -> NEW,
      javanme.PACKAGEkw      -> PACKAGE,
      javanme.PRIVATEkw      -> PRIVATE,
      javanme.PROTECTEDkw    -> PROTECTED,
      javanme.PUBLICkw       -> PUBLIC,
      javanme.RETURNkw       -> RETURN,
      javanme.SHORTkw        -> SHORT,
      javanme.STATICkw       -> STATIC,
      javanme.STRICTFPkw     -> STRICTFP,
      javanme.SUPERkw        -> SUPER,
      javanme.SWITCHkw       -> SWITCH,
      javanme.SYNCHRONIZEDkw -> SYNCHRONIZED,
      javanme.THISkw         -> THIS,
      javanme.THROWkw        -> THROW,
      javanme.THROWSkw       -> THROWS,
      javanme.TRANSIENTkw    -> TRANSIENT,
      javanme.TRUEkw         -> TRUE,
      javanme.TRYkw          -> TRY,
      javanme.VOIDkw         -> VOID,
      javanme.VOLATILEkw     -> VOLATILE,
      javanme.WHILEkw        -> WHILE
    )

    private var kwOffset = -1
    private val kwArray: Array[Int] = {
      val (offset, arr) = createKeywordArray(allKeywords, IDENTIFIER)
      kwOffset = offset
      arr
    }
    final val tokenName = allKeywords.map(_.swap).toMap

//Token representation -----------------------------------------------------

    /** Convert name to token */
    def name2token(name: Name) = {
      val idx = name.start - kwOffset
      if (idx >= 0 && idx < kwArray.length) kwArray(idx)
      else IDENTIFIER
    }

    /** Returns the string representation of given token. */
    def token2string(token: Int): String = token match {
      case IDENTIFIER => "identifier"
      case CHARLIT    => "character literal"
      case DOUBLELIT  => "double literal"
      case FLOATLIT   => "float literal"
      case INTLIT     => "integer literal"
      case LONGLIT    => "long literal"
      case STRINGLIT  => "string literal"
      case EOF        => "eof"
      case ERROR      => "something"
      case AMP        => "`&`"
      case AMPAMP     => "`&&`"
      case AMPEQ      => "`&=`"
      case ASTERISK   => "`*`"
      case ASTERISKEQ => "`*=`"
      case AT         => "`@`"
      case BANG       => "`!`"
      case BANGEQ     => "`!=`"
      case BAR        => "`|`"
      case BARBAR     => "`||`"
      case BAREQ      => "`|=`"
      case COLON      => "`:`"
      case COMMA      => "`,`"
      case DOT        => "`.`"
      case DOTDOTDOT  => "`...`"
      case EQEQ       => "`==`"
      case EQUALS     => "`=`"
      case GT         => "`>`"
      case GTEQ       => "`>=`"
      case GTGT       => "`>>`"
      case GTGTEQ     => "`>>=`"
      case GTGTGT     => "`>>>`"
      case GTGTGTEQ   => "`>>>=`"
      case HAT        => "`^`"
      case HATEQ      => "`^=`"
      case LBRACE     => "`{`"
      case LBRACKET   => "`[`"
      case LPAREN     => "`(`"
      case LT         => "`<`"
      case LTEQ       => "`<=`"
      case LTLT       => "`<<`"
      case LTLTEQ     => "`<<=`"
      case MINUS      => "`-`"
      case MINUSEQ    => "`-=`"
      case MINUSMINUS => "`--`"
      case PERCENT    => "`%`"
      case PERCENTEQ  => "`%=`"
      case PLUS       => "`+`"
      case PLUSEQ     => "`+=`"
      case PLUSPLUS   => "`++`"
      case QMARK      => "`?`"
      case RBRACE     => "`}`"
      case RBRACKET   => "`]`"
      case RPAREN     => "`)`"
      case SEMI       => "`;`"
      case SLASH      => "`/`"
      case SLASHEQ    => "`/=`"
      case TILDE      => "`~`"
      case _ =>
        try s"`${tokenName(token)}`"
        catch {
          case _: ArrayIndexOutOfBoundsException => s"`<$token>`"
          case _: NullPointerException           => s"`<($token)>`"
        }
    }
  }

  /** A scanner for Java.
   *
   *  @author     Martin Odersky
   */
  abstract class JavaScanner extends AbstractJavaScanner with JavaTokenData with Cloneable with ScannerCommon with DocScanner {
    override def intVal = super.intVal// todo: needed?
    override def floatVal = super.floatVal
    def currentPos: Position = g2p(pos - 1)
    var in: JavaCharArrayReader = _

    /** character buffer for literals
     */
    val cbuf = new StringBuilder()

    /** append Unicode character to "lit" buffer
    */
    protected def putChar(c: Char): Unit = { cbuf.append(c) }

    /** Remove the last N characters from the buffer */
    private def popNChars(n: Int): Unit = if (n > 0) cbuf.setLength(cbuf.length - n)

    /** Clear buffer and set name */
    private def setName(): Unit = {
      name = newTermName(cbuf.toString())
      cbuf.setLength(0)
    }

    private class JavaTokenData0 extends JavaTokenData

    /** we need one token lookahead
     */
    val next : JavaTokenData = new JavaTokenData0
    val prev : JavaTokenData = new JavaTokenData0

// Get next token ------------------------------------------------------------

    def nextToken(): Unit = {
      if (next.token == EMPTY) {
        fetchToken()
      }
      else {
        this copyFrom next
        next.token = EMPTY
      }
    }

    def lookaheadToken: Int = {
      prev copyFrom this
      nextToken()
      val t = token
      next copyFrom this
      this copyFrom prev
      t
    }

    /** read next token
     */
    private def fetchToken(): Unit = {
      if (token == EOF) return
      lastPos = in.cpos - 1
      while (true) {
        in.ch match {
          case ' ' | '\t' | CR | LF | FF =>
            in.next()
          case _ =>
            pos = in.cpos
            (in.ch: @switch) match {
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
                putChar(in.ch)
                in.next()
                getIdentRest()
                return

              case '0' =>
                putChar(in.ch)
                in.next()
                if (in.ch == 'x' || in.ch == 'X') {
                  in.next()
                  base = 16
                } else {
                  base = 8
                }
                getNumber()
                return

              case '1' | '2' | '3' | '4' |
                   '5' | '6' | '7' | '8' | '9' =>
                base = 10
                getNumber()
                return

              case '\"' =>
                in.next()
                if (in.ch != '\"') { // "..." non-empty string literal
                  while (in.ch != '\"' && (in.isUnicode || in.ch != CR && in.ch != LF && in.ch != SU)) {
                    getlitch()
                  }
                  if (in.ch == '\"') {
                    token = STRINGLIT
                    setName()
                    in.next()
                  } else {
                    syntaxError("unclosed string literal")
                  }
                } else {
                  in.next()
                  if (in.ch != '\"') { // "" empty string literal
                    token = STRINGLIT
                    setName()
                  } else {
                    in.next()
                    getTextBlock()
                  }
                }
                return

              case '\'' =>
                in.next()
                getlitch()
                if (in.ch == '\'') {
                  in.next()
                  token = CHARLIT
                  setName()
                } else {
                  syntaxError("unclosed character literal")
                }
                return

              case '=' =>
                token = EQUALS
                in.next()
                if (in.ch == '=') {
                  token = EQEQ
                  in.next()
                }
                return

              case '>' =>
                token = GT
                in.next()
                if (in.ch == '=') {
                  token = GTEQ
                  in.next()
                } else if (in.ch == '>') {
                  token = GTGT
                  in.next()
                  if (in.ch == '=') {
                    token = GTGTEQ
                    in.next()
                  } else if (in.ch == '>') {
                    token = GTGTGT
                    in.next()
                    if (in.ch == '=') {
                      token = GTGTGTEQ
                      in.next()
                    }
                  }
                }
                return

              case '<' =>
                token = LT
                in.next()
                if (in.ch == '=') {
                  token = LTEQ
                  in.next()
                } else if (in.ch == '<') {
                  token = LTLT
                  in.next()
                  if (in.ch == '=') {
                    token = LTLTEQ
                    in.next()
                  }
                }
                return

              case '!' =>
                token = BANG
                in.next()
                if (in.ch == '=') {
                  token = BANGEQ
                  in.next()
                }
                return

              case '~' =>
                token = TILDE
                in.next()
                return

              case '?' =>
                token = QMARK
                in.next()
                return

              case ':' =>
                token = COLON
                in.next()
                return

              case '@' =>
                token = AT
                in.next()
                return

              case '&' =>
                token = AMP
                in.next()
                if (in.ch == '&') {
                  token = AMPAMP
                  in.next()
                } else if (in.ch == '=') {
                  token = AMPEQ
                  in.next()
                }
                return

              case '|' =>
                token = BAR
                in.next()
                if (in.ch == '|') {
                  token = BARBAR
                  in.next()
                } else if (in.ch == '=') {
                  token = BAREQ
                  in.next()
                }
                return

              case '+' =>
                token = PLUS
                in.next()
                if (in.ch == '+') {
                  token = PLUSPLUS
                  in.next()
                } else if (in.ch == '=') {
                  token = PLUSEQ
                  in.next()
                }
                return

              case '-' =>
                token = MINUS
                in.next()
                if (in.ch == '-') {
                  token = MINUSMINUS
                  in.next()
                } else if (in.ch == '=') {
                  token = MINUSEQ
                  in.next()
                }
                return

              case '*' =>
                token = ASTERISK
                in.next()
                if (in.ch == '=') {
                  token = ASTERISKEQ
                  in.next()
                }
                return

              case '/' =>
                in.next()
                if (!skipComment()) {
                  token = SLASH
                  in.next()
                  if (in.ch == '=') {
                    token = SLASHEQ
                    in.next()
                  }
                  return
                }

              case '^' =>
                token = HAT
                in.next()
                if (in.ch == '=') {
                  token = HATEQ
                  in.next()
                }
                return

              case '%' =>
                token = PERCENT
                in.next()
                if (in.ch == '=') {
                  token = PERCENTEQ
                  in.next()
                }
                return

              case '.' =>
                token = DOT
                in.next()
                if ('0' <= in.ch && in.ch <= '9') {
                  putChar('.'); getFraction()
                } else if (in.ch == '.') {
                  in.next()
                  if (in.ch == '.') {
                    in.next()
                    token = DOTDOTDOT
                  } else syntaxError("`.` character expected")
                }
                return

              case ';' =>
                token = SEMI
                in.next()
                return

              case ',' =>
                token = COMMA
                in.next()
                return

              case '(' =>
                token = LPAREN
                in.next()
                return

              case '{' =>
                token = LBRACE
                in.next()
                return

              case ')' =>
                token = RPAREN
                in.next()
                return

              case '}' =>
                token = RBRACE
                in.next()
                return

              case '[' =>
                token = LBRACKET
                in.next()
                return

              case ']' =>
                token = RBRACKET
                in.next()
                return

              case SU =>
                if (!in.hasNext) token = EOF
                else {
                  syntaxError("illegal character")
                  in.next()
                }
                return

              case _ =>
                if (Character.isUnicodeIdentifierStart(in.ch)) {
                  putChar(in.ch)
                  in.next()
                  getIdentRest()
                } else {
                  syntaxError("illegal character: "+in.ch.toInt)
                  in.next()
                }
                return
            }
        }
      }
    }

    final protected def putCommentChar(): Unit = { processCommentChar(); in.next() }

    @tailrec final protected def skipBlockComment(isDoc: Boolean): Unit = {
      if (isDoc) beginDocComment("/*") // the second '*' is the current character

      in.ch match {
        case SU  => incompleteInputError("unclosed comment")
        case '*' => putCommentChar() ; if (in.ch == '/') putCommentChar() else skipBlockComment(isDoc)
        case _   => putCommentChar() ; skipBlockComment(isDoc)
      }
    }

    @tailrec final protected def skipLineComment(): Unit = in.ch match {
      case CR | LF | SU =>
      case _            => putCommentChar() ; skipLineComment()
    }

    final protected def skipComment(): Boolean = in.ch match {
      case '/' => putCommentChar() ; skipLineComment() ; finishDocComment() ; true
      case '*' =>
        putCommentChar()
        in.ch match {
          case '*' => skipBlockComment(isDoc = true)
          case _ => skipBlockComment(isDoc = false)
        }
        finishDocComment()
        true
      case _   => false
    }

// Identifiers ---------------------------------------------------------------

    private def getIdentRest(): Unit = {
      while (true) {
        (in.ch: @switch) match {
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
            in.next()

          case '_' =>
            putChar(in.ch)
            in.next()
            getIdentRest()
            return
          case SU =>
            setName()
            token = JavaScannerConfiguration.name2token(name)
            return
          case _ =>
            if (Character.isUnicodeIdentifierPart(in.ch)) {
              putChar(in.ch)
              in.next()
            } else {
              setName()
              token = JavaScannerConfiguration.name2token(name)
              return
            }
        }
      }
    }

// Literals -----------------------------------------------------------------

    /** read next character in character or string literal:
      *
      * @param scanOnly skip emitting errors or adding to the literal buffer
      * @param inTextBlock is this for a text block?
      */
    protected def getlitch(scanOnly: Boolean = false, inTextBlock: Boolean = false): Unit =
      getlitch(in = in, scanOnly = scanOnly, inTextBlock = inTextBlock)

    private def getlitch(in: JavaCharArrayReader, scanOnly: Boolean, inTextBlock: Boolean): Unit = {
      def octal: Char = {
        val leadch: Char = in.ch
        var oct: Int = digit2int(in.ch, 8)
        in.next()
        if ('0' <= in.ch && in.ch <= '7') {
          oct = oct * 8 + digit2int(in.ch, 8)
          in.next()
          if (leadch <= '3' && '0' <= in.ch && in.ch <= '7') {
            oct = oct * 8 + digit2int(in.ch, 8)
            in.next()
          }
        }
        oct.asInstanceOf[Char]
      } // end octal
      def greatEscape: Char = {
        in.next()
        if ('0' <= in.ch && in.ch <= '7') octal
        else {
          val x = in.ch match {
            case 'b'  => '\b'
            case 's'  => ' '
            case 't'  => '\t'
            case 'n'  => '\n'
            case 'f'  => '\f'
            case 'r'  => '\r'
            case '\"' => '\"'
            case '\'' => '\''
            case '\\' => '\\'
            case CR | LF if inTextBlock =>
              if (!scanOnly) in.next()
              0.toChar
            case _    =>
              if (!scanOnly) syntaxError(in.cpos - 1, "invalid escape character")
              in.ch
          }
          if (x != 0) in.next()
          x
        }
      } // end greatEscape
      // begin getlitch
      val c: Char =
        if (in.ch == '\\') greatEscape
        else {
          val res = in.ch
          in.next()
          res
        }
      if (c != 0 && !scanOnly) putChar(c)
    } // end getlitch

    /** read a triple-quote delimited text block, starting after the first three
      * double quotes
      */
    private def getTextBlock(): Unit = {
      // Open delimiter is followed by optional space, then a newline
      while (in.ch == ' ' || in.ch == '\t' || in.ch == FF) {
        in.next()
      }
      if (in.ch != LF && in.ch != CR) { // CR-LF is already normalized into LF by `JavaCharArrayReader`
        syntaxError("illegal text block open delimiter sequence, missing line terminator")
        return
      }
      in.next()

      /* Do a lookahead scan over the full text block to:
       *   - compute common white space prefix
       *   - find the offset where the text block ends
       */
      var commonWhiteSpacePrefix = Int.MaxValue
      var blockEndOffset = 0
      var blockClosed = false
      var lineWhiteSpacePrefix = 0
      var lineIsOnlyWhitespace = true
      val lookahead = in.lookahead
      while (!blockClosed && (lookahead.isUnicode || lookahead.ch != SU)) {
        if (lookahead.ch == '\"') { // Potential end of the block
          lookahead.next()
          if (lookahead.ch == '\"') {
            lookahead.next()
            if (lookahead.ch == '\"') {
              blockClosed = true
              commonWhiteSpacePrefix = commonWhiteSpacePrefix min lineWhiteSpacePrefix
              blockEndOffset = lookahead.cpos - 2
            }
          }

          // Not the end of the block - just a single or double " character
          if (!blockClosed) {
            lineIsOnlyWhitespace = false
          }
        } else if (lookahead.ch == CR || lookahead.ch == LF) { // new line in the block
          lookahead.next()
          if (!lineIsOnlyWhitespace) {
            commonWhiteSpacePrefix = commonWhiteSpacePrefix min lineWhiteSpacePrefix
          }
          lineWhiteSpacePrefix = 0
          lineIsOnlyWhitespace = true
        } else if (lineIsOnlyWhitespace && Character.isWhitespace(lookahead.ch)) { // extend white space prefix
          lookahead.next()
          lineWhiteSpacePrefix += 1
        } else {
          lineIsOnlyWhitespace = false
          getlitch(lookahead, scanOnly = true, inTextBlock = true)
        }
      }

      // Bail out if the block never did have an end
      if (!blockClosed) {
        syntaxError("unclosed text block")
        return
      }

      // Second pass: construct the literal string value this time
      while (in.cpos < blockEndOffset) {
        // Drop the line's leading whitespace
        var remainingPrefix = commonWhiteSpacePrefix
        while (remainingPrefix > 0 && in.ch != CR && in.ch != LF && in.cpos < blockEndOffset) {
          in.next()
          remainingPrefix -= 1
        }

        var trailingWhitespaceLength = 0
        var escapedNewline = false         // Does the line end with `\`?
        while (in.ch != CR && in.ch != LF && in.cpos < blockEndOffset && !escapedNewline) {
          if (Character.isWhitespace(in.ch)) {
            trailingWhitespaceLength += 1
          } else {
            trailingWhitespaceLength = 0
          }

          // Detect if the line is about to end with `\`
          if (in.ch == '\\' && {
            val lookahead = in.copy
            lookahead.next()
            lookahead.ch == CR || lookahead.ch == LF
          }) {
            escapedNewline = true
          }

          getlitch(scanOnly = false, inTextBlock = true)
        }

        // Drop the line's trailing whitespace
        popNChars(trailingWhitespaceLength)

        // Normalize line terminators
        if ((in.ch == CR || in.ch == LF) && !escapedNewline) {
          in.next()
          putChar('\n')
        }
      }

      token = STRINGLIT
      setName()

      // Trailing """
      in.next()
      in.next()
      in.next()
    }

    /** read fractional part and exponent of floating point number
     *  if one is present.
     */
    protected def getFraction(): Unit = {
      token = DOUBLELIT
      while ('0' <= in.ch && in.ch <= '9') {
        putChar(in.ch)
        in.next()
      }
      if (in.ch == 'e' || in.ch == 'E') {
        val lookahead = in.copy
        lookahead.next()
        if (lookahead.ch == '+' || lookahead.ch == '-') {
          lookahead.next()
        }
        if ('0' <= lookahead.ch && lookahead.ch <= '9') {
          putChar(in.ch)
          in.next()
          if (in.ch == '+' || in.ch == '-') {
            putChar(in.ch)
            in.next()
          }
          while ('0' <= in.ch && in.ch <= '9') {
            putChar(in.ch)
            in.next()
          }
        }
        token = DOUBLELIT
      }
      if (in.ch == 'd' || in.ch == 'D') {
        putChar(in.ch)
        in.next()
        token = DOUBLELIT
      } else if (in.ch == 'f' || in.ch == 'F') {
        putChar(in.ch)
        in.next()
        token = FLOATLIT
      }
      setName()
    }

    /** convert name to long value
     */
    def intVal(negated: Boolean): Long = {
      if (token == CHARLIT && !negated) {
        if (name.length > 0) name.charAt(0).toLong else 0
      } else {
        var value: Long = 0
        val divider = if (base == 10) 1 else 2
        val limit: Long =
          if (token == LONGLIT) Long.MaxValue else Int.MaxValue
        var i = 0
        val len = name.length
        while (i < len) {
          val d = digit2int(name.charAt(i), base)
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


    /** convert name, base to double value
    */
    def floatVal(negated: Boolean): Double = {
      val limit: Double =
        if (token == DOUBLELIT) Double.MaxValue else Float.MaxValue
      try {
        val value: Double = java.lang.Double.valueOf(name.toString).doubleValue()
        if (value > limit)
          syntaxError("floating point number too large")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          syntaxError("malformed floating point number")
          0.0
      }
    }
    /** read a number into name and set base
    */
    protected def getNumber(): Unit = {
      while (digit2int(in.ch, if (base < 10) 10 else base) >= 0) {
        putChar(in.ch)
        in.next()
      }
      token = INTLIT
      if (base <= 10 && in.ch == '.') {
        val lookahead = in.copy
        lookahead.next()
        lookahead.ch match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |
               '8' | '9' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' =>
            putChar(in.ch)
            in.next()
            return getFraction()
          case _ =>
            if (!isIdentifierStart(lookahead.ch)) {
              putChar(in.ch)
              in.next()
              return getFraction()
            }
        }
      }
      if (base <= 10 &&
          (in.ch == 'e' || in.ch == 'E' ||
           in.ch == 'f' || in.ch == 'F' ||
           in.ch == 'd' || in.ch == 'D')) {
        return getFraction()
      }
      setName()
      if (in.ch == 'l' || in.ch == 'L') {
        in.next()
        token = LONGLIT
      }
    }

// Errors -----------------------------------------------------------------

    /** generate an error at the given position
    */
    def syntaxError(pos: Int, msg: String): Unit = {
      error(pos, msg)
      token = ERROR
    }

    /** generate an error at the current token position
    */
    def syntaxError(msg: String): Unit = { syntaxError(pos, msg) }

    /** signal an error where the input ended in the middle of a token */
    def incompleteInputError(msg: String): Unit = {
      incompleteInputError(pos, msg)
      token = EOF
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
        JavaScannerConfiguration.token2string(token)
    }

    /** INIT: read lookahead character and token.
     */
    def init(): Unit = {
      in.next()
      nextToken()
    }
  }

  class JavaUnitScanner(unit: CompilationUnit) extends JavaScanner {
    in = new JavaCharArrayReader(new ArraySeq.ofChar(unit.source.content), decodeUni = true, syntaxError)
    init()
    def error(pos: Int, msg: String) = reporter.error(pos, msg)
    def incompleteInputError(pos: Int, msg: String) = currentRun.parsing.incompleteInputError(pos, msg)
    def warning(pos: Int, msg: String, category: WarningCategory) = runReporting.warning(pos, msg, category, site = "")
    def deprecationWarning(pos: Int, msg: String, since: String, actions: List[CodeAction]) = runReporting.deprecationWarning(pos, msg, since, site = "", origin = "", actions)
    implicit def g2p(pos: Int): Position = Position.offset(unit.source, pos)
  }
}
