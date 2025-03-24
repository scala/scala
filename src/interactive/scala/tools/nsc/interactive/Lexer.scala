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

package scala.tools.nsc.interactive

import java.io.Reader

/** Companion object of class `Lexer` which defines tokens and some utility concepts
 *  used for tokens and lexers
 */
object Lexer {

  /** An exception raised if an input does not correspond to what's expected
   *  @param   rdr   the lexer from which the bad input is read
   *  @param   msg   the error message
   */
  class MalformedInput(val rdr: Lexer, val msg: String) extends Exception("Malformed JSON input at "+rdr.tokenPos+": "+msg)

  /** The class of tokens, i.e. descriptions of input words (or: lexemes).
   *  @param str    the characters making up this token
   */
  class Token(val str: String) {
    override def toString = str
  }

  /** A subclass of `Token` representing single-character delimiters
   *  @param char the delimiter character making up this token
   */
  case class Delim(char: Char) extends Token(s"'$char'")

  /** A subclass of token representing integer literals */
  case class IntLit(override val str: String) extends Token(str)

  /** A subclass of token representing floating point literals */
  case class FloatLit(override val str: String) extends Token(str)

  /** A subclass of token representing string literals */
  case class StringLit(override val str: String) extends Token(str) {
    override def toString = quoted(str)
  }

  /** The `true` token */
  val TrueLit = new Token("true")

  /** The `false` token */
  val FalseLit = new Token("false")

  /** The `null` token */
  val NullLit = new Token("null")

  /** The '`(`' token */
  val LParen = new Delim('(')

  /** The '`)`' token */
  val RParen = new Delim(')')

  /** The '`{`' token */
  val LBrace = new Delim('{')

  /** The '`}`' token */
  val RBrace = new Delim('}')

  /** The '`[`' token */
  val LBracket = new Delim('[')

  /** The '`]`' token */
  val RBracket = new Delim(']')

  /** The '`,`' token */
  val Comma = new Delim(',')

  /** The '`:`' token */
  val Colon = new Delim(':')

  /** The token representing end of input */
  val EOF = new Token("<end of input>")

  private def toUDigit(ch: Int): Char = {
    val d = ch & 0xF
    (if (d < 10) d + '0' else d - 10 + 'A').toChar
  }

  private def addToStr(buf: StringBuilder, ch: Char): Unit = {
    ch match {
      case '"' => buf ++= "\\\""
      case '\b' => buf ++= "\\b"
      case '\f' => buf ++= "\\f"
      case '\n' => buf ++= "\\n"
      case '\r' => buf ++= "\\r"
      case '\t' => buf ++= "\\t"
      case '\\' => buf ++= "\\\\"
      case _ =>
        if (' ' <= ch && ch < 128) buf += ch
        else buf ++= "\\u" += toUDigit(ch >>> 12) += toUDigit(ch >>> 8) += toUDigit(ch >>> 4) += toUDigit(ch.toInt)
    }
  }

  /** Returns given string enclosed in `"`-quotes with all string characters escaped
   *  so that they correspond to the JSON standard.
   *  Characters that escaped are:  `"`, `\b`, `\f`, `\n`, `\r`, `\t`, `\`.
   *  Furthermore, every other character which is not in the ASCII range 32-127 is
   *  escaped as a four hex-digit unicode character of the form `\ u x x x x`.
   *  @param   str   the string to be quoted
   */
  def quoted(str: String): String = {
    val buf = new StringBuilder += '\"'
    str foreach (addToStr(buf, _))
    buf += '\"'
    buf.toString
  }

  private val BUF_SIZE = 2 << 16
}

import Lexer._

/** A simple lexer for tokens as they are used in JSON, plus parens `(`, `)`
 *  Tokens understood are:
 *
 *  `(`, `)`, `[`, `]`, `{`, `}`, `:`, `,`, `true`, `false`, `null`,
 *  strings (syntax as in JSON),
 *  integer numbers (syntax as in JSON: -?(0|\d+)
 *  floating point numbers (syntax as in JSON: -?(0|\d+)(\.\d+)?((e|E)(+|-)?\d+)?)
 *  The end of input is represented as its own token, EOF.
 *  Lexers can keep one token lookahead
 *
 * @param rd   the reader from which characters are read.
 */
class Lexer(rd: Reader) {

  /** The last-read character */
  var ch: Char = 0

  /** The number of characters read so far */
  var pos: Long = 0

  /** The last-read token */
  var token: Token = _

  /** The number of characters read before the start of the last-read token */
  var tokenPos: Long = 0

  private var atEOF: Boolean = false
  private val buf = new Array[Char](BUF_SIZE)
  private var nread: Int = 0
  private var bp = 0

  /** Reads next character into `ch` */
  def nextChar(): Unit = {
    assert(!atEOF)
    if (bp == nread) {
      nread = rd.read(buf)
      bp = 0
      if (nread <= 0) { ch = 0; atEOF = true; return }
    }
    ch = buf(bp)
    bp += 1
    pos += 1
  }

  /** If last-read character equals given character, reads next character,
   *  otherwise raises an error
   *  @param  c   the given character to compare with last-read character
   *  @throws  Lexer.MalformedInput if character does not match
   */
  def acceptChar(c: Char) = if (ch == c) nextChar() else error("'"+c+"' expected")

  private val sb = new StringBuilder

  private def putChar(): Unit = {
    sb += ch; nextChar()
  }

  private def putAcceptString(str: String): Unit = {
    str foreach acceptChar
    sb ++= str
  }

  /** Skips whitespace and reads next lexeme into `token`
   *  @throws  Lexer.MalformedInput if lexeme not recognized as a valid token
   */
  def nextToken(): Unit = {
    sb.clear()
    while (!atEOF && ch <= ' ') nextChar()
    tokenPos = pos - 1
    if (atEOF) token = EOF
    else ch match {
      case '(' => putChar(); token = LParen
      case ')' => putChar(); token = RParen
      case '{' => putChar(); token = LBrace
      case '}' => putChar(); token = RBrace
      case '[' => putChar(); token = LBracket
      case ']' => putChar(); token = RBracket
      case ',' => putChar(); token = Comma
      case ':' => putChar(); token = Colon
      case 't' => putAcceptString("true"); token = TrueLit
      case 'f' => putAcceptString("false"); token = FalseLit
      case 'n' => putAcceptString("null"); token = NullLit
      case '"' => getString()
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => getNumber()
      case _ => error("unrecognized start of token: '"+ch+"'")
    }
    //println("["+token+"]")
  }

  /** Reads a string literal, and forms a `StringLit` token from it.
   *  Last-read input character `ch` must be opening `"`-quote.
   *  @throws  Lexer.MalformedInput if lexeme not recognized as a string literal.
   */
  def getString(): Unit = {
    def udigit() = {
      nextChar()
      if ('0' <= ch && ch <= '9') ch - '9'
      else if ('A' <= ch && ch <= 'F') ch - 'A' + 10
      else if ('a' <= ch && ch <= 'f') ch - 'a' + 10
      else error("illegal unicode escape character: '"+ch+"'")
    }
    val delim = ch
    nextChar()
    while (ch != delim && ch >= ' ') {
      if (ch == '\\') {
        nextChar()
        ch match {
          case '\'' => sb += '\''
          case '"' => sb += '"'
          case '\\' => sb += '\\'
          case '/' => sb += '/'
          case 'b' => sb += '\b'
          case 'f' => sb += '\f'
          case 'n' => sb += '\n'
          case 'r' => sb += '\r'
          case 't' => sb += '\t'
          case 'u' => sb += (udigit() << 12 | udigit() << 8 | udigit() << 4 | udigit()).toChar
          case _ => error("illegal escape character: '"+ch+"'")
        }
        nextChar()
      } else {
        putChar()
      }
    }
    acceptChar(delim)
    token = StringLit(sb.toString)
  }

  /** Reads a numeric literal, and forms an `IntLit` or `FloatLit` token from it.
   *  Last-read input character `ch` must be either `-` or a digit.
   *  @throws  Lexer.MalformedInput if lexeme not recognized as a numeric literal.
   */
  def getNumber(): Unit = {
    def digit() =
      if ('0' <= ch && ch <= '9') putChar()
      else error("<digit> expected")
    def digits() =
      do { digit() } while ('0' <= ch && ch <= '9')
    var isFloating = false
    if (ch == '-') putChar()
    if (ch == '0') digit()
    else digits()
    if (ch == '.') {
      isFloating = true
      putChar()
      digits()
    }
    if (ch == 'e' || ch == 'E') {
      isFloating = true
      putChar()
      if (ch == '+' || ch == '-') putChar()
      digits()
    }
    token = if (isFloating) FloatLit(sb.toString) else IntLit(sb.toString)
  }

  /** If current token equals given token, reads next token, otherwise raises an error.
   *  @param  t   the given token to compare current token with
   *  @throws Lexer.MalformedInput  if the two tokens do not match.
   */
  def accept(t: Token): Unit = {
    if (token == t) nextToken()
    else error(s"$t expected, but $token found")
  }

  /** The current token is a delimiter consisting of given character, reads next token,
   *  otherwise raises an error.
   *  @param  ch   the given delimiter character to compare current token with
   *  @throws Lexer.MalformedInput  if the current token `token` is not a delimiter, or
   *                          consists of a character different from `c`.
   */
  def accept(ch: Char): Unit = {
    token match {
      case Delim(`ch`) => nextToken()
      case _ => accept(Delim(ch))
    }
  }

  /** Always throws a [[Lexer.MalformedInput]] exception with given error message.
   *  @param msg  the error message
   */
  def error(msg: String) = throw new MalformedInput(this, msg)

  nextChar()
  nextToken()
}
