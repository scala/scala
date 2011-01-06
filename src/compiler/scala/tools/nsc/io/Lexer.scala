package scala.tools.nsc.io

import java.io.{Reader, Writer, StringReader, StringWriter}
import scala.collection.mutable.{Buffer, ArrayBuffer}
import scala.math.BigInt

object Lexer {

  class MalformedInput(val rdr: Lexer, val msg: String) extends Exception("Malformed JSON input at "+rdr.tokenPos+": "+msg)

  class Token(val str: String) {
    override def toString = str
  }

  case class Delim(char: Char) extends Token("'"+char.toString+"'")
  case class IntLit(override val str: String) extends Token(str)
  case class FloatLit(override val str: String) extends Token(str)
  case class StringLit(override val str: String) extends Token(str) {
    override def toString = quoted(str)
  }

  val TrueLit = new Token("true")
  val FalseLit = new Token("false")
  val NullLit = new Token("null")
  val LParen = new Delim('(')
  val RParen = new Delim(')')
  val LBrace = new Delim('{')
  val RBrace = new Delim('}')
  val LBracket = new Delim('[')
  val RBracket = new Delim(']')
  val Comma = new Delim(',')
  val Colon = new Delim(':')
  val EOF = new Token("<end of input>")

  private def toUDigit(ch: Int): Char = {
    val d = ch & 0xF
    (if (d < 10) d + '0' else d - 10 + 'A').toChar
  }

  private def addToStr(buf: StringBuilder, ch: Char) {
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
        else buf ++= "\\u" += toUDigit(ch >>> 12) += toUDigit(ch >>> 8) += toUDigit(ch >>> 4) += toUDigit(ch)
    }
  }

  def quoted(str: String): String = {
    val buf = new StringBuilder += '\"'
    str foreach (addToStr(buf, _))
    buf += '\"'
    buf.toString
  }

  private val BUF_SIZE = 2 << 16
}

import Lexer._

class Lexer(rd: Reader) {

  var ch: Char = 0
  var pos: Long = 0
  var token: Token = _
  var tokenPos: Long = 0

  private var atEOF: Boolean = false
  private val buf = new Array[Char](BUF_SIZE)
  private var nread: Int = 0
  private var bp = 0

  def nextChar() {
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

  def acceptChar(c: Char) = if (ch == c) nextChar() else error("'"+c+"' expected")

  val sb = new StringBuilder

  def putChar() {
    sb += ch; nextChar()
  }

  def putAcceptString(str: String) {
    str foreach acceptChar
    sb ++= str
  }

  def nextToken() {
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
      case _ => error("illegal string literal character: '"+ch+"'")
    }
    println("["+token+"]")
  }

  def getString() {
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

  def getNumber() {
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

  def accept(t: Token) {
    if (token == t) nextToken()
    else error(t+" expected, but "+token+" found")
  }

  def accept(ch: Char) {
    token match {
      case Delim(`ch`) => nextToken()
      case _ => accept(Delim(ch))
    }
  }

  def error(msg: String) = throw new MalformedInput(this, msg)

  nextChar()
  nextToken()
}
