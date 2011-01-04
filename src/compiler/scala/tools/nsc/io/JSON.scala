package scala.tools.nsc.io

import java.io.{Reader, Writer, StringReader, StringWriter}
import scala.collection.mutable.{Buffer, ArrayBuffer}
import scala.math.BigInt

object JSON {

  class MalformedInput(val rdr: JReader, val msg: String) extends Exception("Malformed JSON input at "+rdr.tokenPos+": "+msg)

  class PrettyWriter(wr: Writer) extends Writer {
    protected val indentStep = "  "
    private var indent = 0
    private def newLine() {
      wr.write('\n')
      wr.write(indentStep * indent)
    }
    def close() = wr.close()
    def flush() = wr.flush()
    def write(str: Array[Char], off: Int, len: Int): Unit = {
      if (off < str.length && off < len) {
        str(off) match {
          case '{' | '[' =>
            indent += 1
            wr.write(str(off))
            newLine()
            wr.write(str, off + 1, len - 1)
          case '}' | ']' =>
            wr.write(str, off, len)
            indent -= 1
          case ',' =>
            wr.write(',')
            newLine()
            wr.write(str, off + 1, len - 1)
          case ':' =>
            wr.write(':')
            wr.write(' ')
            wr.write(str, off + 1, len - 1)
          case _ =>
            wr.write(str, off, len)
        }
      } else {
        wr.write(str, off, len)
      }
    }
    override def toString = wr.toString
  }

  object JReader {

    type Token = Int

    val STRING = 1
    val INTEGER = 2
    val FLOAT = 3
    val LBRACE = 4
    val RBRACE = 5
    val LBRACKET = 6
    val RBRACKET = 7
    val COMMA = 8
    val COLON = 9
    val TRUE = 10
    val FALSE = 11
    val NULL = 12
    val EOF = 13

    def show(token: Token) = token match {
      case STRING => "string literal"
      case INTEGER => "integer literal"
      case FLOAT => "floating point literal"
      case LBRACE => "'{'"
      case RBRACE => "'}'"
      case LBRACKET => "'['"
      case RBRACKET => "']'"
      case COMMA => "','"
      case COLON => "':'"
      case TRUE => "'true'"
      case FALSE => "'false'"
      case NULL => "'null'"
      case EOF => "<end of input>"
    }

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

  import JReader._

  abstract class JIterator[T] extends BufferedIterator[T] {
    def reader: Option[JReader]
  }

  class JReader(rd: Reader) {

    var ch: Char = 0
    var pos: Long = 0
    var token: Token = 0
    var tokenPos: Long = 0
    private var atEOF: Boolean = false

    private val buf = new Array[Char](BUF_SIZE)
    private var nread: Int = 0
    private var bp = 0

    def nextChar() {
      if (!atEOF) {
        if (bp == nread) {
          nread = rd.read(buf)
          bp = 0
          if (nread <= 0) { ch = 0; atEOF = true; return }
        }
        ch = buf(bp)
        bp += 1
        pos += 1
      }
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
        case '{' => putChar(); token = LBRACE
        case '}' => putChar(); token = RBRACE
        case '[' => putChar(); token = LBRACKET
        case ']' => putChar(); token = RBRACKET
        case ',' => putChar(); token = COMMA
        case ':' => putChar(); token = COLON
        case 't' => putAcceptString("true"); token = TRUE
        case 'f' => putAcceptString("false"); token = FALSE
        case 'n' => putAcceptString("null"); token = NULL
        case '"' => getString()
        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => getNumber()
        case _ => error("illegal string literal character: '"+ch+"'")
      }
      println("["+showCurrent+"]")
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
      token = STRING
    }

    def getNumber() {
      def digit() =
        if ('0' <= ch && ch <= '9') putChar()
        else error("<digit> expected")
      def digits() =
        do { digit() } while ('0' <= ch && ch <= '9')
      token = INTEGER
      if (ch == '-') putChar()
      if (ch == '0') digit()
      else digits()
      if (ch == '.') {
        token = FLOAT
        putChar()
        digits()
      }
      if (ch == 'e' || ch == 'E') {
        token = FLOAT
        putChar()
        if (ch == '+' || ch == '-') putChar()
        digits()
      }
    }

    def accept(t: Token) =
      if (token == t) nextToken()
      else error(show(t)+" expected, but "+showCurrent+" found")

    def headValue(): JValue = token match {
      case STRING => JString(tokenStr)
      case INTEGER =>
        try {
          JLong(tokenStr.toLong)
        } catch {
          case ex: NumberFormatException => JInteger(BigInt(tokenStr))
        }
      case FLOAT => JDouble(tokenStr.toDouble)
      case LBRACE => new JObject(this)
      case LBRACKET => new JArray(this)
      case TRUE => JTrue
      case FALSE => JFalse
      case NULL => JNull
      case t => error("unexpected: "+showCurrent)
    }

    def nextValue(): JValue = {
      val result = headValue()
      nextToken()
      result
    }

    def iterator: JIterator[JValue] =
      new JTopLevel(this).inputIterator

    def tokenStr = sb.toString

    def showCurrent() = token match {
      case STRING => "string literal "+quoted(tokenStr)
      case INTEGER => "integer literal "+tokenStr
      case FLOAT => "floating point literal "+tokenStr
      case _ => show(token)
    }

    def error(msg: String) = throw new MalformedInput(this, msg)

    nextChar()
    nextToken()
  }

  abstract class JElement {
    def isComplete: Boolean
    def doneReading()
    def readFully(): this.type
    def write(wr: Writer)
  }

  case class J_: (label: String, value: JValue) extends JElement {
    def isComplete = value.isComplete
    def doneReading() = value.doneReading()
    def readFully() = { value.readFully(); this }
    def write(wr: Writer) { wr.write(quoted(label)); wr.write(":"); value.write(wr) }
    override def toString = label+" J_: "+value
  }

  abstract class JValue extends JElement {
    def J_: (label: String) = new J_: (label, this)
  }

  abstract class JContainer[T <: JElement](closingToken: Token,
                                           elems: T*) extends JValue {
    protected var input: Option[JReader] = None
    private var prevInput = input

    val contents: Buffer[T] = new ArrayBuffer[T] ++= elems

    protected def readElement(rdr: JReader): T

    val inputIterator = new JIterator[T] {
      var last: Option[T] = None
      var lastIsNext: Boolean = false
      def hasNext: Boolean =
        lastIsNext || {
          input match {
            case None => false
            case Some(rdr) =>
              closeLast()
              rdr.token != closingToken
          }
        }
      def head: T =
        if (lastIsNext) last.get
        else input match {
          case None => Iterator.empty.next
          case Some(rdr) =>
            closeLast()
            if (last.isDefined) rdr.accept(COMMA)
            val x = readElement(rdr)
            last = Some(x)
            lastIsNext = true
            x
        }
      def next(): T = {
        val result = head
        lastIsNext = false
        println("next: "+result)
        result
      }
      def closeLast(): Unit = last match {
        case Some(elem) => elem.doneReading()
        case _ =>
      }
      def reader = input orElse prevInput
    }

    def isComplete: Boolean = !inputIterator.hasNext

    def doneReading() {
      require(isComplete)
      input match {
        case None => // already closed
        case Some(rdr) =>
          inputIterator.closeLast()
          println("done reading: "+rdr.showCurrent)
          rdr.accept(closingToken)
          prevInput = input
          input = None
      }
    }

    def readFully() = {
      contents ++= inputIterator map (_.readFully())
      doneReading()
      this
    }

    def skip() {
      inputIterator foreach { _ => () }
      doneReading()
    }

    protected def writeContents(wr: Writer) {
      var first = true
      for (elem <- contents) {
        if (first) first = false else wr.write(",")
        elem.write(wr)
      }
    }

  }

  class JObject(elems: J_: *) extends JContainer(RBRACE, elems: _*) {
    def this(in: JReader) = { this(); input = Some(in) }
    protected def readElement(rdr: JReader): J_: = {
      if (rdr.token == STRING) {
        val label = rdr.tokenStr
        rdr.nextToken(); rdr.accept(COLON)
        label J_: rdr.nextValue()
      } else {
        rdr.error("string literal expected, but "+rdr.tokenStr+" found")
      }
    }

    def write(wr: Writer) {
      wr.write("{")
      writeContents(wr)
      wr.write("}")
    }

    override def toString = contents.mkString("{", ",", "}")
    override def hashCode = contents.hashCode + 17
    override def equals(other: Any) = other match {
      case that: JObject => this.contents == that.contents
      case _ => false
    }
  }

  class JArray(elems: JValue*) extends JContainer(RBRACKET, elems: _*) {
    def this(in: JReader) = { this(); input = Some(in) }
    protected val closingToken = RBRACKET
    protected def readElement(rdr: JReader): JValue = rdr.nextValue()

    def write(wr: Writer) {
      wr.write("[")
      writeContents(wr)
      wr.write("]")
    }

    override def toString = contents.mkString("[", ",", "]")
    override def hashCode = contents.hashCode + 19
    override def equals(other: Any) = other match {
      case that: JArray => this.contents == that.contents
      case _ => false
    }
  }

  class JTopLevel(elems: JValue*) extends JContainer(EOF, elems: _*) {
    def this(in: JReader) = { this(); input = Some(in) }
    protected val closingToken = EOF
    protected def readElement(rdr: JReader): JValue = rdr.nextValue()

    def write(wr: Writer) {
      writeContents(wr)
    }

    override def toString = contents.mkString(",")
    override def hashCode = contents.hashCode + 23
    override def equals(other: Any) = other match {
      case that: JTopLevel => this.contents == that.contents
      case _ => false
    }
  }

  object JObject {
    def apply(elems: J_: *) = new JObject(elems: _*)
    def unapplySeq(x: JObject): Some[Seq[J_:]] = Some(x.contents)
  }

  object JArray {
    def apply(elems: JValue*) = new JArray(elems: _*)
    def unapplySeq(x: JArray): Some[Seq[JValue]] = Some(x.contents)
  }

  abstract class JAtom extends JValue {
    def isComplete = true
    def doneReading() {}
    def readFully() = this
  }

  case class JInteger(value: BigInt) extends JAtom {
    def write(wr: Writer) { wr.write(value.toString) }
  }

  case class JLong(value: Long) extends JAtom {
    def write(wr: Writer) { wr.write(value.toString) }
  }

  case class JDouble(value: Double) extends JAtom {
    def write(wr: Writer) { wr.write(value.toString) }
  }

  case class JString(value: String) extends JAtom {
    def write(wr: Writer) { wr.write(quoted(value)) }
  }

  case object JTrue extends JAtom {
    def write(wr: Writer) { wr.write("true") }
  }

  case object JFalse extends JAtom {
    def write(wr: Writer) { wr.write("false") }
  }

  case object JNull extends JAtom {
    def write(wr: Writer) { wr.write("null") }
  }
}

object Test extends Application {
  import JSON._
  val obj = JObject(
    "a" J_: JArray(JLong(1), JDouble(2.0), JInteger(BigInt("12345678901234567890"))),
    "b" J_: JString("I am a \t String \n on two lines"),
    "c" J_: JObject("t" J_: JTrue, "f" J_: JFalse),
    "d" J_: JNull,
    "e" J_: JArray(),
    "f" J_: JObject()
  )
  val sw = new PrettyWriter(new StringWriter())
  obj.write(sw)
  sw.close()
  val s = sw.toString
  println("written: "+s)
  var sr = new JReader(new StringReader(s))
  val it = sr.iterator
  val r = it.head.readFully()
  it.next()
  println("read: "+r)
  assert(!it.hasNext, it.next()+"/"+sr.showCurrent+"/"+r)
  assert(obj == r)
}
