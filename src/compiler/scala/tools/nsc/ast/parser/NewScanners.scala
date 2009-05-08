/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.ast.parser

import scala.tools.nsc.util.SourceFile._
import scala.tools.nsc.util._
import scala.annotation.switch

trait NewScanners {
  val global : Global
  import global._
  import Tokens._
  trait CoreScannerInput extends BufferedIterator[Char] {
    private[NewScanners] val scratch = new StringBuilder

    /** iterates over and applies <code>f</code> to the next element
     *  if this iterator has a next element that <code>f</code> is defined for.
     */
    def readIf[T](f : PartialFunction[Char,T]) : Option[T] =
      if (hasNext && f.isDefinedAt(head))
        Some(f(next))
      else None

    /** iterates over elements as long as <code>f</code> is true
     *  for each element, returns whether anything was read
     */
    def readWhile(f : Char => Boolean) : Boolean = {
      var read = false
      while (hasNext && f(head)) {
        next
        read = true
      }
      read
    }

    def readIfStartsWith(c : Char) : Boolean =
      if (head == c) { next; true } else false
    def readIfStartsWith(c0 : Char, c1 : Char) : Boolean =
      if (head == c0 && peek(1) == c1) {
        next; next; true
      } else false
    def startsWith(c0: Char, c1 : Char) : Boolean = head == c0 && peek(1) == c1
    def isUnicode : Boolean

    def peek(idx : Int) : Char

    def offset : Int
    def error(offset : Int, msg : String) : Unit
    def incompleteError(offset : Int, msg : String) : Unit = error(offset, msg)
    def textFor(from : Int, until : Int) : RandomAccessSeq[Char]
  }
  trait ScannerInput extends CoreScannerInput {
    def seek(offset : Int) : Unit
  }
  class DefaultInput(in : NewCharArrayReader) extends ScannerInput {
    import scala.collection.mutable._
    def seek(offset : Int) = in.seek(offset)
    def offset = in.offset
    def head = peek(0)
    def next = in.next
    def isUnicode : Boolean = in.isUnicode
    def hasNext = in.hasNext
    def peek(idx : Int) = {
      val offset = in.offset
      var jdx = idx
      var result = in.next
      while (jdx > 0) {
        jdx = jdx - 1
        result =in.next
      }
      in.seek(offset) // jump back to old position
      result
    }
    def error(offset : Int, msg : String) : Unit = {}
    def textFor(from : Int, until : Int) = in.buf.slice(from, until).mkString
  }

  abstract class ParserScanner extends BaseScanner {
    def init = nextToken

    private var doc : String = ""
    var sepRegions : List[Int] = Nil
    private val current = new TokenHolder
    private val    next = new TokenHolder
    implicit def in : ScannerInput

    def assertEmpty(th: TokenHolder) =
      if (th.code != EMPTY) in.error(th.offset, "unexpected token")
    def assertNotEmpty(th: TokenHolder) =
      if (th.code == EMPTY) in.incompleteError(th.offset, "expected token not present")

    var lastCode = EMPTY
    next.code = EMPTY
    current.code = EMPTY
    def hasNext = in.hasNext || (next.code != EMPTY && next.code != EOF)
    def flush : ScannerInput = {
      assertNotEmpty(current)
      in.seek(unadjust(current.offset))
      current.code = EMPTY
      next.code = EMPTY
      in
    }
    def seek(offset : Int, lastCode : Int) = {
      assertEmpty(current)
      in.seek(unadjust(offset))
      this.lastCode = lastCode
      nextToken
    }
    def resume(lastCode : Int) = {
      assertEmpty(current)
      this.lastCode = lastCode
      nextToken
    }
    /** read next token and return last position
     */
    def skipToken: Int = {
      val p = current.offset; nextToken
      // XXX: account for off by one error //???
      p
    }
    def currentPos = {
      assertNotEmpty(current)
      current.offset
    }
    def fillNext : Boolean = {
      assertEmpty(next)
      var hasNewline = false
      do {
        fill(next)
      } while (next.code match {
        case NEWLINE|NEWLINES|WHITESPACE|COMMENT =>
          assert((next.code != COMMENT) == (xmlOk))
          hasNewline = hasNewline || next.code == NEWLINE || next.code == NEWLINES
          if (next.code == COMMENT)
            doc = next.value.asInstanceOf[Option[String]].getOrElse("")
          true
        case _ => false
        })
        hasNewline
    }

    def flushDoc = {
      val ret = doc
      doc = ""
      ret
    }

    def nextToken : Unit = {
      if (current.code == EOF) return // nothing more.
      var lastIsComment = false
      lastCode = current.code match {
      case WHITESPACE|EMPTY => lastCode
      case COMMENT => lastIsComment = true; lastCode
      case code => code
      }
      // push on braces
      val pushOn = (current.code) match {
      case LBRACE => RBRACE
      case LPAREN => RPAREN
      case LBRACKET => RBRACKET
      case CASE =>
        ARROW
      case RBRACE =>
        while (!sepRegions.isEmpty && sepRegions.head != RBRACE)
          sepRegions = sepRegions.tail
        if (!sepRegions.isEmpty)
          sepRegions = sepRegions.tail
        EMPTY
      case code @ (ARROW) if (!sepRegions.isEmpty && sepRegions.head == code) =>
        sepRegions = sepRegions.tail
        EMPTY
      case ARROW =>
        EMPTY
      case code @ (RPAREN|RBRACKET) =>
        if (!sepRegions.isEmpty && sepRegions.head == code)
          sepRegions = sepRegions.tail
        EMPTY
      case _ => EMPTY
      }
      if (pushOn != EMPTY) sepRegions = pushOn :: sepRegions

      if (next.code != EMPTY) {
        current.copy(next)
        next.code = EMPTY
      } else fill(current)

      def currentIsNext : Unit = {
        assertNotEmpty(next)
        return nextToken
      }
      current.code match {
      case CASE|SEMI =>
        fillNext
        (current.code,next.code) match {
        case (CASE,OBJECT) =>
          current.code = CASEOBJECT; next.code = EMPTY
        case (CASE, CLASS) => current.code = CASECLASS ; next.code = EMPTY
        case (SEMI, ELSE ) => currentIsNext
        case _ =>
        }
      case WHITESPACE|COMMENT =>
        if (current.code == COMMENT)
          doc = current.value.asInstanceOf[Option[String]].getOrElse("")
        nextToken
      case NEWLINE | NEWLINES =>
        if (!xmlOk) in.error(current.offset, "XML mode not legal here")
        val headIsRBRACE = if (sepRegions.isEmpty) true else sepRegions.head == RBRACE
        val hasNewline = fillNext
        if (headIsRBRACE && (inLastOfStat(lastCode) && inFirstOfStat(next.code)
           /* This need to be commented out, otherwise line
              continuation in the interpreter will not work
	      XXX: not sure how the IDE reacts with this commented out.
              || next.code == EOF */ )) {
          //if (hasNewline) current.code = NEWLINES
        } else {
          currentIsNext
        }
      case _ =>
      }
    }

    def token = {
      assertNotEmpty(current)
      current.code
    }

    def nextTokenCode = {
      if (next.code == EMPTY) fillNext
      next.code
    }

    def name = current.value.get.asInstanceOf[Name]

    def charVal = current.value.get.asInstanceOf[Char]

    def intVal(negated : Boolean) : Long = {
      val base = current.value.asInstanceOf[Option[Int]].getOrElse(10)
      intVal(current.offset, current.code, current.nLit(this), negated, base)
    }
    def intVal : Long = intVal(false)

    def floatVal(negated: Boolean): Double = {
      floatVal(current.offset, current.code, current.nLit(this), negated)
    }
    def floatVal : Double = floatVal(false)

    def stringVal = current.value.get.asInstanceOf[String]
  }

  class TokenHolder {
    var offset : Int = 0
    var code : Int = 0
    var length : Int = 0
    var value : Option[Any] = None
    def copy(from : TokenHolder) = {
      this.offset = from.offset
      this.code   = from.code
      this.length = from.length
      this.value = from.value
    }
    def set(offset : Int, length : Int, code : Int) = {
      this.offset = offset; this.length = length; this.code = code; this.value = None
    }
    def set(offset : Int, length : Int, code : Int, value : Any) = {
      this.offset = offset; this.length = length; this.code = code; this.value = Some(value)
    }
    def nLit(implicit in : BaseScanner) = (in.in.textFor(in.unadjust(offset), in.unadjust(offset + length)))

  }

  trait BaseScanner {
    implicit def in : CoreScannerInput
    ScannerConfiguration.hashCode // forces initialization
    import ScannerConfiguration._
    var xmlOk = true

    def iterator = new Iterator[(Int,Int,Int)] { // offset,length,code
      val current = new TokenHolder
      def hasNext = in.hasNext
      def next = {
        fill(current)
        (current.offset, current.length, current.code)
      }
    }
    // IDE hooks
    def   adjust(offset : Int) = offset
    def unadjust(offset : Int) = offset
    def identifier(name : Name) = name

    protected def fill(current : TokenHolder) : Unit = {
      if (!in.hasNext) {
        current.offset = adjust(in.offset)
        current.code = EOF
        return
      }
      val oldXmlOk = xmlOk
      xmlOk = false
      val offset = in.offset // call "after" next
      def escapeCode(offset : Int) : Char = in.next match {
      case c if simpleEscape.isDefinedAt(c) => simpleEscape(c)
      case c if isDigit(c) =>
        val length = in.scratch.length
        try {
          in.scratch append c
          while (isDigit(in.head)) in.scratch append in.next
          val n = Integer.parseInt(in.scratch.drop(length).mkString, 8)
          if (n > 0377) {
            in.error(offset, "malformed octal character code"); 0.toChar
          } else n.toChar
        } catch {
          case ex : Exception => in.error(offset, "malformed octal character code"); 0.toChar
        } finally {
          in.scratch.setLength(length)
        }
      case c => in.error(offset, "unrecognized escape code \'" + c + "\'"); c
      }
      def getIdentRest : Unit = in.readIf{
      case '_' =>
        in.scratch append '_'
        val c = in.head
        if (isOperatorPart(c)) getOperatorRest else getIdentRest
      case c if isIdentifierPart(c) =>
         in.scratch append c; getIdentRest
      }

      val next = in.next
      // called after everything is read.
      def length = in.offset - offset

      def value(code : Int, value : Any) : Int = {
        current.value = Some(value)
        code
      }
      def doOperator(c : Char) = {
        in.scratch.setLength(0)
        in.scratch append(c)
        getOperatorRest
        val name : Name = global.newTermName(in.scratch.toString)
        value(name2token(name), (name))
      }
      current.offset = adjust(offset)
      current.value = None
      current.code = next match {
      case ';' => (SEMI)
      case ',' => (COMMA)
      case '(' => xmlOk = true; (LPAREN)
      case ')' => (RPAREN)
      case '{' => xmlOk = true; (LBRACE)
      case '}' => (RBRACE)
      case '[' => (LBRACKET)
      case ']' => (RBRACKET)
      case SU => EOF
      case '\u21D2' => (ARROW)
      case '\u2190' => (LARROW)
      case '<' =>
        if (oldXmlOk && (in.head match {
        case ('!' | '?') => true
        case c if xml.Parsing.isNameStart(c) => true
        case _ => false
        })) { in.next; XMLSTART }
        else doOperator('<')
      case ' ' | '\t' => in.readWhile(isSpace); xmlOk = true; (WHITESPACE)
      case '/' =>
        if (in.readIfStartsWith('/')) {
          while (in.hasNext && !isNewLine(in.head)) in.next
          (COMMENT)
        } else if (in.readIfStartsWith('*')) {
          val emptyOrDoc = in.readIfStartsWith('*')
          val empty = emptyOrDoc && in.readIfStartsWith('/')
          val isDoc = emptyOrDoc && !empty

          if (isDoc)
            in.scratch setLength 0

          var count = 0
          if (!empty) while (count != -1) in.next match {
          case SU => in.incompleteError(offset, "unterminated comment"); count = -1
          case '*' if in.readIfStartsWith('/') => count -= 1
          case '/' if in.readIfStartsWith('*') => count += 1
          case c =>
            if (isDoc) in.scratch append c
          }
          if (!isDoc) (COMMENT) else value(COMMENT, in.scratch.toString)
        } else doOperator('/')
      case c @ ('~' | '!' | '@' | '#' | '%' |
                '^' | '*' | '+' | '-' | /* '<' | | '/' */
                '>' | '?' | ':' | '=' | '&' |
                '|' | '\\') => doOperator(c)
      case c @
       ('A' | 'B' | 'C' | 'D' | 'E' |
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
        'z') =>
        in.scratch.setLength(0)
        in.scratch.append(c : Char)
        getIdentRest
        val name = global.newTermName(in.scratch.toString)
        in.scratch.setLength(0)
        val code = name2token(name)
        if (code == IDENTIFIER) value(code, identifier(name))
        else value(code, (name))
      case '0' =>
        if (in.head match {
        case 'x' | 'X' => true
        case _ => false
        }) { in.next; value(getNumber(offset, 16, "0x"), 16) }
        else value(getNumber(offset, 8, "0"), 8)
      case '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => getNumber(offset, 10, "")
      case '.' =>
        val frac = getFraction(false)
        val code = (frac getOrElse DOT)
        code
      case '\'' =>
        def endQ(cvalue : Char) : Int = {
          if (!in.readIfStartsWith('\'')) {
            in.error(offset, "missing terminating quote")
          }
          value(CHARLIT, cvalue)
        }
        in.next match {
        case CR|LF|FF|SU|EOF if !in.isUnicode =>
          in.error(offset, "unterminated character literal")
          value(CHARLIT, 0.toChar)
        case '\'' =>
          in.error(offset, "empty character literal")
          value(CHARLIT, 0.toChar)
        case '\\' => endQ(escapeCode(offset))
        case c if (Character.isUnicodeIdentifierStart(c)) && in.head != '\'' =>
          in.scratch.setLength(0)
          in.scratch append c
          getIdentRest
          if (in.readIfStartsWith('\'')) in.error(offset, "unexpected quote after symbol")
          value(SYMBOLLIT, in.scratch.toString)
        case c if isSpecial(c) && in.head != '\'' =>
          in.scratch.setLength(0)
          in.scratch append(c)
          getOperatorRest
          if (in.readIfStartsWith('\'')) in.error(offset, "unexpected quote after symbol")
          value(SYMBOLLIT, in.scratch.toString)
        case c => endQ(c)
        }
      case '\"' =>
        if (in.readIfStartsWith('\"')) {
          if (in.readIfStartsWith('\"')) {
            // multiline
            in.scratch setLength 0
            while (in.next match {
            case SU if !in.isUnicode => in.incompleteError(offset, "unterminated multi-line string"); false
            case '\"' if in.readIfStartsWith('\"') =>
              if (in.readIfStartsWith('\"')) false
              else {
                in.scratch append "\"\""; true
              }
            case '\\' if false => // XXX: not for multi-line strings?
              in.scratch append escapeCode(in.offset - 1)
              true
            case c => in.scratch append c; true
            }) {}
            val ret = value(STRINGLIT, in.scratch.toString)
            in.scratch setLength 0
            ret
          } else value(STRINGLIT, "")
        } else {
          in.scratch setLength 0
          while (in.next match {
          case '\"' => false
          case CR|LF|FF|SU if !in.isUnicode =>
            in.error(offset, "unterminated string"); false
          case '\\' =>
            in.scratch append escapeCode(in.offset - 1); true
          case c => in.scratch.append(c); true
          }) {}
          val ret = value(STRINGLIT, in.scratch.toString)
          in.scratch setLength 0
          ret
        }
      case '`' =>
        in.scratch setLength 0
        if (in.head == '`') in.error(offset, "empty quoted identifier")
        while (in.head match {
        case '`' => in.next; false
        case CR | LF | FF | SU | EOF =>
        in.error(offset, "unterminated quoted identifier")
        false
        case _ => true
        }) in.scratch append in.next
        val name = global.newTermName(in.scratch.toString)
        value(BACKQUOTED_IDENT, (name))
      case c if (c == CR | c == LF | c == FF) =>
        var multiple = false
        in.readWhile{
        case d if isNewLine(d) =>
          multiple = multiple || d == c; true
        case ' ' | '\t' => true // eat the whitespace after newlines.
        case _ => false
        }
        xmlOk = true
        (if (multiple) NEWLINES else NEWLINE)
      case c =>
        if (Character.isUnicodeIdentifierStart(c)) {
          in.scratch.setLength(0)
          in.scratch append c
          getIdentRest
          val name = global.newTermName(in.scratch.toString)
          in.scratch.setLength(0)
          val code = name2token(name)
          value(code, (name))
        } else if (isSpecial(c)) {
          in.scratch.setLength(0)
          in.scratch append c
          getOperatorRest
          val name = global.newTermName(in.scratch.toString)
          in.scratch.setLength(0)
          val code = name2token(name)
          value(code, (name))
        } else {
          in.error(offset, "illegal character: \'" + c + "\'")
          (ERROR)
        }
      }
      current.length = length
    }
    def intVal(offset : Int, token : Int, name0 : RandomAccessSeq[Char], negated: Boolean, base : Int): Long = {
      if (name0.length == 1 && name0(0) == '0') return 0

      var name = name0
      if (name.length > 2 && name(0) == '0' && (name(1) match {
        case 'x'|'X' => true
        case _ => false
      })) name = name.drop(2)

      while (name.last match {
        case 'l'|'L' => true
        case _ => false
      }) name = name.take(name.length - 1)

      if (token == CHARLIT && !negated) {
        if (name.length > 0) name(0) else 0
      } else {
        var value: Long = 0
        val divider = if (base == 10) 1 else 2
        val limit: Long =
          if (token == LONGLIT) Math.MAX_LONG else Math.MAX_INT
        var i = 0
        val len = name.length
        while (i < len) {
          val d = digit2int(name(i), base)
          if (d < 0) {
            in.error(offset, "malformed integer number")
            return 0
          }
          if (value < 0 ||
              limit / (base / divider) < value ||
              limit - (d / divider) < value * (base / divider) &&
              !(negated && limit == value * base - 1 + d)) {
                in.error(offset, "integer number too large")
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
    def floatVal(offset : Int, token : Int, name0 : RandomAccessSeq[Char], negated: Boolean): Double = {
      var name = name0
      while (name.last match {
          case 'f'|'F'|'d'|'D' => true
          case _ => false
      }) name = name.take(name.length - 1)

      val limit: Double =
        if (token == DOUBLELIT) Math.MAX_DOUBLE else Math.MAX_FLOAT
      try {
        val value: Double = java.lang.Double.valueOf(name.mkString).doubleValue()
        if (value > limit)
          in.error(offset, "floating point number too large")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          in.error(offset, "malformed floating point number")
          0.0
      }
    }
  }


  // utility functions
  def isSpecial(c : Char) : Boolean = {
    val chtp = Character.getType(c)
    chtp == Character.MATH_SYMBOL || chtp == Character.OTHER_SYMBOL
  }
  def isDigit(c : Char) : Boolean = digit2int(c, 10) >= 0

  def isIdentifierStart(c: Char): Boolean = (c: @switch) match {
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
         'z' => true
    case _ => false
  }
  def isIdentifierPart(c: Char) : Boolean = (c: @switch) match {
    case ('A' | 'B' | 'C' | 'D' | 'E' |
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
       'z') => true
    case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => true
    case c => Character.isUnicodeIdentifierPart(c)
  }
  //isIdentifierStart(c) || isDigit(c) || isUnicodeIdentifierPart(c)
  def isOperatorPart(c : Char) : Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }

  private def getOperatorRest(implicit in : CoreScannerInput) : Unit = {
    in.readWhile{
    case ('/') if in.startsWith('/','*') || in.startsWith('/','/') => false
    case ('*') if in.startsWith('*','/') => false
    case (c) if isOperatorPart(c) => in.scratch append c; true
    case _ => false
    }
  }
  private def isFraction(c0 : Char, c1 : Char) =
    isDigit(c0) || (c0 match {
    case 'd'|'D'|'f'|'F' if !isIdentifierPart(c1) => true
    case 'e'|'E' if isDigit(c1) => true
    case _ => false
    })
  private def getFraction(hasWhole : Boolean)(implicit in : CoreScannerInput) : Option[Int] = {
    val hasDigits = in.readWhile(isDigit)
    if (!hasDigits && !hasWhole) return None

    def end(code : Int) : Option[Int] = {
      if (!hasDigits && isIdentifierPart(in.peek(1))) None
      else in.next; Some(code)
    }
    in.head match {
    case 'f'|'F' => return end(FLOATLIT)
    case 'd'|'D' => return end(DOUBLELIT)
    case 'e'|'E' if {
        val peek = in.peek(1)
        peek == '-' || peek == '+' || isDigit(peek)
    } =>
      in.next // eat the e.
      var hasDigit = isDigit(in.next) // eat +/-/digit
      hasDigit = in.readWhile(isDigit) || hasDigit
      in.readIf{ // eat an optional f or d
      case 'f'|'F' => FLOATLIT
      case 'd'|'D' => DOUBLELIT
      } orElse Some(DOUBLELIT)
    case _ if hasDigits => Some(DOUBLELIT)
    case _ => None // we didn't read anything
    }
  }
  private def getNumber(offset : Int, base : Int, prefix : String)(implicit in : CoreScannerInput) : Int = {
    val hasBody = in.readWhile{
    case at if at >= '0' && at <= '9' => true
    case at if base == 16 && ((at >= 'a' && at <= 'f') || (at >= 'A' && at <= 'F')) => true
    case _ => false
    }
    if (!hasBody) base match {
      // because Java does this
    case 16 =>
      in.error(offset, "Invalid hex literal number")
      return INTLIT
    case _ =>
    }
    val code = if (in.head == '.') {
      in.peek(1) match {
      case c if isDigit(c) => in.next; getFraction(true).get
      case 'f'|'F'|'d'|'D' if !isIdentifierPart(in.peek(2)) => in.next; getFraction(true).get
      case 'e'|'E' if {
        val peek = in.peek(2)
        isDigit(peek) || peek == '-' || peek == '+'
      } =>
        in.next // consume the dot
        in.next // consume the e
        in.next // consume the +/-/digit
        in.readWhile(isDigit) // consume remaining digits
        in.readIf{
        case 'f'|'F' => FLOATLIT
        case 'd'|'D' => DOUBLELIT
        } getOrElse DOUBLELIT
      case c if isIdentifierStart(c) => INTLIT
      case _ => in.next; DOUBLELIT
      }
    } else (in.readIf{
    case 'l'|'L' => LONGLIT
    case 'f'|'F' => FLOATLIT
    case 'd'|'D' => DOUBLELIT
    } getOrElse {
      if (in.head == 'e' || in.head == 'E') {
        in.next
        if (in.head == '-' || in.head == '+') in.next
        in.readWhile(isDigit)
        in.readIf{
        case 'f'|'F' => FLOATLIT
        case 'd'|'D' => DOUBLELIT
        } getOrElse DOUBLELIT
      } else INTLIT
    })
    if (in.readWhile(isIdentifierPart))
      in.error(offset, "Invalid literal number")
    code
  }
  // todo: add LBRACKET
  def inFirstOfStat(token: Int) = (token: @switch) match {
    case EOF | CATCH | ELSE | EXTENDS | FINALLY | MATCH | REQUIRES | WITH | YIELD |
         COMMA | SEMI | NEWLINE | NEWLINES | DOT | COLON | EQUALS | ARROW | LARROW |
         SUBTYPE | VIEWBOUND | SUPERTYPE | HASH | RPAREN | RBRACKET | RBRACE => false
    case _ => true
  }
  def inLastOfStat(token: Int) = (token: @switch) match {
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | SYMBOLLIT |
         IDENTIFIER | BACKQUOTED_IDENT | THIS | NULL | TRUE | FALSE | RETURN | USCORE |
         TYPE | XMLSTART | RPAREN | RBRACKET | RBRACE => true
    case _ => false
  }

  def digit(c : Char, radix : Int) = c match {
  case c if c >= '0' && c <= '7' => c - '0'
  case c if c >= '8' && c <= '9' && radix >= 10 => c - '0'
  case c if c >= '8' && c <= '9' && radix == 8 => throw new NumberFormatException("Malformed octal number")
  case c if c >= 'a' && c <= 'f' && radix == 16 => c - 'a' + 9
  case c if c >= 'A' && c <= 'F' && radix == 16 => c - 'A' + 9
  }
  private val simpleEscape : PartialFunction[Char,Char] = {
  case 'b' => '\b'
  case 't' => '\t'
  case 'n' => '\n'
  case 'f' => '\f'
  case 'r' => '\r'
  case '\"' => '\"'
  case '\'' => '\''
  case '\\' => '\\'
  }

  def digit2int(ch: Char, base: Int): Int = {
    if ('0' <= ch && ch <= '9' && ch < '0' + base)
      ch - '0'
    else if ('A' <= ch && ch < 'A' + base - 10)
      ch - 'A' + 10
    else if ('a' <= ch && ch < 'a' + base - 10)
      ch - 'a' + 10
    else
      -1
  }

  object ScannerConfiguration {
    private var key: Array[Byte] = _
    private var maxKey = 0
    private var tokenName = new Array[global.Name](128);
    {
      var tokenCount = 0
      // Enter keywords
      def enterKeyword(n: global.Name, tokenId: Int) {
        while (tokenId >= tokenName.length) {
          val newTokName = new Array[global.Name](tokenName.length * 2)
          Array.copy(tokenName, 0, newTokName, 0, newTokName.length)
          tokenName = newTokName
        }
        tokenName(tokenId) = n
        if (n.start > maxKey) maxKey = n.start
        if (tokenId >= tokenCount) tokenCount = tokenId + 1
      }
      import global.nme

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
      key = new Array[Byte](maxKey + 1)
      for (i <- 0 to maxKey)
        key(i) = IDENTIFIER
      for (j <- 0 until tokenCount)
        if (tokenName(j) ne null)
          key(tokenName(j).start) = j.asInstanceOf[Byte]

    }
//Token representation -----------------------------------------------------

    /** Convert name to token */
    def name2token(name: global.Name): Int =
      if (name.start <= maxKey) key(name.start) else IDENTIFIER

    def isKeyword(code : Int) = code match {
      case code if code >= IF && code <= REQUIRES => true
      case _ => false
    }

    /** Returns the string representation of given token. */
    def token2string(token: Int): String = token match {
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
      case CASECLASS =>
        "case class"
      case CASEOBJECT =>
        "case object"
      case XMLSTART =>
        "$XMLSTART$<"
      case COMMENT => "cmnt"
      case WHITESPACE => "ws"
      case IGNORE => "ig"
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
  }

  class UnitScanner(unit: CompilationUnit) extends ParserScanner {
    implicit val in =
      new DefaultInput(new NewCharArrayReader(unit.source.content, !settings.nouescape.value, error)) {
        override def error(offset : Int, msg : String) : Unit = UnitScanner.this.error(offset, msg)
        override def incompleteError(offset : Int, msg : String) =
          unit.incompleteInputError(new OffsetPosition(unit.source, offset), msg)
      }
    init
    private def error(offset : Int, msg : String) : Unit = unit.error(new OffsetPosition(unit.source,offset), msg)
  }
}
