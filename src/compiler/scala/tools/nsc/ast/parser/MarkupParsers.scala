/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id: MarkupParsers.scala 17754 2009-05-18 10:54:00Z milessabin $

package scala.tools.nsc
package ast.parser

import scala.collection.mutable
import scala.util.control.ControlException
import scala.tools.nsc.util.{Position,NoPosition,SourceFile,CharArrayReader}
import scala.xml.{Text, TextBuffer}
import SourceFile.{SU,LF}
import scala.annotation.switch

/** This trait ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait MarkupParsers {self: Parsers =>
  case object MissingEndTagException extends RuntimeException with ControlException {
    override def getMessage = "start tag was here: "
  }

  case object ConfusedAboutBracesException extends RuntimeException with ControlException {
    override def getMessage = " I encountered a '}' where I didn't expect one, maybe this tag isn't closed <"
  }

  case object TruncatedXML extends RuntimeException with ControlException {
    override def getMessage = "input ended while parsing XML"
  }

  import global._

  class MarkupParser(parser: UnitParser, presWS: Boolean) /*with scala.xml.parsing.MarkupParser[Tree,Tree] */{

    import Tokens.{EMPTY, LBRACE, RBRACE}

    final val preserveWS = presWS
    var input : CharArrayReader = _

    import parser.{symbXMLBuilder => handle, o2p, r2p}

    def curOffset : Int = input.charOffset - 1
    var tmppos : Position = NoPosition
    def ch = input.ch
    /** this method assign the next character to ch and advances in input */
    def nextch = { val result = input.ch; input.nextChar(); result } // { s.in.next; /*s.xNext;*/ ch = s.in.ch ; pos = s.in.cpos }

    var xEmbeddedBlock = false

    /** munch expected XML token, report syntax error for unexpected.
     *
     *  @param that ...
     */
    /*[Duplicate]*/ def xToken(that: Char) {
      if (ch == that) nextch
      else if (ch == SU)
	throw TruncatedXML
      else reportSyntaxError("'" + that + "' expected instead of '" + ch + "'")
    }

    var debugLastStartElement = new mutable.Stack[(Int, String)]

    /** checks whether next character starts a Scala block, if yes, skip it.
     * @return true if next character starts a scala block
     */
    /*[Duplicate]*/ def xCheckEmbeddedBlock: Boolean = {
      // attentions, side-effect, used in xText
      xEmbeddedBlock = (ch == '{') && { nextch; (ch != '{') }
      //Console.println("pos = "+pos+" xEmbeddedBlock returns "+xEmbeddedBlock)
      xEmbeddedBlock
    }

    /** parse attribute and add it to listmap
     *  [41] Attributes   ::= { S Name Eq AttValue }
     *       AttValue     ::= `'` { _  } `'`
     *                      | `"` { _ } `"`
     *                      | `{` scalablock `}`
     */
    /*[Duplicate]*/ def xAttributes = {
      var aMap = new mutable.HashMap[String, Tree]()
      while (xml.Parsing.isNameStart(ch)) {
        val start = curOffset
        val key = xName
        xEQ
        val delim = ch
        val mid = curOffset
        val value: /* AttribValue[*/Tree/*]*/ = ch match {
          case '"' | '\'' =>
            nextch
            val tmp = xAttributeValue(delim)
            nextch
            try {
              handle.parseAttribute(r2p(start, mid, curOffset), tmp)
            } catch {
              case e =>
                reportSyntaxError("error parsing attribute value")
                parser.errorTermTree
            }

          case '{'  =>
            nextch
            xEmbeddedExpr
          case SU =>
            throw TruncatedXML
          case _ =>
            reportSyntaxError("' or \" delimited attribute value" +
                              " or '{' scala-expr '}' expected" )
            Literal(Constant("<syntax-error>"))
        }
        // well-formedness constraint: unique attribute names
        if (aMap.contains(key)) {
          reportSyntaxError( "attribute "+key+" may only be defined once" )
        }
        aMap.update(key, value)
        if ((ch != '/') && (ch != '>')) {
          xSpace
        }
      }
      aMap
    }

    /** attribute value, terminated by either ' or ". value may not contain <.
     *  @param endch either ' or "
     */
    /*[Duplicate]*/ def xAttributeValue(endCh: Char): String = {
      val buf = new StringBuilder
      while (ch != endCh) {
        if (ch == SU)
          throw TruncatedXML
        buf append ch
        nextch
      }
      val str = buf.toString()

      // @todo: normalize attribute value
      // well-formedness constraint
      if (str.indexOf('<') != -1) {
        reportSyntaxError( "'<' not allowed in attrib value" ); ""
      } else {
        str
      }
    }

    /** parse a start or empty tag.
     *  [40] STag         ::= '<' Name { S Attribute } [S]
     *  [44] EmptyElemTag ::= '<' Name { S Attribute } [S]
     */
    /*[Duplicate]*/ def xTag: (String, mutable.Map[String, Tree]) = {
      val elemName = xName
      xSpaceOpt
      val aMap =
        if (xml.Parsing.isNameStart(ch)) xAttributes
        else new mutable.HashMap[String, Tree]()
      (elemName, aMap)
    }

    /** [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'
     */
    /*[Duplicate]*/ def xEndTag(startName: String) {
      xToken('/')
      val endName = xName
      if (endName != startName) {
        reportSyntaxError("expected closing tag of " + startName)
        throw MissingEndTagException
      }
      xSpaceOpt
      xToken('>')
    }

    /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
     *
     * see [15]
     */
    /*[Duplicate]*/ def xCharData: Tree = {
      val start = curOffset
      xToken('[')
      xToken('C')
      xToken('D')
      xToken('A')
      xToken('T')
      xToken('A')
      xToken('[')
      val mid = curOffset
      val sb: StringBuilder = new StringBuilder()
      while (true) {
        if (ch==']' &&
           { sb.append(ch); nextch; ch == ']' } &&
           { sb.append(ch); nextch; ch == '>' }) {
          sb.length = sb.length - 2
          nextch
          return handle.charData(r2p(start, mid, curOffset), sb.toString())
        } else if (ch == SU)
          throw TruncatedXML
        else
          sb.append(ch)
        nextch
      }
      Predef.error("this cannot happen")
    }

    def xUnparsed: Tree = {
      val start = curOffset
      val sb: StringBuilder = new StringBuilder()
      while (true) {
        if (ch=='<' &&
           { sb.append(ch); nextch; ch == '/' } &&
           { sb.append(ch); nextch; ch == 'x' } &&
           { sb.append(ch); nextch; ch == 'm' } &&
           { sb.append(ch); nextch; ch == 'l' } &&
           { sb.append(ch); nextch; ch == ':' } &&
           { sb.append(ch); nextch; ch == 'u' } &&
           { sb.append(ch); nextch; ch == 'n' } &&
           { sb.append(ch); nextch; ch == 'p' } &&
           { sb.append(ch); nextch; ch == 'a' } &&
           { sb.append(ch); nextch; ch == 'r' } &&
           { sb.append(ch); nextch; ch == 's' } &&
           { sb.append(ch); nextch; ch == 'e' } &&
           { sb.append(ch); nextch; ch == 'd' } &&
           { sb.append(ch); nextch; ch == '>' }) {
          sb.length = sb.length - "</xml:unparsed".length
          nextch
          return handle.unparsed(r2p(start, start, curOffset), sb.toString())
        } else if (ch == SU) {
          throw TruncatedXML
        } else sb.append(ch)
        nextch
      }
      Predef.error("this cannot happen")
    }

    /** CharRef ::= "&#" '0'..'9' {'0'..'9'} ";"
     *            | "&#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
     *
     * see [66]
     */
    /*[Duplicate]*/ def xCharRef: String = {
      val hex = (ch == 'x') && { nextch; true }
      val base = if (hex) 16 else 10
      var i = 0
      while (ch != ';') {
        (ch: @switch) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            i = i * base + ch.asDigit
          case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
             | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
            if (!hex)
              reportSyntaxError("hex char not allowed in decimal char ref\n"
                           +"Did you mean to write &#x ?");
            else
              i = i * base + ch.asDigit
          case SU =>
            throw TruncatedXML
          case _ =>
            reportSyntaxError("character '"+ch+"' not allowed in char ref")
        }
        nextch
      }
      new String(Array(i.asInstanceOf[Char]))
    }

    /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
     *
     * see [15]
     */
    /*[Duplicate]*/ def xComment: Tree = {
      val start = curOffset - 2 // ? right so ?
      val sb: StringBuilder = new StringBuilder()
      xToken('-')
      xToken('-')
      while (true) {
        if (ch=='-'  && { sb.append(ch); nextch; ch == '-' }) {
          sb.length = sb.length - 1
          nextch
          xToken('>')
          return handle.comment(r2p(start, start, curOffset), sb.toString())
        } else if (ch == SU) {
          throw TruncatedXML
        } else sb.append(ch)
        nextch
      }
      Predef.error("this cannot happen")
    }

    /**
     *  @param pos ...
     *  @param ts  ...
     *  @param txt ...
     */
    /*[Duplicate]*/ def appendText(pos: Position, ts: mutable.Buffer[Tree],
                                   txt: String) {
      if (!preserveWS) {
        for (t <- TextBuffer.fromString(txt).toText) {
          ts.append(handle.text(pos, t.text))
        }
      }
      else
        ts.append( handle.text(pos, txt))
    }

    /** adds entity/character to to ts as side-effect
     *  @precond ch == '&amp;'
     */
    def content_AMP(ts: mutable.ArrayBuffer[Tree]) {
      nextch
      ch match {
        case '#' => // CharacterRef
          nextch
          val theChar = handle.text(tmppos, xCharRef)
          xToken(';')
          ts.append(theChar)
        case _ => // EntityRef
          val n = xName
          xToken(';')
          ts.append(handle.entityRef(tmppos, n))
      }
    }

    /**
     *  @precond ch == '{'
     *  @postcond: xEmbeddedBlock == false!
     */
    def content_BRACE(p: Position, ts:mutable.ArrayBuffer[Tree]) {
      if (xCheckEmbeddedBlock)
        ts.append(xEmbeddedExpr)
      else {
        appendText(p, ts, xText)/*
        val str = new StringBuilder("{")
        str.append(xText)
        nextch
        appendText(p, ts, str.toString())*/
      }
    }

    /** Returns true if it encounters an end tag (without consuming it),
     *  appends trees to ts as side-effect.
     *
     *  @param ts ...
     *  @return   ...
     */
    private def content_LT(ts: mutable.ArrayBuffer[Tree]): Boolean = {
      ch match {
        case '/' =>
          return true               // end tag
        case '!' =>
          nextch                    // CDATA or Comment
          ts.append(if ('[' == ch) xCharData else xComment)
        case '?' =>                 // PI
          nextch
          ts.append(xProcInstr)
        case _   =>
          ts.append(element)        // child node
      }
      false
    }

    /*[Duplicate]*/ def content: mutable.Buffer[Tree] = {
      var ts = new mutable.ArrayBuffer[Tree]
      var exit = false
      while (!exit) {
        if (xEmbeddedBlock)
          ts.append(xEmbeddedExpr)
        else {
          tmppos = o2p(curOffset)
          ch match {
            case '<' => // end tag, cdata, comment, pi or child node
              nextch
              exit = content_LT(ts)
            case '{' => // either the character '{' or an embedded scala block
              content_BRACE(tmppos, ts)
            case '&' => // EntityRef or CharRef
              content_AMP(ts)
            case SU =>
              exit = true
            case _ =>  // text content
              appendText(tmppos, ts, xText)
              // here xEmbeddedBlock might be true
          }
        }
      }
      ts
    }

    /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
     *                | xmlTag1 '/' '>'
     */
    /*[Duplicate]*/ def element: Tree = {
      val start = curOffset
      val (qname, attrMap) = xTag
      if (ch == '/') { // empty element
        xToken('/')
        xToken('>')
        handle.element(r2p(start, start, curOffset), qname, attrMap, new mutable.ListBuffer[Tree])
      }
      else { // handle content
        xToken('>')
        if (qname == "xml:unparsed")
          return xUnparsed

        debugLastStartElement.push((start, qname))
        val ts = content
        xEndTag(qname)
        debugLastStartElement.pop
        val pos = r2p(start, start, curOffset)
        qname match {
          case "xml:group" => handle.group(pos, ts)
          case _ => handle.element(pos, qname, attrMap, ts)
        }
      }
    }

    /** actually, Name ::= (Letter | '_' | ':') (NameChar)*  but starting with ':' cannot happen
     *  Name ::= (Letter | '_') (NameChar)*
     *
     *  see  [5] of XML 1.0 specification
     *
     *  pre-condition:  ch != ':' // assured by definition of XMLSTART token
     *  post-condition: name does neither start, nor end in ':'
     */
    /*[Duplicate]*/   def xName: String = {
      if (ch == SU) {
	throw TruncatedXML
      } else if ( !xml.Parsing.isNameStart(ch)) {
        reportSyntaxError("name expected, but char '"+ch+"' cannot start a name")
        return ""
      }
      val buf = new StringBuilder
      do {
        buf append ch; nextch
      } while (xml.Parsing.isNameChar(ch))
      if (':' == buf.last) {
        reportSyntaxError( "name cannot end in ':'" )
        buf.setLength(buf.length - 1)
      }
      val n = buf.toString().intern()
      //cbuf.length = 0
      n
    }

    /** scan [S] '=' [S]*/
    /*[Duplicate]*/   def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

    /** skip optional space S? */
    /*[Duplicate]*/   def xSpaceOpt = { while (xml.Parsing.isSpace(ch)) { nextch }}

    /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
    /*[Duplicate]*/ def xSpace =
      if (xml.Parsing.isSpace(ch)) { nextch; xSpaceOpt }
      else if (ch == SU)
        throw TruncatedXML
      else reportSyntaxError("whitespace expected")

    /** '<?' ProcInstr ::= Name [S ({Char} - ({Char}'>?' {Char})]'?>'
     *
     * see [15]
     */
    /*[Duplicate]*/ def xProcInstr: Tree = {
      val sb: StringBuilder = new StringBuilder()
      val n = xName
      if (xml.Parsing.isSpace(ch)) {
        xSpace
        while (true) {
          if (ch == '?' && { sb.append(ch); nextch; ch == '>' }) {
            sb.length = sb.length - 1
            nextch
            return handle.procInstr(tmppos, n, sb.toString)
          } else
            sb.append(ch);
          nextch
        }
      }
      xToken('?')
      xToken('>')
      handle.procInstr(tmppos, n, sb.toString)
    }

    /** parse character data.
     *  precondition: xEmbeddedBlock == false (we are not in a scala block)
     */
    /*[Duplicate]*/ def xText: String = {
      if (xEmbeddedBlock) Predef.error("internal error: encountered embedded block"); // assert
      //Console.println("xText ch now "+ch)
      //if( xCheckEmbeddedBlock ) {
      //  return ""
      //} else {
        var exit = false
        val buf = new StringBuilder
        while (!exit && (ch!=SU)) {
          val expectRBRACE = ch == '}'
          if (expectRBRACE) {
            val la = input.lookaheadReader
            la.nextChar()
            if (la.ch != '}') {
              reportSyntaxError("in XML content, please use '}}' to express '}'")
              throw ConfusedAboutBracesException
            }
            nextch
          }
          buf append ch
          nextch
          exit = xCheckEmbeddedBlock ||(ch == '<') || (ch == '&')
        }
        val str = buf.toString()
        //cbuf.length = 0
        str
      //}
    }

    //val cbuf = new StringBuilder()

    /** append Unicode character to name buffer*/
    //private def putChar(c: char) = cbuf.append(c)

    /** xLiteral = element { element }
     *  @return Scala representation of this xml literal
     *  precondition: s.xStartsXML == true
     */
    def xLiteral: Tree = try {
      input = parser.in
      handle.isPattern = false

      //val pos = s.currentPos
      var tree:Tree = null
      val ts = new mutable.ArrayBuffer[Tree]()
      val start = curOffset
      tmppos = o2p(curOffset)    // Iuli: added this line, as it seems content_LT uses tmppos when creating trees
//      assert(ch == '<')
//      nextch
      content_LT(ts)
      //Console.println("xLiteral:ts = "+ts.toList)
      //lastend = s.in.bp
      //lastch  = s.in.ch
      //if (settings.debug.value) {
      //  Console.println("DEBUG 1: I am getting char '"+ch+"' at lastend "+lastend+" pos = "+pos); // DEBUG
      //}
      input = input.lookaheadReader
      xSpaceOpt
      // parse more XML ?
      if (ch == '<') {
        input = parser.in
        xSpaceOpt
        while (ch == '<') {
          nextch
          ts.append(element)
          xSpaceOpt
        }
        tree = handle.makeXMLseq(r2p(start, start, curOffset), ts)
      } else {
        input = parser.in
        assert(ts.length == 1)
        tree = ts(0)
      }
      tree
    } catch {
      case c @ TruncatedXML =>
        parser.incompleteInputError(c.getMessage)
        EmptyTree

      case c @ (MissingEndTagException | ConfusedAboutBracesException) =>
        parser.syntaxError((debugLastStartElement.top._1):Int,
                           c.getMessage + debugLastStartElement.top._2+">")
        EmptyTree

      case _:ArrayIndexOutOfBoundsException =>
        parser.syntaxError((debugLastStartElement.top._1),
                      "missing end tag in XML literal for <"
                      +debugLastStartElement.top._2+">");
        EmptyTree
    } finally {
      parser.in.resume(Tokens.XMLSTART)
    }

    /** @see xmlPattern. resynchronizes after successful parse
     *  @return this xml pattern
     *  precondition: s.xStartsXML == true
     */
    def xLiteralPattern: Tree = try {
      input = parser.in
      val oldMode = handle.isPattern;
      handle.isPattern = true
//      assert(ch == '<')
//      nextch
      var tree = xPattern; xSpaceOpt;
      handle.isPattern = oldMode;
      tree
    } catch {
      case c @ TruncatedXML =>
        parser.syntaxError(curOffset, c.getMessage)
        EmptyTree

      case c @ (MissingEndTagException | ConfusedAboutBracesException) =>
        parser.syntaxError((debugLastStartElement.top._1),
                      c.getMessage + debugLastStartElement.top._2+">")
        EmptyTree

      case _:ArrayIndexOutOfBoundsException =>
        parser.syntaxError((debugLastStartElement.top._1),
                      "missing end tag in XML literal for <"
                      +debugLastStartElement.top._2+">")
        EmptyTree
    } finally {
      parser.in.resume(Tokens.XMLSTART)
    }

    def escapeToScala[A](op: => A, kind: String) = {
      xEmbeddedBlock = false
      val savedSepRegions = parser.in.sepRegions
      parser.in.resume(LBRACE)
      try {
        op //p.expr(true,false);
      } finally {
        parser.in.sepRegions = savedSepRegions // parser.in.sepRegions.tail
        if (parser.in.token != RBRACE) {
          reportSyntaxError(" expected end of Scala "+kind)
        }
      }
    }

    def xEmbeddedExpr: Tree = escapeToScala(parser.block(), "block")

    /** xScalaPatterns  ::= patterns
     */
    def xScalaPatterns: List[Tree] = escapeToScala(parser.patterns(true), "pattern")

    var scannerState: List[List[Int]] = Nil

/*
    private def pushScannerState {
      scannerState = s.sepRegions :: scannerState
      s.sepRegions = Nil
    }
    private def popScannerState {
      s.sepRegions = scannerState.head
      scannerState = scannerState.tail
    }
    */
/*
    private def init {
      ch = s.in.ch
      curOffset = s.in.ccurOffset
    }
    */

    def reportSyntaxError(str: String) = {
      parser.syntaxError(curOffset, "in XML literal: " + str)
      nextch
    }
/*
    private def sync {
      xEmbeddedBlock = false
      s.xSync
    }
*/
    /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
     *                  | Name [S] '/' '>'
     */
    def xPattern: Tree = {
      var start = curOffset
      val qname = xName
      debugLastStartElement.push((start, qname))
      xSpaceOpt
      if (ch == '/') { // empty tag
        nextch
        xToken('>')
        return handle.makeXMLpat(r2p(start, start, curOffset), qname, new mutable.ArrayBuffer[Tree]())
      }

      // else: tag with content
      xToken('>')
      var ts = new mutable.ArrayBuffer[Tree]
      var exit = false
      while (! exit) {
        val start1 = curOffset
        if (xEmbeddedBlock) {
          ts ++= xScalaPatterns
        } else
          ch match {
            case '<' => // tag
              nextch
              if (ch != '/') { //child
                ts.append(xPattern)
              } else {
                exit = true
              }
            case '{' => // embedded Scala patterns
              while (ch == '{') {
                nextch
                ts ++= xScalaPatterns
              }
              // postcond: xEmbeddedBlock = false;
              if (xEmbeddedBlock) Predef.error("problem with embedded block"); // assert

            case SU =>
              throw TruncatedXML

            case _ => // text
              appendText(r2p(start1, start1, curOffset), ts, xText)
              // here  xEmbeddedBlock might be true;
              //if( xEmbeddedBlock ) throw new ApplicationError("after:"+text); // assert
          }
      }
      xEndTag(qname)
      debugLastStartElement.pop
      handle.makeXMLpat(r2p(start, start, curOffset), qname, ts)
    }

  } /* class MarkupParser */
}
