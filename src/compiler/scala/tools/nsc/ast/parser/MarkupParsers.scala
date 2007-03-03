/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.ast.parser

import compat.StringBuilder
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.tools.nsc.util.Position
import scala.xml.{Text, TextBuffer}

/** This trait ...
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
trait MarkupParsers requires SyntaxAnalyzer {

  import global._
  //import posAssigner.atPos

class MarkupParser(unit: CompilationUnit, s: Scanner, p: Parser, presWS: boolean) /*with scala.xml.parsing.MarkupParser[Tree,Tree] */{

  import Tokens.{EMPTY, LBRACE, RBRACE}

  final val preserveWS = presWS;

  import p.{symbXMLBuilder => handle}
  import s.token

  /** holds the position in the source file */
  /*[Duplicate]*/ var pos: Int = _

  /** holds temporary values of pos */
  /*[Duplicate]*/ var tmppos: Int = _

  /** holds the next character */
  /*[Duplicate]*/ var ch: Char = _

  /** character buffer, for names */
  /*[Duplicate]*/ protected val cbuf = new StringBuilder()

  /** append Unicode character to name buffer*/
  /*[Duplicate]*/ protected def putChar(c: char) = cbuf.append(c)

  /*[Duplicate]*/ var xEmbeddedBlock = false

  /** munch expected XML token, report syntax error for unexpected.
   *
   *  @param that ...
   */
  /*[Duplicate]*/ def xToken(that: Char): Unit =
    if (ch == that)
      nextch
    else
      reportSyntaxError("'" + that + "' expected instead of '" + ch + "'")

  var debugLastStartElement = new mutable.Stack[(Int, String)]

  /** checks whether next character starts a Scala block, if yes, skip it.
   * @return true if next character starts a scala block
   */
  /*[Duplicate]*/ def xCheckEmbeddedBlock:Boolean = {
    // attentions, side-effect, used in xText
    xEmbeddedBlock = ( ch == '{' ) && { nextch;( ch != '{' ) }
    //Console.println("pos = "+pos+" xEmbeddedBlock returns "+xEmbeddedBlock)
    return xEmbeddedBlock;
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  /*[Duplicate]*/ def xAttributes = {
    var aMap = new mutable.HashMap[String, Tree]()
    while (xml.Parsing.isNameStart(ch)) {
      val key = xName
      xEQ
      val delim = ch
      val pos1 = pos
      val value: /* AttribValue[*/Tree/*]*/ = ch match {
        case '"' | '\'' =>
          nextch
          val tmp = xAttributeValue(delim)
          nextch
          try {
            handle.parseAttribute(pos1, tmp)
          } catch {
            case e =>
              reportSyntaxError("error parsing attribute value")
              p.errorTermTree
          }

        case '{'  =>
          nextch
          xEmbeddedExpr
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
  /*[Duplicate]*/ def xAttributeValue(endCh: char): String = {
    while (ch != endCh) {
      putChar(ch);
      nextch;
    };
    val str = cbuf.toString();
    cbuf.setLength(0);
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
    Tuple2(elemName, aMap)
  }

  /* [42]  '<' xmlEndTag ::=  '<' '/' Name S? '>'                 */
  /*[Duplicate]*/ def xEndTag(n: String) = {
    xToken('/')
    val m = xName
    if (n != m)
      reportSyntaxError( "expected closing tag of " + n/* +", not "+m*/);
    xSpaceOpt
    xToken('>')
  }

 /** '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
   *
   * see [15]
   */
  /*[Duplicate]*/ def xCharData: Tree = {
    xToken('[')
    xToken('C')
    xToken('D')
    xToken('A')
    xToken('T')
    xToken('A')
    xToken('[')
    val pos1 = pos
    val sb: StringBuilder = new StringBuilder()
    while (true) {
      if (ch==']' &&
         { sb.append(ch); nextch; ch == ']' } &&
         { sb.append(ch); nextch; ch == '>' }) {
        sb.setLength(sb.length() - 2)
        nextch
        return handle.charData(pos1, sb.toString())
      } else sb.append(ch)
      nextch
    }
    Predef.error("this cannot happen")
  }

  def xUnparsed: Tree = {
    val pos1 = pos
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
        sb.setLength(sb.length - "</xml:unparsed".length)
        nextch
        return handle.unparsed(pos1, sb.toString())
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
  /*[Duplicate]*/ def xCharRef:String = {
    val hex = (ch == 'x') && { nextch; true }
    val base = if (hex) 16 else 10
    var i = 0
    while (ch != ';') {
      ch match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + ch.asDigit
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (!hex)
            reportSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + ch.asDigit
        case _ =>
          reportSyntaxError("character '"+ch+" not allowed in char ref\n")
      }
      nextch
    }
    new String(Array(i.asInstanceOf[char]))
  }

  /** Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * see [15]
   */
  /*[Duplicate]*/ def xComment: Tree = {
    val sb: StringBuilder = new StringBuilder()
    xToken('-')
    xToken('-')
    while (true) {
      if( ch=='-'  && { sb.append(ch); nextch; ch == '-' } ) {
        sb.setLength(sb.length() - 1);
        nextch
        xToken('>')
        return handle.comment(pos, sb.toString())
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
  /*[Duplicate]*/ def appendText(pos: int, ts: mutable.Buffer[Tree],
                                 txt: String): Unit =
    if (!preserveWS) {
      for (val t <- TextBuffer.fromString(txt).toText) {
        ts.append(handle.text(pos, t.text))
      }
    }
    else
      ts.append( handle.text(pos, txt))

  /** adds entity/character to to ts as side-effect
   *  @precond ch == '&amp;'
   */
  def content_AMP(ts: mutable.ArrayBuffer[Tree]): Unit = {
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
  def content_BRACE(p: Int, ts:mutable.ArrayBuffer[Tree]): Unit = {
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
  def content_LT(ts: mutable.ArrayBuffer[Tree]): Boolean = {
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
        tmppos = pos;
        ch match {
          case '<' => // end tag, cdata, comment, pi or child node
            nextch
            exit = content_LT(ts)
          case '{' => // either the character '{' or an embedded scala block
            content_BRACE(tmppos, ts)
          case '&' => // EntityRef or CharRef
            content_AMP(ts)
          case _ =>  // text content
            appendText(tmppos, ts, xText)
          // here xEmbeddedBlock might be true
        }
      }
    }
    ts
  } /* end content */

  /** '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   */
  /*[Duplicate]*/ def element: Tree = {
    val pos1 = pos
    val Tuple2(qname, attrMap) = xTag
    //Console.println("MarkupParser::element("+qname+","+attrMap+")");
    if (ch == '/') { // empty element
      xToken('/')
      xToken('>')
      handle.element(pos1, qname, attrMap, new mutable.ListBuffer[Tree])
    }
    else { // handle content
      xToken('>')
      if(qname == "xml:unparsed")
        return xUnparsed

      debugLastStartElement.push((pos1, qname))
      val ts = content
      xEndTag(qname)
      debugLastStartElement.pop
      qname match {
        case "xml:group"   => handle.group(pos1, ts)
        case _ => handle.element(pos1, qname, attrMap, ts)
      }
    }
  }


  /** actually, Name ::= (Letter | '_' | ':') (NameChar)*  but starting with ':' cannot happen
   ** Name ::= (Letter | '_') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   *
   *  pre-condition:  ch != ':' // assured by definition of XMLSTART token
   *  post-condition: name does neither start, nor end in ':'
   */
  /*[Duplicate]*/   def xName: String = {
    if( !xml.Parsing.isNameStart( ch ) ) {
      reportSyntaxError( "name expected, but char '"+ch+"' cannot start a name" )
      return ""
    }
    do {
        putChar( ch )
        nextch
    } while( xml.Parsing.isNameChar( ch ) )
	if(':' == cbuf.charAt(cbuf.length-1)) {
      reportSyntaxError( "name cannot end in ':'" )
      cbuf.setLength(cbuf.length-1)
	}
    val n = cbuf.toString().intern()
    cbuf.setLength( 0 )
    n
  }


  /** scan [S] '=' [S]*/
  /*[Duplicate]*/   def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

  /** skip optional space S? */
  /*[Duplicate]*/   def xSpaceOpt = { while( xml.Parsing.isSpace( ch ) ) { nextch; }}

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  /*[Duplicate]*/ def xSpace = {
    if (xml.Parsing.isSpace(ch)) {
      nextch; xSpaceOpt
    }
    else {
      reportSyntaxError("whitespace expected");
    }
  }

/** '<?' ProcInstr ::= Name [S ({Char} - ({Char}'>?' {Char})]'?>'
   *
   * see [15]
   */
  /*[Duplicate]*/   def xProcInstr: Tree = {
    val sb:StringBuilder = new StringBuilder();
    val n = xName;
    if( xml.Parsing.isSpace( ch ) ) {
      xSpace;
      while( true ) {
        if( ch=='?' && { sb.append( ch ); nextch; ch == '>' } ) {
          sb.setLength( sb.length() - 1 );
          nextch;
          return handle.procInstr(tmppos, n.toString(), sb.toString());
        } else
          sb.append( ch );
        nextch;
      }
    };
    xToken('?');
    xToken('>');
    return handle.procInstr(tmppos, n.toString(), sb.toString());
  }

  /** parse character data.
  *   precondition: xEmbeddedBlock == false (we are not in a scala block)
  */
  /*[Duplicate]*/ def xText: String = {
    if( xEmbeddedBlock ) Predef.error("internal error: encountered embedded block"); // assert
    //Console.println("xText ch now "+ch)
    //if( xCheckEmbeddedBlock ) {
    //  return ""
    //} else {
      var exit = false;
      while( !exit ) {
        putChar( ch );
        val expectRBRACE = ch == '}'
        // TODO check for "}}"
        nextch
        if(expectRBRACE) {
          if(ch == '}')
            nextch
          else
            reportSyntaxError("in XML content, please use '}}' to express '}'")
        }
        exit = xCheckEmbeddedBlock ||( ch == '<' ) || ( ch == '&' );
      }
      val str = cbuf.toString();
      cbuf.setLength( 0 );
      str
    //}
  }

  //val cbuf = new StringBuilder();

  /** append Unicode character to name buffer*/
  //private def putChar(c: char) = cbuf.append( c );

  /** xLiteral = element { element }
   * @return Scala representation of this xml literal
   * precondition: s.xStartsXML == true
  */
  def xLiteral: Tree = try {
    //Console.println("entering xLiteral!!")
    init; pushScannerState
    handle.isPattern = false
    val pos = s.currentPos
    var lastend = 0
    var lastch  = ch
    //var tree = element;
    var tree:Tree = null
    val ts = new mutable.ArrayBuffer[Tree]()
    content_LT(ts)
    //Console.println("xLiteral:ts = "+ts.toList)
    lastend = s.in.bp
    lastch  = s.in.ch
    //if (settings.debug.value) {
    //  Console.println("DEBUG 1: I am getting char '"+ch+"' at lastend "+lastend+" pos = "+pos); // DEBUG
    //}
    xSpaceOpt
    // parse more XML ?
    if (ch == '<') {
      //val ts = new mutable.ArrayBuffer[Tree]();
      //ts.append( tree );
      while( ch == '<' ) {
        nextch
        ts.append( element )
        lastend = s.in.bp
        lastch  = s.in.ch
        xSpaceOpt
      }
      tree = handle.makeXMLseq( pos, ts )
    } else {
      assert(ts.length == 1)
      tree = ts(0)
    }
    s.in.bp = lastend // ugly hack
    s.in.ch = lastch
    //if (settings.debug.value) {
    //  Console.println("DEBUG 2: restoring char '"+lastch+"' at lastend "+lastend+" pos = "+pos); // DEBUG
    //}
    //Console.println("out of xLiteral, parsed:"+tree.toString());
    s.next.token = Tokens.EMPTY;
    s.nextToken()
    popScannerState
    tree
  }
  catch {
    case _:ArrayIndexOutOfBoundsException =>
      s.syntaxError(debugLastStartElement.top._1,
                    "missing end tag in XML literal for <"
                    +debugLastStartElement.top._2+">");
      EmptyTree;
  }

  /** @see xmlPattern. resynchronizes after succesful parse
   * @return this xml pattern
   * precondition: s.xStartsXML == true
  */
  def xLiteralPattern:Tree = try {
    init; pushScannerState
    val oldMode = handle.isPattern;
    handle.isPattern = true;
    var tree = xPattern; xSpaceOpt;
    handle.isPattern = oldMode;
    s.next.token = Tokens.EMPTY;
    s.nextToken()
    popScannerState
    tree
  } catch {
    case _:ArrayIndexOutOfBoundsException =>
      s.syntaxError(debugLastStartElement.top._1,
                    "missing end tag in XML literal for <"
                    +debugLastStartElement.top._2+">");
      EmptyTree;
  }

  def xEmbeddedExpr: Tree = {
    sync;
    val b = p.block() //p.expr(true,false);
    if(/*s.*/token != RBRACE) {
      reportSyntaxError(" expected end of Scala block");
    }
    init;
    return b
  }

  /** xScalaPatterns  ::= patterns
   */
  def xScalaPatterns: List[Tree] = {
    sync;
    val b = p.patterns(true);
    if (/*s.*/token != RBRACE) {
      reportSyntaxError(" expected end of Scala patterns");
    }
    init;
    return b
  }

  //var ch: Char = _;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit = { s.in.next; /*s.xNext;*/ ch = s.in.ch ; pos = s.in.cpos; }

  //def lookahead = { s.xLookahead }
  var scannerState: List[List[Int]] = Nil

  def pushScannerState {
    scannerState = s.sepRegions::scannerState
    s.sepRegions = Nil
  }
  def popScannerState  {
    s.sepRegions = scannerState.head;
    scannerState = scannerState.tail
  }

  def init: Unit = {
    ch = s.in.ch;
    pos = s.in.cpos;
    //Console.println("\ninit! ch = "+ch);
  }

  def reportSyntaxError(str: String) = {
    s.syntaxError(pos-1, "in XML literal: " + str)
    nextch
  }

  def sync: Unit = {
    xEmbeddedBlock = false
    s.xSync
  }

  /** '<' xPattern  ::= Name [S] { xmlPattern | '{' pattern3 '}' } ETag
   *                  | Name [S] '/' '>'
   */
  def xPattern:Tree = {
    //Console.println("xPattern")
    val pos1 = pos
    val qname = xName
    debugLastStartElement.push((pos1, qname))
    xSpaceOpt
    if (ch == '/') { // empty tag
      nextch
      xToken('>')
      return handle.makeXMLpat(pos1, qname, new mutable.ArrayBuffer[Tree]())
    }

     // else: tag with content
    xToken('>')
    var ts = new mutable.ArrayBuffer[Tree]
    var exit = false
    while (! exit) {
      val pos2 = pos
      if (xEmbeddedBlock) {
        ts ++ xScalaPatterns
      } else
        ch match {
          case '<' => { // tag
            nextch
            if (ch != '/') { //child
              ts.append(xPattern)
            } else {
              exit = true
            }
          }
          case '{' => // embedded Scala patterns
            while( ch == '{' ) {
              s.in.next;
              ts ++ xScalaPatterns;
            }
            // postcond: xEmbeddedBlock = false;
          if (xEmbeddedBlock) Predef.error("problem with embedded block"); // assert
          case _ => // teMaxt
            appendText( pos2, ts, xText );
          // here  xEmbeddedBlock might be true;
          //if( xEmbeddedBlock ) throw new ApplicationError("after:"+text); // assert
        }
    }
    xEndTag(qname)
    debugLastStartElement.pop
    handle.makeXMLpat(pos1, qname, ts)
  }

} /* class MarkupParser */
}
