/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing;

import scala.collection.mutable;
import scala.collection.immutable.ListMap;

abstract class MarkupParser[MarkupType, AVType] {


  /** the handler of the markup */
  val handle: MarkupHandler[MarkupType, AVType];

  /** if true, does not remove surplus whitespace */
  val preserveWS: Boolean;

  /** holds the position in the source file */
  var pos: Int = _;

  /** holds temporary values of pos */
  var tmppos: Int = _;

  /** holds the next character */
  var ch: Char = _;

  /** character buffer, for names */
  protected val cbuf = new StringBuffer();

  /** append Unicode character to name buffer*/
  protected def putChar(c: Char) = cbuf.append(c);


  var xEmbeddedBlock = false;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit;

  /** this method should assign the first character of the input to ch */
  def init: Unit;

  val enableEmbeddedExpressions: Boolean;

  /** report a syntax error */
  def reportSyntaxError(str: String): Unit;

  /** munch expected XML token, report syntax error for unexpected
  */
  def xToken(that: Char): Unit = {
    if (ch == that)
      nextch;
    else
      reportSyntaxError("'" + that + "' expected instead of '" + ch + "'");
  }

  /** checks whether next character starts a Scala block, if yes, skip it.
   * @return true if next character starts a scala block
   */
  def xCheckEmbeddedBlock:Boolean = {
    xEmbeddedBlock =
      enableEmbeddedExpressions && (ch == '{') && { nextch; ch != '{' };
    return xEmbeddedBlock;
  }

  /** parse attribute and add it to listmap
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *                      | `{` scalablock `}`
  */
  def xAttributes = {
    var aMap = new mutable.HashMap[String, AttribValue[AVType]];
    while (xml.Parsing.isNameStart(ch)) {
      val key = xName;
      xEQ;
      val delim = ch;
      val pos1 = pos;
      val value: AttribValue[AVType] = ch match {
        case '"' | '\'' =>
          nextch;
          val tmp = xAttributeValue(delim);
          nextch;
          handle.attributeCDataValue(pos1, tmp);
        case '{' if enableEmbeddedExpressions =>
          nextch;
          handle.attributeEmbedded(pos1, xEmbeddedExpr);
        case _ =>
          reportSyntaxError( "' or \" delimited attribute value or '{' scala-expr '}' expected" );
          handle.attributeCDataValue( pos1, "<syntax-error>" )
      };
      // well-formedness constraint: unique attribute names
      if (aMap.contains(key))
        reportSyntaxError( "attribute " + key + " may only be defined once" );
      aMap.update( key, value );
      if ((ch != '/') && (ch != '>'))
        xSpace;
    };
   aMap
  }

  /** attribute value, terminated by either ' or ". value may not contain &lt;.
   *  @param endch either ' or "
   */
  def xAttributeValue(endch: Char): String = {
    while (ch != endch) {
      putChar(ch);
      nextch
    }
    val str = cbuf.toString();
    cbuf.setLength( 0 );
    // @todo: normalize attribute value
    // well-formedness constraint
    if (str.indexOf('<') != -1) {
      reportSyntaxError( "'<' not allowed in attrib value" ); ""
    }
    else
      str
  }

  /** parse a start or empty tag.
   *  [40] STag         ::= '&lt;' Name { S Attribute } [S]
   *  [44] EmptyElemTag ::= '&lt;' Name { S Attribute } [S]
   */
  def xTag: Pair[String, mutable.Map[String, AttribValue[AVType]]] = {
    val elemName = xName;
    xSpaceOpt;
    val aMap = if (xml.Parsing.isNameStart(ch)) {
      xAttributes;
    } else {
      new mutable.HashMap[String, AttribValue[AVType]]();
    }
    Tuple2(elemName, aMap)
  }

  /** [42]  '&lt;' xmlEndTag ::=  '&lt;' '/' Name S? '&gt;'
   */
  def xEndTag(n: String) = {
    xToken('/');
    val m = xName;
    if(n != m) reportSyntaxError("expected closing tag of " + n/* +", not "+m*/);
    xSpaceOpt;
    xToken('>')
  }

  /** '&lt;! CharData ::= [CDATA[ ( {char} - {char}"]]&gt;"{char} ) ']]&gt;'
   *
   * see [15]
   */
  def xCharData: MarkupType = {
    xToken('[');
    xToken('C');
    xToken('D');
    xToken('A');
    xToken('T');
    xToken('A');
    xToken('[');
    val pos1 = pos;
    val sb:StringBuffer = new StringBuffer();
    while (true) {
      if( ch==']'  &&
         { sb.append(ch); nextch; ch == ']' } &&
         { sb.append(ch); nextch; ch == '>' } ) {
        sb.setLength(sb.length() - 2);
        nextch;
        return handle.charData( pos1, sb.toString() );
      } else sb.append( ch );
      nextch;
    }
    throw FatalError("this cannot happen");
  };

  /** CharRef ::= "&amp;#" '0'..'9' {'0'..'9'} ";"
   *            | "&amp;#x" '0'..'9'|'A'..'F'|'a'..'f' { hexdigit } ";"
   *
   * see [66]
   */
  def xCharRef: String = {
    val hex  = (ch == 'x') && { nextch; true };
    val base = if (hex) 16 else 10;
    var i = 0;
    while( ch != ';' ) {
      ch match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + Character.digit( ch, base );
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if( !hex )
            reportSyntaxError("hex char not allowed in decimal char ref\n"
                         +"Did you mean to write &#x ?");
          else
            i = i * base + Character.digit(ch, base);
        case _ =>
          reportSyntaxError("character '" + ch + " not allowed in char ref\n");
      }
      nextch;
    }
    new String(Predef.Array[char](i.asInstanceOf[char]))
  }


  /** Comment ::= '&lt;!--' ((Char - '-') | ('-' (Char - '-')))* '--&gt;'
   *
   * see [15]
   */
  def xComment: MarkupType = {
    val sb: StringBuffer = new StringBuffer();
    xToken('-');
    xToken('-');
    while (true) {
      if (ch == '-'  && { sb.append(ch); nextch; ch == '-' }) {
        sb.setLength(sb.length() - 1);
        nextch;
        xToken('>');
        return handle.comment(pos, sb.toString());
      } else sb.append(ch);
      nextch;
    }
    throw FatalError("this cannot happen");
  };

  def appendText(pos: Int, ts: mutable.Buffer[MarkupType], txt: String): Unit = {
    if (!preserveWS)
      for (val t <- TextBuffer.fromString(txt).toText) {
        ts.append(handle.text(pos, t.text));
      }
    else
      ts.append(handle.text(pos, txt));
  }

  def content: mutable.Buffer[MarkupType] = {
    var ts = new mutable.ArrayBuffer[MarkupType];
    var exit = false;
    while (!exit) {
      if (xEmbeddedBlock) {
        ts.append(xEmbeddedExpr);
      } else {
        tmppos = pos;
        ch match {
          case '<' => // another tag
            nextch;
            ch match {
              case '/' =>
                exit = true;                    // end tag
              case '!' =>
                nextch;
                if ('[' == ch)                 // CDATA
                  ts.append(xCharData);
                else                            // comment
                  ts.append(xComment);
              case '?' =>                       // PI
                nextch;
                ts.append(xProcInstr);
              case _   =>
                ts.append(element);     // child
            }

          case '{' =>
            if (xCheckEmbeddedBlock) {
              ts.append(xEmbeddedExpr);
            }
            else {
              val str = new StringBuffer("{");
              str.append(xText);
              appendText(tmppos, ts, str.toString());
            }
          // postcond: xEmbeddedBlock == false!
          case '&' => // EntityRef or CharRef
            nextch;
            ch match {
              case '#' => // CharacterRef
                nextch;
              val theChar = handle.text( tmppos, xCharRef );
              xToken(';');
              ts.append( theChar );
              case _ => // EntityRef
                val n = xName ;
                xToken(';');
                ts.append( handle.entityRef( tmppos, n ) );
            }
          case _ => // text content
            appendText(tmppos, ts, xText);
          // here xEmbeddedBlock might be true
        }
      }
    }
    ts
  } /* end content */

  /** '&lt;' element ::= xmlTag1 '&gt;'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '&gt;'
   */
  def element: MarkupType = {
    var pos1 = pos;
    val Tuple2(qname, attrMap) = xTag;
    if (ch == '/') { // empty element
      xToken('/');
      xToken('>');
      handle.element(pos1, qname, attrMap, new mutable.ListBuffer[MarkupType]);
    }
    else { // handle content
      xToken('>');
      val ts = content;
      xEndTag(qname);
      handle.element(pos1, qname, attrMap, ts)
    }
  }

  def xEmbeddedExpr: MarkupType;

  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def xName: String = {
    if (xml.Parsing.isNameStart(ch)) {
      do {
        putChar(ch);
        nextch;
      } while (xml.Parsing.isNameChar(ch));
      val n = cbuf.toString().intern();
      cbuf.setLength(0);
      n
    }
    else {
      reportSyntaxError("name expected");
      new String()
    }
  }


  /** scan [S] '=' [S]*/
  def xEQ = { xSpaceOpt; xToken('='); xSpaceOpt }

  /** skip optional space S? */
  def xSpaceOpt = while (xml.Parsing.isSpace(ch)) { nextch; };

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  def xSpace = {
    if (xml.Parsing.isSpace(ch)) {
      nextch; xSpaceOpt
    }
    else {
      reportSyntaxError("whitespace expected");
    }
  }

  /** '&lt;?' ProcInstr ::= Name [S ({Char} - ({Char}'&gt;?' {Char})]'?&gt;'
   *
   * see [15]
   */
  def xProcInstr: MarkupType = {
    val sb:StringBuffer = new StringBuffer();
    val n = xName;
    if( xml.Parsing.isSpace( ch ) ) {
      xSpace;
      while (true) {
        if (ch=='?' && { sb.append( ch ); nextch; ch == '>' }) {
          sb.setLength(sb.length() - 1);
          nextch;
          return handle.procInstr(tmppos, n.toString(), sb.toString());
        } else
          sb.append(ch);
        nextch
      }
    };
    xToken('?');
    xToken('>');
    return handle.procInstr(tmppos, n.toString(), sb.toString());
  }

  /** parse character data.
   *   precondition: xEmbeddedBlock == false (we are not in a scala block)
   */
  def xText: String = {
    if (xEmbeddedBlock)
      throw FatalError("internal error: encountered embedded block"); // assert

    if (xCheckEmbeddedBlock)
      return ""
    else {
      var exit = false;
      while (!exit) {
        putChar(ch);
        exit = { nextch; xCheckEmbeddedBlock } || (ch == '<') || (ch == '&');
      }
      val str = cbuf.toString();
      cbuf.setLength(0);
      str
    }
  }

}
