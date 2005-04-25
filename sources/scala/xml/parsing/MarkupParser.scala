/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing;

/** an xml parser. parses XML, invokes callback methods of a MarkupHandler
 *  and returns whatever the markup handler returns. Use ConstructingParser
 *  if you just want to parse XML to construct instances of scala.xml.Node.
 */
abstract class MarkupParser with TokenTests {

  /** the handler of the markup */
  val handle: MarkupHandler;

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

  //var xEmbeddedBlock = false;

  /** this method assign the next character to ch and advances in input */
  def nextch: Unit;

  //final val enableEmbeddedExpressions: Boolean = false;

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
  def xCheckEmbeddedBlock:Boolean = {
    xEmbeddedBlock =
      enableEmbeddedExpressions && (ch == '{') && { nextch; ch != '{' };
    return xEmbeddedBlock;
  }
   */

  /** parse attribute and create namespace scope, metadata
   *  [41] Attributes    ::= { S Name Eq AttValue }
   */
  def xAttributes(pscope:NamespaceBinding): Pair[MetaData,NamespaceBinding] = {
    var scope: NamespaceBinding = pscope;
    var aMap: MetaData = Null;
    while( isNameStart( ch )) {
      val pos = this.pos;

      val qname = xName;
      val _     = xEQ;
      val value = xAttributeValue();

      Utility.prefix(qname) match {
        case Some("xmlns") =>
          val prefix = qname.substring(6 /*xmlns:*/ , qname.length());
          scope = new NamespaceBinding(prefix, value, scope);

        case Some(prefix)       =>
          val key = qname.substring(prefix.length()+1, qname.length());
          aMap = new PrefixedAttribute(prefix, key, value, aMap);

        case _             =>
          if( qname == "xmlns" )
            scope = new NamespaceBinding(null, value, scope);
          else
            aMap = new UnprefixedAttribute(qname, value, aMap);
      }

      if ((ch != '/') && (ch != '>'))
        xSpace;
    }

    if(!aMap.wellformed(scope))
        reportSyntaxError( "double attribute");

    Pair(aMap,scope)
  }

  /** attribute value, terminated by either ' or ". value may not contain &lt;.
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   */
  def xAttributeValue(): String = {
    val endch = ch;
    nextch;
    while (ch != endch) {
      if('<' == ch)
        reportSyntaxError( "'<' not allowed in attrib value" );
      putChar(ch);
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength( 0 );

    // @todo: normalize attribute value
    // well-formedness constraint
    str

  }

  /** parse a start or empty tag.
   *  [40] STag         ::= '&lt;' Name { S Attribute } [S]
   *  [44] EmptyElemTag ::= '&lt;' Name { S Attribute } [S]
   */
  protected def xTag(pscope:NamespaceBinding): Tuple3[String, MetaData, NamespaceBinding] = {
    val qname = xName;

    xSpaceOpt;
    val Pair(aMap: MetaData, scope: NamespaceBinding) = {
      if(isNameStart( ch ))
        xAttributes(pscope)
      else
        Pair(Null, pscope)
    }
    Triple(qname, aMap, scope);
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
  def xCharData: NodeSeq = {
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
        return handle.text( pos1, sb.toString() );
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
    new String(Predef.Array(i.asInstanceOf[char]))
  }


  /** Comment ::= '&lt;!--' ((Char - '-') | ('-' (Char - '-')))* '--&gt;'
   *
   * see [15]
   */
  def xComment: NodeSeq = {
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

  /* todo: move this into the NodeBuffer class */
  def appendText(pos: Int, ts: NodeBuffer, txt: String): Unit = {
    if (preserveWS)
      ts + handle.text(pos, txt);
    else
      for (val t <- TextBuffer.fromString(txt).toText) {
        ts + handle.text(pos, t.text);
      }
  }

  def content(pscope: NamespaceBinding): NodeSeq = {
    var ts = new NodeBuffer;
    var exit = false;
    while( !exit ) {
      /*      if( xEmbeddedBlock ) {
       ts.append( xEmbeddedExpr );
       } else {*/
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
                  ts + xCharData;
                else                            // comment
                  ts + xComment;
              case '?' =>                       // PI
                nextch;
                ts + xProcInstr;
              case _   =>
                ts + element1(pscope);     // child
            }

          //case '{' =>
/*            if( xCheckEmbeddedBlock ) {
              ts.appendAll(xEmbeddedExpr);
            } else {*/
          //    val str = new StringBuffer("{");
          //    str.append(xText);
          //    appendText(tmppos, ts, str.toString());
            /*}*/
          // postcond: xEmbeddedBlock == false!
          case '&' => // EntityRef or CharRef
            nextch;
            ch match {
              case '#' => // CharacterRef
                nextch;
              val theChar = handle.text( tmppos, xCharRef );
              xToken(';');
              ts + theChar ;
              case _ => // EntityRef
                val n = xName ;
                xToken(';');
                ts + handle.entityRef( tmppos, n ) ;
            }
          case _ => // text content
            appendText(tmppos, ts, xText);
          // here xEmbeddedBlock might be true
        }
    /*}*/
  }
    // 2do: optimize seq repr.
    new NodeSeq {
      val theSeq = ts.toList;
    }
  } /* end content */

  def element(pscope: NamespaceBinding): NodeSeq = {
    xToken('<');
    element1(pscope);
  }

  /** '&lt;' element ::= xmlTag1 '&gt;'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '&gt;'
   */
  def element1(pscope: NamespaceBinding): NodeSeq = {
    val pos = this.pos;
    val Tuple3(qname, aMap, scope) = xTag(pscope);
    val ts = {
      if(ch == '/') {    // empty element
        xToken('/');
        xToken('>');
        NodeSeq.Empty;
      } else {          // element with  content
        xToken('>');
        val tmp = content(scope);
        xEndTag( qname );
        tmp;
      }
    }
    Utility.prefix(qname) match {
      case Some(pre) =>
        val local = qname.substring(pre.length()+1, qname.length());
        handle.element(pos, pre, local, aMap, scope, ts );
      case _ =>
        handle.element(pos, null, qname, aMap, scope, ts );
    }
  }

  //def xEmbeddedExpr: MarkupType;

  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def xName: String = {
    if (isNameStart(ch)) {
      do {
        putChar(ch);
        nextch;
      } while (isNameChar(ch));
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
  def xSpaceOpt = while (isSpace(ch)) { nextch; };

  /** scan [3] S ::= (#x20 | #x9 | #xD | #xA)+ */
  def xSpace = {
    if (isSpace(ch)) {
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
  def xProcInstr: NodeSeq = {
    val sb:StringBuffer = new StringBuffer();
    val n = xName;
    if( isSpace( ch ) ) {
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
    //if( xEmbeddedBlock ) throw FatalError("internal error: encountered embedded block"); // assert

    /*if( xCheckEmbeddedBlock )
      return ""
    else {*/
      var exit = false;
      while( !exit ) {
        putChar( ch );
        nextch;
        exit = /*{ nextch; xCheckEmbeddedBlock }||*/( ch == '<' ) || ( ch == '&' );
      }
      val str = cbuf.toString();
      cbuf.setLength(0);
      str
    /*}*/
  }

}
