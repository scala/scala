/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing;

import scala.xml.dtd._ ;
/** an xml parser. parses XML 1.0, invokes callback methods of a MarkupHandler
 *  and returns whatever the markup handler returns. Use ConstructingParser
 *  if you just want to parse XML to construct instances of scala.xml.Node.
 */
abstract class MarkupParser with TokenTests {

  //
  // variables, values
  //

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

  var dtd: DTD = null;

  var decls: List[scala.xml.dtd.Decl] = Nil;

  var eof: Boolean = false;

  //
  // methods
  //

  /** &lt;? prolog ::= xml S
   */
  def prolog(): Tuple3[Option[String], Option[String], Option[Boolean]] = {

    var info_ver: Option[String] = None;
    var info_enc: Option[String] = None;
    var info_stdl: Option[Boolean] = None;

    xToken("xml");
    xSpace;
    val Pair(md,scp) = xAttributes(TopScope);
    xToken('?');
    xToken('>');
    xSpace;
    if(TopScope == scp) {
      var m = md;

      if(!m.isPrefixed && m.key == "version") {
        if(m.value == "1.0") {
          info_ver = Some("1.0");
          m = m.next;
        } else {
          reportSyntaxError("cannot deal with versions != 1.0");
        }
      } else
        reportSyntaxError("VersionInfo expected!");

      if(!m.isPrefixed && m.key == "encoding") {
        val enc = m.value;
        if(!isValidIANAEncoding(enc))
          reportSyntaxError("\""+enc+"\" is not a valid encoding");
        info_enc = Some(enc);
        m = m.next
      }

      if(!m.isPrefixed && m.key == "standalone") {
        m.value.match {
          case "yes" =>
            info_stdl = Some(true);
          case "no" =>
            info_stdl = Some(false);
          case _ =>
            reportSyntaxError("either 'yes' or 'no' expected");
        }
        m = m.next
      }

      if(m != Null)
        reportSyntaxError("VersionInfo EncodingDecl? SDDecl? or '?>' expected!");
    } else
        reportSyntaxError("no xmlns definitions here, please");

    Tuple3(info_ver,info_enc,info_stdl)
  }

  /**
   *[22]        prolog     ::=          XMLDecl? Misc* (doctypedecl Misc*)?
   *[23]        XMLDecl    ::=          '&lt;?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
   *[24]        VersionInfo        ::=          S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
   *[25]        Eq         ::=          S? '=' S?
   *[26]        VersionNum         ::=          '1.0'
   *[27]        Misc       ::=          Comment | PI | S
   */

  def document(): Document = {
    this.dtd = null;
    var info_prolog: Tuple3[Option[String], Option[String], Option[Boolean]] =
      Tuple3(None,None,None);
    if('<' != ch) {
      reportSyntaxError("< expected");
      return null;
    }

    nextch; // is prolog ?
    if('?' == ch) {
      nextch;
      info_prolog = prolog();
    }
    val children = content(TopScope); // DTD handled as side effect
    var elemCount = 0;
    var theNode: Node = _;
    for(val c <- children) c.match {
      case _:ProcInstr => ;
      case _:Comment => ;
      case _:EntityRef => // todo: fix entities, shouldn't be "special"
        reportSyntaxError("no entity references alllowed here");
      case s:SpecialNode =>
        if(s.toString().trim().length() > 0) //non-empty text nodes not allowed
          elemCount = elemCount + 2;
      case m:Node =>
        elemCount = elemCount + 1;
      theNode = m;
    }
    if(1 != elemCount) {
      reportSyntaxError("document must contain exactly one element");
      Console.println(children.toList);
    }
    val doc = new Document();
    doc.children    = children;
    doc.docElem    = theNode;
    doc.version    = info_prolog._1;
    doc.encoding   = info_prolog._2;
    doc.standAlone = info_prolog._3;
    doc.dtd        = this.dtd;
    return doc
  }

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

  def xToken(that: Seq[Char]): Unit = {
    val it = that.elements;
    while(it.hasNext)
      xToken(it.next);
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

      if ((ch != '/') && (ch != '>') && ('?' != ch))
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
    xToken("[CDATA[");
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
    var exit = eof;
    while( !exit ) {
      //Console.println("in content, ch = '"+ch+"' line="+scala.io.Position.line(pos));
      /*      if( xEmbeddedBlock ) {
       ts.append( xEmbeddedExpr );
       } else {*/
        tmppos = pos;
        exit = eof;
        if(!eof)
          ch match {
          case '<' => // another tag
            //Console.println("before ch = '"+ch+"' line="+scala.io.Position.line(pos)+" pos="+pos);
            nextch;
            //Console.println("after ch = '"+ch+"' line="+scala.io.Position.line(pos)+" pos="+pos);

            ch match {
              case '/' =>
                exit = true;                    // end tag
              case '!' =>
                nextch;
              if ('[' == ch)                 // CDATA
                ts + xCharData;
              else if ('D' == ch) // doctypedecl, parse DTD
                parseDTD();
              else // comment
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
            //Console.println("text content?? pos = "+pos);
            appendText(tmppos, ts, xText);
          // here xEmbeddedBlock might be true
          }
    /*}*/
    }
    // 2do: optimize seq repr.
    new NodeSeq {
      val theSeq = ts.toList;
    }
  } // content(NamespaceBinding)

  /** externalID ::= SYSTEM S syslit
   *                 PUBLIC S pubid S syslit
   */

  def externalID(): ExternalID = ch.match {
    case 'S' =>
      nextch;
      xToken("YSTEM");
      val sysID = systemLiteral();
      new SystemID(sysID);
    case 'P' =>
      nextch; xToken("UBLIC");
      val pubID = pubidLiteral();
      xSpace;
      val sysID = systemLiteral();
      new PublicID(pubID, sysID);
  }
  /** parses document type declaration and assigns it to instance variable
   *  dtd.
   *
   *  <! parseDTD ::= DOCTYPE name ... >
   */
  def parseDTD(): Unit = { // dirty but fast
    var extID: ExternalID = null;
    if(this.dtd != null)
      reportSyntaxError("unexpected character");
    xToken("DOCTYPE");
    xSpace;
    val n = xName;
    xSpace;
    //external ID
    if('S' == ch || 'P' == ch) {
      extID = externalID();
      xSpace;
    }
    if('[' == ch) { // internal subset
      nextch;
      /* TODO */
      while(']' != ch)
        nextch;
      // TODO: do the DTD parsing?? ?!?!?!?!!
      xToken(']');
      xSpaceOpt;
    }
    xToken('>');
    this.dtd = new DTD {
      override var externalID = extID;
    }
  }

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
    //Console.println("in xText! ch = '"+ch+"'");
      var exit = false;
      while( !exit ) {
        //Console.println("LOOP in xText! ch = '"+ch+"' + pos="+pos);
        putChar( ch );
        val opos = pos;
        nextch;

        //Console.println("STILL LOOP in xText! ch = '"+ch+"' + pos="+pos+" opos="+opos);


        exit = eof || /*{ nextch; xCheckEmbeddedBlock }||*/( ch == '<' ) || ( ch == '&' );
      }
      val str = cbuf.toString();
      cbuf.setLength(0);
      str
    /*}*/
  }

  /** attribute value, terminated by either ' or ". value may not contain &lt;.
   *       AttValue     ::= `'` { _ } `'`
   *                      | `"` { _ } `"`
   */
  def systemLiteral(): String = {
    val endch = ch;
    if(ch!='\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected");
    nextch;
    while (ch != endch) {
      putChar(ch);
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength( 0 );
    str
  }


  /* [12]       PubidLiteral ::=        '"' PubidChar* '"' | "'" (PubidChar - "'")* "'" */
  def pubidLiteral(): String = {
    val endch = ch;
    if(ch!='\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected");
    nextch;
    while (ch != endch) {
      putChar(ch);
      if(!isPubIDChar(ch))
        reportSyntaxError("char '"+ch+"' is not allowed in public id");
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength( 0 );
    str
  }

  //
  //  dtd parsing
  //

  def intSubset(): Unit = {
    xSpace;
    while(']' != ch)
      ch match {
        case '%' =>
          nextch;
          decls = PEReference(xName) :: decls;
          xToken(';')
          //peReference
      case '<' =>
        nextch;

        if('?' == ch)
          xProcInstr; // simply ignore processing instructions!
        else {
          xToken('!');
          ch.match {
            case '-' =>
              xComment ; // ignore comments

            case 'E' =>
              nextch;
              if('L' == ch) {
                nextch;
                elementDecl()
              } else
                entityDecl();

            case 'A' =>
              nextch;
              attrDecl();

            case 'N' =>
              nextch;
              notationDecl();
          }
        }
        case _ =>
          reportSyntaxError("unexpected character");
      }
  }

  /** <! element := ELEMENT
   */
  def elementDecl(): Unit = {
    xToken("EMENT");
    xSpace;
    val n = xName;
    xSpace;
    while('>' != ch) {
      putChar(ch);
      nextch;
    }
    nextch;
    val cmstr = cbuf.toString();
    cbuf.setLength( 0 );
    val cm = ContentModel.parse(cmstr);
    decls = ElemDecl(n, cm, null)::decls;
  }

  /** <! element := ELEMENT
   */
  def attrDecl() = {
    xToken("TTLIST");
    xSpace;
    val n = xName;
    var attList: List[AttrDecl] = Nil;
    // later: find the elemDecl for n
    while('>' != ch) {
      Console.println("");
      val aname = xName;
      var defdecl: DefaultDecl = null;
      xSpace;
      while('"' != ch && '\'' != ch && '#' != ch && '<' != ch) {
        if(!isSpace(ch))
          cbuf.append(ch);
        nextch;
      }
      ch match {
        case '\'' | '"' =>
          val defValue = xAttributeValue(); // default value
          defdecl = DEFAULT(false, defValue);

        case '#' => xName.match {
            case "FIXED" =>
              xSpace;
              val defValue = xAttributeValue(); // default value
              defdecl = DEFAULT(true, defValue);
            case "IMPLIED" =>
              defdecl = IMPLIED
            case "REQUIRED" =>
              defdecl = REQUIRED
          }
        case _ =>
      }
      xSpaceOpt;

      attList = AttrDecl(xName, cbuf.toString(), defdecl) :: attList;
      cbuf.setLength(0);
    }
    nextch;
    decls = AttListDecl(n, attList.reverse) :: decls
  }

  /** <! element := ELEMENT
   */
  def entityDecl() = {
    var isParameterEntity = false;
    var entdef: EntityDef = null;
    xToken("NTITY");
    xSpace;
    if('%' == ch) {
      isParameterEntity = true;
      xSpace;
    }
    val n = xName;
    xSpace;

    val res = ch match {
      case 'S' | 'P' => //sy
        val extID = externalID();
        if(isParameterEntity) {


          ParameterEntityDecl(n, ExtDef(extID))

        } else { // notation?

          xSpace;
          if('>' != ch) {
            xToken("NDATA");
            xSpace;
            val notat = xName;
            xSpace;
            UnparsedEntityDecl(n, extID, notat);
          } else

            ParsedEntityDecl(n, ExtDef(extID));

        }

      case '"' | '\'' =>
        val av = xAttributeValue();
        if(isParameterEntity)
          ParameterEntityDecl(n, IntDef(av))
        else
          ParsedEntityDecl(n, IntDef(av));
    }
    decls = res :: decls;
  } // entityDecl

  /** 'N' notationDecl ::= "OTATION"
   */
  def notationDecl() = {
    xToken("OTATION");
    xSpace;
    val notat = xName;
    xSpace;
    val extID = externalID();
    xSpace;
    xToken('>');
    decls = NotationDecl(notat, extID) :: decls;
  }

}
