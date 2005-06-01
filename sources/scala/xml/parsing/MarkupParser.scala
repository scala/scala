/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.xml.parsing;

import scala.io.Source;
import scala.xml.dtd._ ;

/**
 * An XML parser.
 *
 * Parses XML 1.0, invokes callback methods of a MarkupHandler
 * and returns whatever the markup handler returns. Use
 * <code>ConstructingParser</code> if you just want to parse XML to
 * construct instances of <code>scala.xml.Node</code>.
 *
 * While XML elements are returned, DTD declarations - if handled - are
 * collected using side-effects.
 */
abstract class MarkupParser: (MarkupParser with MarkupHandler) extends AnyRef with TokenTests {

  val input: Source;

  def externalSource(systemLiteral: String): Source;

  //
  // variables, values
  //

  var curInput: Source = input;

  /** the handler of the markup, should return this */
  val handle: MarkupHandler;

  /** stack of inputs */
  var inpStack: List[Source] = Nil;

  /** holds the position in the source file */
  var pos: Int = _;


  /* used when reading external subset */
  var extIndex = -1;

  /** holds temporary values of pos */
  var tmppos: Int = _;

  /** holds the next character */
  var ch: Char = _;

  /** character buffer, for names */
  protected val cbuf = new StringBuffer();

  var dtd: DTD = null;

  var eof: Boolean = false;

  //
  // methods
  //

  /** &lt;? prolog ::= xml S ... ?&gt;
   */
  def xmlProcInstr(): MetaData = {
    xToken("xml");
    xSpace;
    val Pair(md,scp) = xAttributes(TopScope);
    if(scp != TopScope)
      reportSyntaxError("no xmlns definitions here, please.");
    xToken('?');
    xToken('>');
    md
  }

  /** &lt;? prolog ::= xml S
   */
  def prolog(): Tuple3[Option[String], Option[String], Option[Boolean]] = {

    //Console.println("(DEBUG) prolog");

    var info_ver: Option[String] = None;
    var info_enc: Option[String] = None;
    var info_stdl: Option[Boolean] = None;

    var m = xmlProcInstr();

    xSpace;

    if (!m.isPrefixed && m.key == "version") {
      if (m.value == "1.0") {
        info_ver = Some("1.0");
        m = m.next;
      } else {
        reportSyntaxError("cannot deal with versions != 1.0");
      }
    } else
      reportSyntaxError("VersionInfo expected!");

    if (!m.isPrefixed && m.key == "encoding") {
      val enc = m.value;
      if (!isValidIANAEncoding(enc))
        reportSyntaxError("\"" + enc + "\" is not a valid encoding");
      info_enc = Some(enc);
      m = m.next
    }

    if (!m.isPrefixed && m.key == "standalone") {
      m.value match {
        case "yes" =>
          info_stdl = Some(true);
        case "no" =>
          info_stdl = Some(false);
        case _ =>
          reportSyntaxError("either 'yes' or 'no' expected");
      }
      m = m.next
    }

    if (m != Null)
      reportSyntaxError("VersionInfo EncodingDecl? SDDecl? or '?>' expected!");
    Tuple3(info_ver,info_enc,info_stdl)
  }

  /** prolog, but without standalone */
  def textDecl(): Tuple2[Option[String],Option[String]] = {

    var info_ver: Option[String] = None;
    var info_enc: Option[String] = None;

    var m = xmlProcInstr();

    if (!m.isPrefixed && m.key == "version") {
      if (m.value == "1.0") {
        info_ver = Some("1.0");
        m = m.next;
      } else {
        reportSyntaxError("cannot deal with versions != 1.0");
      }
    } else
      reportSyntaxError("VersionInfo expected!");

    if (m != Null && !m.isPrefixed && m.key == "encoding") {
      val enc = m.value;
      if (!isValidIANAEncoding(enc))
        reportSyntaxError("\"" + enc + "\" is not a valid encoding");
      info_enc = Some(enc);
      m = m.next
    }

    if (m != Null)
      reportSyntaxError("VersionInfo EncodingDecl? SDDecl? or '?>' expected!");

    Tuple2(info_ver, info_enc);
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

    //Console.println("(DEBUG) document");

    this.dtd = null;
    var info_prolog: Tuple3[Option[String], Option[String], Option[Boolean]] =
      Tuple3(None, None, None);
    if ('<' != ch) {
      reportSyntaxError("< expected");
      return null;
    }

    nextch; // is prolog ?
    if ('?' == ch) {
      nextch;
      info_prolog = prolog();
    }
    val children = content(TopScope); // DTD handled as side effect
    var elemCount = 0;
    var theNode: Node = _;
    for (val c <- children) c match {
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
    if (1 != elemCount) {
      reportSyntaxError("document must contain exactly one element");
      Console.println(children.toList);
    }

    val doc = new Document();
    doc.children   = children;
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
  def nextch: Unit = {
    if (curInput.hasNext) {
      ch = curInput.next;
      pos = curInput.pos;
    } else {
      val ilen = inpStack.length;
      //Console.println("  ilen = "+ilen+ " extIndex = "+extIndex);
      if ((ilen != extIndex) && (ilen > 0)) {
        /** for external source, inpStack == Nil ! need notify of eof! */
        pop();
      } else {
        eof = true;
        ch = 0.asInstanceOf[Char];
      //throw new Exception("this is the end")
      }
    }
  }

  //final val enableEmbeddedExpressions: Boolean = false;

  /** munch expected XML token, report syntax error for unexpected
  */
  def xToken(that: Char): Unit = {
    if (ch == that)
      nextch;
    else  {
      reportSyntaxError("'" + that + "' expected instead of '" + ch + "'");
      error("FATAL");
    }
  }

  def xToken(that: Seq[Char]): Unit = {
    val it = that.elements;
    while (it.hasNext)
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
    while (isNameStart(ch)) {
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
      if ('<' == ch)
        reportSyntaxError( "'<' not allowed in attrib value" );
      putChar(ch);
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength(0);

    // @todo: normalize attribute value
    // well-formedness constraint
    str

  }

  /** entity value, terminated by either ' or ". value may not contain &lt;.
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   */
  def xEntityValue(): String = {
    val endch = ch;
    nextch;
    while (ch != endch) {
      putChar(ch);
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength(0);
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
      if (isNameStart(ch))
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
    if (n != m)
      reportSyntaxError("expected closing tag of " + n/* +", not "+m*/);
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
    val sb: StringBuffer = new StringBuffer();
    while (true) {
      if (ch==']'  &&
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
    while (ch != ';') {
      ch match {
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          i = i * base + Character.digit( ch, base );
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
           | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          if (! hex)
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

  /** '<' content1 ::=  ... */
  def content1(pscope: NamespaceBinding, ts: NodeBuffer): Unit = {
    ch match {
      case '!' =>
        nextch;
      if ('[' == ch)                 // CDATA
        ts + xCharData;
      else if ('D' == ch) // doctypedecl, parse DTD // @todo REMOVE HACK
        parseDTD();
      else // comment
        ts + xComment;
      case '?' =>                       // PI
        nextch;
        ts + xProcInstr;
      case _   =>
        ts + element1(pscope);     // child
    }
  }

  /** content1 ::=  '&lt;' content1 | '&amp;' charref ... */
  def content(pscope: NamespaceBinding): NodeSeq = {
    var ts = new NodeBuffer;
    var exit = eof;
    while (! exit) {
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

            if('/' ==ch)
              exit = true;                    // end tag
            else
              content1(pscope, ts)
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
                /*
                ts + handle.entityRef( tmppos, n ) ;
                */
                push( n );
            }
          case _ => // text content
            //Console.println("text content?? pos = "+pos);
            appendText(tmppos, ts, xText);
          // here xEmbeddedBlock might be true
          }
    /*}*/
    }
    val list = ts.toList;
    // 2do: optimize seq repr.
    new NodeSeq {
      val theSeq = list;
    }
  } // content(NamespaceBinding)

  /** externalID ::= SYSTEM S syslit
   *                 PUBLIC S pubid S syslit
   */

  def externalID(): ExternalID = ch match {
    case 'S' =>
      nextch;
      xToken("YSTEM");
      xSpace;
      val sysID = systemLiteral();
      new SystemID(sysID);
    case 'P' =>
      nextch; xToken("UBLIC");
      xSpace;
      val pubID = pubidLiteral();
      xSpace;
      val sysID = systemLiteral();
      new PublicID(pubID, sysID);
  }


  /** parses document type declaration and assigns it to instance variable
   *  dtd.
   *
   *  &lt;! parseDTD ::= DOCTYPE name ... >
   */
  def parseDTD(): Unit = { // dirty but fast
    //Console.println("(DEBUG) parseDTD");
    var extID: ExternalID = null;
    if (this.dtd != null)
      reportSyntaxError("unexpected character (DOCTYPE already defined");
    xToken("DOCTYPE");
    xSpace;
    val n = xName;
    xSpace;
    //external ID
    if('S' == ch || 'P' == ch) {
      extID = externalID();
      xSpace;
    }

    /* parse external subset of DTD
     */

    if(null != extID) {

      pushExternal(extID.systemId);
      //val extSubsetSrc = externalSource( extID.systemId );

      extIndex = inpStack.length;
      /*
       .indexOf(':') != -1) { // assume URI
         Source.fromFile(new java.net.URI(extID.systemLiteral));
       } else {
         Source.fromFile(extID.systemLiteral);
       }
      */
      //Console.println("I'll print it now");
      //val old = curInput;
      //tmppos = curInput.pos;
      //val oldch = ch;
      //curInput = extSubsetSrc;
      //pos = 0;
      //nextch;

      extSubset();

      pop();

      extIndex = -1;

      //curInput = old;
      //pos = curInput.pos;
      //ch = curInput.ch;
      //eof = false;
      //while(extSubsetSrc.hasNext)
      //Console.print(extSubsetSrc.next);

      //Console.println("returned from external, current ch = "+ch )
    }

    if ('[' == ch) { // internal subset
      nextch;
      /* TODO */
      //Console.println("hello");
      intSubset();
      //while(']' != ch)
      //  nextch;
      // TODO: do the DTD parsing?? ?!?!?!?!!
      xToken(']');
      xSpaceOpt;
    }
    xToken('>');
    this.dtd = new DTD {
      override var externalID = extID;
      override val decls      = handle.decls.reverse;
    }
    //this.dtd.initializeEntities();
    handle.endDTD(n);
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
    val Tuple2(pre, local) = Utility.prefix(qname).match {
      case Some(p) => Pair(p,qname.substring(p.length()+1, qname.length()));
      case _       => Pair(null,qname);
    }
    val ts = {
      if (ch == '/') {  // empty element
        xToken('/');
        xToken('>');
        handle.elemStart(pos, pre, local, aMap, scope);
        NodeSeq.Empty;
      }
      else {           // element with content
        xToken('>');
        handle.elemStart(pos, pre, local, aMap, scope);
        val tmp = content(scope);
        xEndTag(qname);
        tmp;
      }
    }
    val res = handle.elem(pos, pre, local, aMap, scope, ts );
    handle.elemEnd(pos, pre, local);
    res
  }

  //def xEmbeddedExpr: MarkupType;

  /** Name ::= (Letter | '_' | ':') (NameChar)*
   *
   *  see  [5] of XML 1.0 specification
   */
  def xName: String = {
    if (isNameStart(ch)) {
      while (isNameChar(ch)) {
        putChar(ch);
        nextch;
      }
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
  def xSpaceOpt = while (isSpace(ch) && !eof) { nextch; };

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
    if (isSpace(ch)) {
      xSpace;
      while (true) {
        if (ch == '?' && { sb.append( ch ); nextch; ch == '>' }) {
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
      while (! exit) {
        //Console.println("LOOP in xText! ch = '"+ch+"' + pos="+pos);
        putChar(ch);
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
    if (ch != '\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected");
    nextch;
    while (ch != endch) {
      putChar(ch);
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength(0);
    str
  }


  /* [12]       PubidLiteral ::=        '"' PubidChar* '"' | "'" (PubidChar - "'")* "'" */
  def pubidLiteral(): String = {
    val endch = ch;
    if (ch!='\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected");
    nextch;
    while (ch != endch) {
      putChar(ch);
      //Console.println("hello '"+ch+"'"+isPubIDChar(ch));
      if(!isPubIDChar(ch))
        reportSyntaxError("char '"+ch+"' is not allowed in public id");
      nextch;
    }
    nextch;
    val str = cbuf.toString();
    cbuf.setLength(0);
    str
  }

  //
  //  dtd parsing
  //

  def extSubset(): Unit = {
    var textdecl:Tuple2[Option[String],Option[String]] = null;
    if(ch=='<') {
      nextch;
      if(ch=='?') {
        nextch;
        textdecl = textDecl()
      } else
        markupDecl1();
    }
    while(!eof) {
      markupDecl();
    }
  }

  def markupDecl1() = {
    def doInclude() = {
      xToken('['); while(']' != ch) markupDecl(); nextch // ']'
    }
    def doIgnore() = {
      xToken('['); while(']' != ch) nextch; nextch; // ']'
    }
    if('?' == ch) {
      nextch;
      xProcInstr; // simply ignore processing instructions!
    } else {
      xToken('!');
      ch match {
        case '-' =>
          xComment ; // ignore comments

        case 'E' =>
          nextch;
          if ('L' == ch) {
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

        case '[' if inpStack.length >= extIndex =>
          nextch;
          xSpaceOpt;
          ch match {
            case '%' =>
              nextch;
              val ent = xName;
              xToken(';');
              xSpaceOpt;
            /*
              Console.println("hello, pushing!");
            {
              val test =  replacementText(ent);
              while(test.hasNext)
                Console.print(test.next);
            } */
              push(ent);
              xSpaceOpt;
              //Console.println("hello, getting name");
              val stmt = xName;
              //Console.println("hello, got name");
              xSpaceOpt;
            //Console.println("how can we be eof = "+eof);

            // eof = true because not external?!
              //if(!eof)
              //  error("expected only INCLUDE or IGNORE");

              //pop();


              //Console.println("hello, popped");
              stmt.match {
                // parameter entity
                case "INCLUDE" =>
                  doInclude();
                case "IGNORE" =>
                  doIgnore()
              }
            case 'I' =>
              nextch;
              ch.match {
                case 'G' =>
                  nextch;
                  xToken("NORE");
                  xSpaceOpt;
                  doIgnore()
                case 'N' =>
                  nextch;
                  xToken("NCLUDE");
                  doInclude()
              }
          }
        xToken(']');
        xToken('>');

        case _  =>
          curInput.reportError(pos, "unexpected character '"+ch+"', expected some markupdecl");
        while(ch!='>')
          nextch;

      }
    }
  }

  def markupDecl(): Unit = ch match {
    case '%' =>                  // parameter entity reference
      nextch;
      val ent = xName;
      xToken(';');
      if(!isValidating)
        handle.peReference(ent); //  n-v: just create PE-reference
      else
        push(ent);               //    v: parse replacementText

    //peReference
    case '<' =>
      nextch;
      markupDecl1();

    case _ if isSpace(ch) =>
      xSpace;
    case _ =>
      reportSyntaxError("markupdecl: unexpected character '"+ch+"'");
      nextch;
  }

  /**  "rec-xml/#ExtSubset" pe references may not occur within markup
   declarations
   */
  def intSubset(): Unit = {
    //Console.println("(DEBUG) intSubset()");
    xSpace;
    while (']' != ch) {
      markupDecl()
    }
  }

  /** &lt;! element := ELEMENT
   */
  def elementDecl(): Unit = {
    xToken("EMENT");
    xSpace;
    val n = xName;
    xSpace;
    while ('>' != ch) {
      //Console.println("["+ch+"]");
      putChar(ch);
      nextch;
    }
    //Console.println("END["+ch+"]");
    nextch;
    val cmstr = cbuf.toString();
    cbuf.setLength(0);
    handle.elemDecl(n, cmstr);
  }

  /** &lt;! attlist := ATTLIST
   */
  def attrDecl() = {
    xToken("TTLIST");
    xSpace;
    val n = xName;
    xSpace;
    var attList: List[AttrDecl] = Nil;
    // later: find the elemDecl for n
    while ('>' != ch) {
      val aname = xName;
      //Console.println("attribute name: "+aname);
      var defdecl: DefaultDecl = null;
      xSpace;
      // could be enumeration (foo,bar) parse this later :-/
      while ('"' != ch && '\'' != ch && '#' != ch && '<' != ch) {
        if(!isSpace(ch))
          cbuf.append(ch);
        nextch;
      }
      val atpe = cbuf.toString();
      cbuf.setLength(0);
      //Console.println("attr type: "+atpe);
      ch match {
        case '\'' | '"' =>
          val defValue = xAttributeValue(); // default value
          defdecl = DEFAULT(false, defValue);

        case '#' =>
          nextch;
          xName match {
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

      attList = AttrDecl(aname, atpe, defdecl) :: attList;
      cbuf.setLength(0);
    }
    nextch;
    handle.attListDecl(n, attList.reverse);
  }

  /** &lt;! element := ELEMENT
   */
  def entityDecl() = {
    //Console.println("entityDecl()");
    var isParameterEntity = false;
    var entdef: EntityDef = null;
    xToken("NTITY");
    xSpace;
    if ('%' == ch) {
      nextch;
      isParameterEntity = true;
      xSpace;
    }
    val n = xName;
    xSpace;
    ch match {
      case 'S' | 'P' => //sy
        val extID = externalID();
        if(isParameterEntity) {

          xSpaceOpt;
          xToken('>');
          handle.parameterEntityDecl(n, ExtDef(extID))

        } else { // notation?

          xSpace;
          if ('>' != ch) {
            xToken("NDATA");
            xSpace;
            val notat = xName;
            xSpaceOpt;
            xToken('>');
            handle.unparsedEntityDecl(n, extID, notat);
          } else {
            nextch;
            handle.parsedEntityDecl(n, ExtDef(extID));
          }
        }

      case '"' | '\'' =>
        val av = xEntityValue();
        xSpaceOpt;
        xToken('>');
        if (isParameterEntity)
          handle.parameterEntityDecl(n, IntDef(av));
        else
          handle.parsedEntityDecl(n, IntDef(av));
    }

    {}
  } // entityDecl

  /** 'N' notationDecl ::= "OTATION"
   */
  def notationDecl(): Unit = {
    xToken("OTATION");
    xSpace;
    val notat = xName;
    xSpace;
    val extID = if (ch == 'S') {
      externalID();
    }
    else if (ch == 'P') {
      /** PublicID (without system, only used in NOTATION) */
      nextch;
      xToken("UBLIC");
      xSpace;
      val pubID = pubidLiteral();
      xSpaceOpt;
      val sysID = if (ch != '>')
        systemLiteral()
      else
        null;
      new PublicID(pubID, sysID);
    } else
      error("PUBLIC or SYSTEM expected");
    xSpaceOpt;
    xToken('>');
    handle.notationDecl(notat, extID)
  }

  /**
   * report a syntax error
   */
  def reportSyntaxError(pos: int, str: String): Unit = {
    curInput.reportError(pos, str)
  }

  def reportSyntaxError(str: String): Unit = reportSyntaxError(pos, str);

  /**
   * report a syntax error
   */
  def reportValidationError(pos: int, str: String): Unit = {
    curInput.reportError(pos, str)
  }


  def push(entityName:String) = {
    //Console.println("BEFORE PUSHING  "+ch);
    //Console.println("BEFORE PUSHING  "+pos);
    //Console.print("[PUSHING "+entityName+"]");
    if(!eof)
      inpStack = curInput :: inpStack;

    curInput = replacementText(entityName);
    nextch;
  }

  /*
  def push(src:Source) = {
    curInput = src;
    nextch;
  }
  */

  def pushExternal(systemId:String) = {
    //Console.print("BEFORE PUSH, curInput = $"+curInput.descr);
    //Console.println(" stack = "+inpStack.map { x => "$"+x.descr });

    //Console.print("[PUSHING EXTERNAL "+systemId+"]");
    if(!eof)
      inpStack = curInput :: inpStack;

    curInput = externalSource(systemId);

    //Console.print("AFTER PUSH, curInput = $"+curInput.descr);
    //Console.println(" stack = "+inpStack.map { x => "$"+x.descr });

    nextch;
  }
  def pop() = {

    curInput = inpStack.head;
    inpStack = inpStack.tail;
    ch = curInput.ch;
    pos = curInput.pos;
    eof = false; // must be false, because of places where entity refs occur
    //Console.println("\n AFTER POP, curInput = $"+curInput.descr);
    //Console.println(inpStack.map { x => x.descr });
  }

}
