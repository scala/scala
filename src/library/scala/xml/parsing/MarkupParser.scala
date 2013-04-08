/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package parsing

import scala.io.Source
import scala.xml.dtd._
import Utility.Escapes.{ pairs => unescape }

/**
 * An XML parser.
 *
 * Parses XML 1.0, invokes callback methods of a `MarkupHandler` and returns
 * whatever the markup handler returns. Use `ConstructingParser` if you just
 * want to parse XML to construct instances of `scala.xml.Node`.
 *
 * While XML elements are returned, DTD declarations - if handled - are
 * collected using side-effects.
 *
 * @author  Burak Emir
 * @version 1.0
 */
trait MarkupParser extends MarkupParserCommon with TokenTests
{
  self: MarkupParser with MarkupHandler =>

  type PositionType = Int
  type InputType    = Source
  type ElementType  = NodeSeq
  type AttributesType = (MetaData, NamespaceBinding)
  type NamespaceType = NamespaceBinding

  def truncatedError(msg: String): Nothing = throw FatalError(msg)
  def errorNoEnd(tag: String) = throw FatalError("expected closing tag of " + tag)

  def xHandleError(that: Char, msg: String) = reportSyntaxError(msg)

  val input: Source

  /** if true, does not remove surplus whitespace */
  val preserveWS: Boolean

  def externalSource(systemLiteral: String): Source

  //
  // variables, values
  //

  protected var curInput: Source = input

  // See ticket #3720 for motivations.
  private class WithLookAhead(underlying: Source) extends Source {
    private val queue = scala.collection.mutable.Queue[Char]()
    def lookahead(): BufferedIterator[Char] = {
      val iter = queue.iterator ++ new Iterator[Char] {
        def hasNext = underlying.hasNext
        def next() = { val x = underlying.next(); queue += x; x }
      }
      iter.buffered
    }
    val iter = new Iterator[Char] {
      def hasNext = underlying.hasNext || !queue.isEmpty
      def next() = if (!queue.isEmpty) queue.dequeue() else underlying.next()
    }
  }

  def lookahead(): BufferedIterator[Char] = curInput match {
    case curInputWLA:WithLookAhead =>
      curInputWLA.lookahead()
    case _ =>
      val newInput = new WithLookAhead(curInput)
      curInput = newInput
      newInput.lookahead()
  }


  /** the handler of the markup, returns this */
  private val handle: MarkupHandler = this

  /** stack of inputs */
  var inpStack: List[Source] = Nil

  /** holds the position in the source file */
  var pos: Int = _

  /* used when reading external subset */
  var extIndex = -1

  /** holds temporary values of pos */
  var tmppos: Int = _

  /** holds the next character */
  var nextChNeeded: Boolean = false
  var reachedEof: Boolean = false
  var lastChRead: Char = _
  def ch: Char = {
    if (nextChNeeded) {
      if (curInput.hasNext) {
        lastChRead = curInput.next()
        pos = curInput.pos
      } else {
        val ilen = inpStack.length
        //Console.println("  ilen = "+ilen+ " extIndex = "+extIndex);
        if ((ilen != extIndex) && (ilen > 0)) {
          /* for external source, inpStack == Nil ! need notify of eof! */
          pop()
        } else {
          reachedEof = true
          lastChRead = 0.asInstanceOf[Char]
        }
      }
      nextChNeeded = false
    }
    lastChRead
  }

  /** character buffer, for names */
  protected val cbuf = new StringBuilder()

  var dtd: DTD = null

  protected var doc: Document = null

  def eof: Boolean = { ch; reachedEof }

  //
  // methods
  //

  /** {{{
   *  <? prolog ::= xml S ... ?>
   *  }}} */
  def xmlProcInstr(): MetaData = {
    xToken("xml")
    xSpace()
    val (md,scp) = xAttributes(TopScope)
    if (scp != TopScope)
      reportSyntaxError("no xmlns definitions here, please.")
    xToken('?')
    xToken('>')
    md
  }

  /** Factored out common code.
   */
  private def prologOrTextDecl(isProlog: Boolean): (Option[String], Option[String], Option[Boolean]) = {
    var info_ver: Option[String] = None
    var info_enc: Option[String] = None
    var info_stdl: Option[Boolean] = None

    val m = xmlProcInstr()
    var n = 0

    if (isProlog)
      xSpaceOpt()

    m("version") match {
      case null =>
      case Text("1.0") => info_ver = Some("1.0"); n += 1
      case _     => reportSyntaxError("cannot deal with versions != 1.0")
    }

    m("encoding") match {
      case null =>
      case Text(enc) =>
        if (!isValidIANAEncoding(enc))
          reportSyntaxError("\"" + enc + "\" is not a valid encoding")
        else {
          info_enc = Some(enc)
          n += 1
        }
    }

    if (isProlog) {
      m("standalone") match {
        case null =>
        case Text("yes") => info_stdl = Some(true);  n += 1
        case Text("no")  => info_stdl = Some(false); n += 1
        case _     => reportSyntaxError("either 'yes' or 'no' expected")
      }
    }

    if (m.length - n != 0) {
      val s = if (isProlog) "SDDecl? " else ""
      reportSyntaxError("VersionInfo EncodingDecl? %sor '?>' expected!" format s)
    }

    (info_ver, info_enc, info_stdl)
  }

  /** {{{
   *  <? prolog ::= xml S?
   *  // this is a bit more lenient than necessary...
   *  }}} */
  def prolog(): (Option[String], Option[String], Option[Boolean]) =
    prologOrTextDecl(isProlog = true)

  /** prolog, but without standalone */
  def textDecl(): (Option[String], Option[String]) =
    prologOrTextDecl(isProlog = false) match { case (x1, x2, _)  => (x1, x2) }

  /** {{{
   *  [22]     prolog      ::= XMLDecl? Misc* (doctypedecl Misc*)?
   *  [23]     XMLDecl     ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
   *  [24]     VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
   *  [25]     Eq          ::= S? '=' S?
   *  [26]     VersionNum  ::= '1.0'
   *  [27]     Misc        ::= Comment | PI | S
   * }}} */
  def document(): Document = {
    doc = new Document()

    this.dtd = null
    var info_prolog: (Option[String], Option[String], Option[Boolean]) = (None, None, None)
    if ('<' != ch) {
      reportSyntaxError("< expected")
      return null
    }

    nextch() // is prolog ?
    var children: NodeSeq = null
    if ('?' == ch) {
      nextch()
      info_prolog = prolog()
      doc.version    = info_prolog._1
      doc.encoding   = info_prolog._2
      doc.standAlone = info_prolog._3

      children = content(TopScope) // DTD handled as side effect
    }
    else {
      val ts = new NodeBuffer()
      content1(TopScope, ts)  // DTD handled as side effect
      ts &+ content(TopScope)
      children = NodeSeq.fromSeq(ts)
    }
    //println("[MarkupParser::document] children now: "+children.toList)
    var elemCount = 0
    var theNode: Node = null
    for (c <- children) c match {
      case _:ProcInstr =>
      case _:Comment =>
      case _:EntityRef => // todo: fix entities, shouldn't be "special"
        reportSyntaxError("no entity references allowed here")
      case s:SpecialNode =>
        if (s.toString.trim().length > 0) //non-empty text nodes not allowed
          elemCount += 2
      case m:Node =>
        elemCount += 1
        theNode = m
    }
    if (1 != elemCount) {
      reportSyntaxError("document must contain exactly one element")
      Console.println(children.toList)
    }

    doc.children = children
    doc.docElem = theNode
    doc
  }

  /** append Unicode character to name buffer*/
  protected def putChar(c: Char) = cbuf append c

  /** As the current code requires you to call nextch once manually
   *  after construction, this method formalizes that suboptimal reality.
   */
  def initialize: this.type = {
    nextch()
    this
  }

  protected def ch_returning_nextch: Char = { val res = ch; nextch(); res }

  def mkAttributes(name: String, pscope: NamespaceBinding): AttributesType =
    if (isNameStart (ch)) xAttributes(pscope)
    else (Null, pscope)

  def mkProcInstr(position: Int, name: String, text: String): ElementType =
    handle.procInstr(position, name, text)

  /** this method tells ch to get the next character when next called */
  def nextch() {
    // Read current ch if needed
    ch

    // Mark next ch to be required
    nextChNeeded = true
  }

  /** parse attribute and create namespace scope, metadata
   *  {{{
   *  [41] Attributes    ::= { S Name Eq AttValue }
   *  }}}
   */
  def xAttributes(pscope: NamespaceBinding): (MetaData, NamespaceBinding) = {
    var scope: NamespaceBinding = pscope
    var aMap: MetaData = Null
    while (isNameStart(ch)) {
      val qname = xName
      xEQ() // side effect
      val value = xAttributeValue()

      Utility.prefix(qname) match {
        case Some("xmlns") =>
          val prefix = qname.substring(6 /*xmlns:*/ , qname.length)
          scope = new NamespaceBinding(prefix, value, scope)

        case Some(prefix) =>
          val key = qname.substring(prefix.length+1, qname.length)
          aMap = new PrefixedAttribute(prefix, key, Text(value), aMap)

        case _ =>
          if( qname == "xmlns" )
            scope = new NamespaceBinding(null, value, scope)
          else
            aMap = new UnprefixedAttribute(qname, Text(value), aMap)
      }

      if ((ch != '/') && (ch != '>') && ('?' != ch))
        xSpace()
    }

    if(!aMap.wellformed(scope))
        reportSyntaxError( "double attribute")

    (aMap,scope)
  }

  /** entity value, terminated by either ' or ". value may not contain &lt;.
   *  {{{
   *       AttValue     ::= `'` { _  } `'`
   *                      | `"` { _ } `"`
   *  }}}
   */
  def xEntityValue(): String = {
    val endch = ch
    nextch()
    while (ch != endch && !eof) {
      putChar(ch)
      nextch()
    }
    nextch()
    val str = cbuf.toString()
    cbuf.length = 0
    str
  }

  /** {{{
   *  '<! CharData ::= [CDATA[ ( {char} - {char}"]]>"{char} ) ']]>'
   *
   *  see [15]
   *  }}} */
  def xCharData: NodeSeq = {
    xToken("[CDATA[")
    def mkResult(pos: Int, s: String): NodeSeq = {
      handle.text(pos, s)
      PCData(s)
    }
    xTakeUntil(mkResult, () => pos, "]]>")
  }

  /** {{{
   *  Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
   *
   * see [15]
   *  }}} */
  def xComment: NodeSeq = {
    val sb: StringBuilder = new StringBuilder()
    xToken("--")
    while (true) {
      if (ch == '-'  && { sb.append(ch); nextch(); ch == '-' }) {
        sb.length = sb.length - 1
        nextch()
        xToken('>')
        return handle.comment(pos, sb.toString())
      } else sb.append(ch)
      nextch()
    }
    throw FatalError("this cannot happen")
  }

  /* todo: move this into the NodeBuilder class */
  def appendText(pos: Int, ts: NodeBuffer, txt: String): Unit = {
    if (preserveWS)
      ts &+ handle.text(pos, txt)
    else
      for (t <- TextBuffer.fromString(txt).toText) {
        ts &+ handle.text(pos, t.text)
      }
  }

  /** {{{
   *  '<' content1 ::=  ...
   *  }}} */
  def content1(pscope: NamespaceBinding, ts: NodeBuffer) {
    ch match {
      case '!' =>
        nextch()
      if ('[' == ch)                 // CDATA
        ts &+ xCharData
      else if ('D' == ch) // doctypedecl, parse DTD // @todo REMOVE HACK
        parseDTD()
      else // comment
        ts &+ xComment
      case '?' =>                    // PI
        nextch()
        ts &+ xProcInstr
      case _   =>
        ts &+ element1(pscope)      // child
    }
  }

  /** {{{
   *  content1 ::=  '<' content1 | '&' charref ...
   *  }}} */
  def content(pscope: NamespaceBinding): NodeSeq = {
    val ts = new NodeBuffer
    var exit = eof
    // todo: optimize seq repr.
    def done = new NodeSeq { val theSeq = ts.toList }

    while (!exit) {
      tmppos = pos
      exit = eof

      if (eof)
        return done

      ch match {
        case '<' => // another tag
          nextch(); ch match {
            case '/'    => exit = true  // end tag
            case _      => content1(pscope, ts)
          }

        // postcond: xEmbeddedBlock == false!
        case '&' => // EntityRef or CharRef
          nextch(); ch match {
            case '#'  =>  // CharacterRef
              nextch()
              val theChar = handle.text(tmppos, xCharRef(() => ch, () => nextch()))
              xToken(';')
              ts &+ theChar
            case _ =>     // EntityRef
              val n = xName
              xToken(';')

              if (unescape contains n) {
                handle.entityRef(tmppos, n)
                ts &+ unescape(n)
              } else push(n)
          }
        case _ => // text content
          appendText(tmppos, ts, xText)
      }
    }
    done
  } // content(NamespaceBinding)

  /** {{{
   *  externalID ::= SYSTEM S syslit
   *                 PUBLIC S pubid S syslit
   *  }}} */
  def externalID(): ExternalID = ch match {
    case 'S' =>
      nextch()
      xToken("YSTEM")
      xSpace()
      val sysID = systemLiteral()
      new SystemID(sysID)
    case 'P' =>
      nextch(); xToken("UBLIC")
      xSpace()
      val pubID = pubidLiteral()
      xSpace()
      val sysID = systemLiteral()
      new PublicID(pubID, sysID)
  }


  /** parses document type declaration and assigns it to instance variable
   *  dtd.
   *  {{{
   *  <! parseDTD ::= DOCTYPE name ... >
   *  }}} */
  def parseDTD() { // dirty but fast
    var extID: ExternalID = null
    if (this.dtd ne null)
      reportSyntaxError("unexpected character (DOCTYPE already defined")
    xToken("DOCTYPE")
    xSpace()
    val n = xName
    xSpace()
    //external ID
    if ('S' == ch || 'P' == ch) {
      extID = externalID()
      xSpaceOpt()
    }

    /* parse external subset of DTD
     */

    if ((null != extID) && isValidating) {

      pushExternal(extID.systemId)
      extIndex = inpStack.length

      extSubset()
      pop()
      extIndex = -1
    }

    if ('[' == ch) { // internal subset
      nextch()
      /* TODO */
      intSubset()
      // TODO: do the DTD parsing?? ?!?!?!?!!
      xToken(']')
      xSpaceOpt()
    }
    xToken('>')
    this.dtd = new DTD {
      /*override var*/ externalID = extID
      /*override val */decls      = handle.decls.reverse
    }
    //this.dtd.initializeEntities();
    if (doc ne null)
      doc.dtd = this.dtd

    handle.endDTD(n)
  }

  def element(pscope: NamespaceBinding): NodeSeq = {
    xToken('<')
    element1(pscope)
  }

  /** {{{
   *  '<' element ::= xmlTag1 '>'  { xmlExpr | '{' simpleExpr '}' } ETag
   *               | xmlTag1 '/' '>'
   *  }}} */
  def element1(pscope: NamespaceBinding): NodeSeq = {
    val pos = this.pos
    val (qname, (aMap, scope)) = xTag(pscope)
    val (pre, local) = Utility.prefix(qname) match {
      case Some(p) => (p, qname drop p.length+1)
      case _       => (null, qname)
    }
    val ts = {
      if (ch == '/') {  // empty element
        xToken("/>")
        handle.elemStart(pos, pre, local, aMap, scope)
        NodeSeq.Empty
      }
      else {           // element with content
        xToken('>')
        handle.elemStart(pos, pre, local, aMap, scope)
        val tmp = content(scope)
        xEndTag(qname)
        tmp
      }
    }
    val res = handle.elem(pos, pre, local, aMap, scope, ts == NodeSeq.Empty, ts)
    handle.elemEnd(pos, pre, local)
    res
  }

  /** Parse character data.
   *
   *  precondition: `xEmbeddedBlock == false` (we are not in a scala block)
   */
  private def xText: String = {
    var exit = false
    while (! exit) {
      putChar(ch)
      nextch()

      exit = eof || ( ch == '<' ) || ( ch == '&' )
    }
    val str = cbuf.toString
    cbuf.length = 0
    str
  }

  /** attribute value, terminated by either ' or ". value may not contain &lt;.
   *  {{{
   *       AttValue     ::= `'` { _ } `'`
   *                      | `"` { _ } `"`
   *  }}} */
  def systemLiteral(): String = {
    val endch = ch
    if (ch != '\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected")
    nextch()
    while (ch != endch && !eof) {
      putChar(ch)
      nextch()
    }
    nextch()
    val str = cbuf.toString()
    cbuf.length = 0
    str
  }

  /** {{{
   *  [12]       PubidLiteral ::=        '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
   *  }}} */
  def pubidLiteral(): String = {
    val endch = ch
    if (ch!='\'' && ch != '"')
      reportSyntaxError("quote ' or \" expected")
    nextch()
    while (ch != endch && !eof) {
      putChar(ch)
      //println("hello '"+ch+"'"+isPubIDChar(ch))
      if (!isPubIDChar(ch))
        reportSyntaxError("char '"+ch+"' is not allowed in public id")
      nextch()
    }
    nextch()
    val str = cbuf.toString
    cbuf.length = 0
    str
  }

  //
  //  dtd parsing
  //

  def extSubset(): Unit = {
    var textdecl: (Option[String],Option[String]) = null
    if (ch == '<') {
      nextch()
      if (ch == '?') {
        nextch()
        textdecl = textDecl()
      } else
        markupDecl1()
    }
    while (!eof)
      markupDecl()
  }

  def markupDecl1() = {
    def doInclude() = {
      xToken('['); while(']' != ch) markupDecl(); nextch() // ']'
    }
    def doIgnore() = {
      xToken('['); while(']' != ch) nextch(); nextch() // ']'
    }
    if ('?' == ch) {
      nextch()
      xProcInstr // simply ignore processing instructions!
    } else {
      xToken('!')
      ch match {
        case '-' =>
          xComment // ignore comments

        case 'E' =>
          nextch()
          if ('L' == ch) {
            nextch()
            elementDecl()
          } else
            entityDecl()

        case 'A' =>
          nextch()
          attrDecl()

        case 'N' =>
          nextch()
          notationDecl()

        case '[' if inpStack.length >= extIndex =>
          nextch()
          xSpaceOpt()
          ch match {
            case '%' =>
              nextch()
              val ent = xName
              xToken(';')
              xSpaceOpt()

              push(ent)
              xSpaceOpt()
              val stmt = xName
              xSpaceOpt()

              stmt match {
                // parameter entity
                case "INCLUDE" => doInclude()
                case "IGNORE"  => doIgnore()
              }
            case 'I' =>
              nextch()
              ch match {
                case 'G' =>
                  nextch()
                  xToken("NORE")
                  xSpaceOpt()
                  doIgnore()
                case 'N' =>
                  nextch()
                  xToken("NCLUDE")
                  doInclude()
              }
          }
        xToken(']')
        xToken('>')

        case _  =>
          curInput.reportError(pos, "unexpected character '"+ch+"', expected some markupdecl")
        while (ch!='>')
          nextch()
      }
    }
  }

  def markupDecl(): Unit = ch match {
    case '%' =>                  // parameter entity reference
      nextch()
      val ent = xName
      xToken(';')
      if (!isValidating)
        handle.peReference(ent)  //  n-v: just create PE-reference
      else
        push(ent)                //    v: parse replacementText

    //peReference
    case '<' =>
      nextch()
      markupDecl1()
    case _ if isSpace(ch) =>
      xSpace()
    case _ =>
      reportSyntaxError("markupdecl: unexpected character '"+ch+"' #" + ch.toInt)
      nextch()
  }

  /**  "rec-xml/#ExtSubset" pe references may not occur within markup declarations
   */
  def intSubset() {
    //Console.println("(DEBUG) intSubset()")
    xSpace()
    while (']' != ch)
      markupDecl()
  }

  /** &lt;! element := ELEMENT
   */
  def elementDecl() {
    xToken("EMENT")
    xSpace()
    val n = xName
    xSpace()
    while ('>' != ch) {
      //Console.println("["+ch+"]")
      putChar(ch)
      nextch()
    }
    //Console.println("END["+ch+"]")
    nextch()
    val cmstr = cbuf.toString()
    cbuf.length = 0
    handle.elemDecl(n, cmstr)
  }

  /** {{{
   *  <! attlist := ATTLIST
   *  }}} */
  def attrDecl() = {
    xToken("TTLIST")
    xSpace()
    val n = xName
    xSpace()
    var attList: List[AttrDecl] = Nil

    // later: find the elemDecl for n
    while ('>' != ch) {
      val aname = xName
      xSpace()
      // could be enumeration (foo,bar) parse this later :-/
      while ('"' != ch && '\'' != ch && '#' != ch && '<' != ch) {
        if (!isSpace(ch))
          cbuf.append(ch)
        nextch()
      }
      val atpe = cbuf.toString
      cbuf.length = 0

      val defdecl: DefaultDecl = ch match {
        case '\'' | '"' =>
          DEFAULT(fixed = false, xAttributeValue())

        case '#' =>
          nextch()
          xName match {
            case "FIXED"    => xSpace() ; DEFAULT(fixed = true, xAttributeValue())
            case "IMPLIED"  => IMPLIED
            case "REQUIRED" => REQUIRED
          }
        case _ =>
          null
      }
      xSpaceOpt()

      attList ::= AttrDecl(aname, atpe, defdecl)
      cbuf.length = 0
    }
    nextch()
    handle.attListDecl(n, attList.reverse)
  }

  /** {{{
   *  <! element := ELEMENT
   *  }}} */
  def entityDecl() = {
    var isParameterEntity = false
    xToken("NTITY")
    xSpace()
    if ('%' == ch) {
      nextch()
      isParameterEntity = true
      xSpace()
    }
    val n = xName
    xSpace()
    ch match {
      case 'S' | 'P' => //sy
        val extID = externalID()
        if (isParameterEntity) {
          xSpaceOpt()
          xToken('>')
          handle.parameterEntityDecl(n, ExtDef(extID))
        } else { // notation?
          xSpace()
          if ('>' != ch) {
            xToken("NDATA")
            xSpace()
            val notat = xName
            xSpaceOpt()
            xToken('>')
            handle.unparsedEntityDecl(n, extID, notat)
          } else {
            nextch()
            handle.parsedEntityDecl(n, ExtDef(extID))
          }
        }

      case '"' | '\'' =>
        val av = xEntityValue()
        xSpaceOpt()
        xToken('>')
        if (isParameterEntity)
          handle.parameterEntityDecl(n, IntDef(av))
        else
          handle.parsedEntityDecl(n, IntDef(av))
    }
    {}
  } // entityDecl

  /** {{{
   *  'N' notationDecl ::= "OTATION"
   *  }}} */
  def notationDecl() {
    xToken("OTATION")
    xSpace()
    val notat = xName
    xSpace()
    val extID = if (ch == 'S') {
      externalID()
    }
    else if (ch == 'P') {
      /* PublicID (without system, only used in NOTATION) */
      nextch()
      xToken("UBLIC")
      xSpace()
      val pubID = pubidLiteral()
      xSpaceOpt()
      val sysID = if (ch != '>')
        systemLiteral()
      else
        null
      new PublicID(pubID, sysID)
    } else {
      reportSyntaxError("PUBLIC or SYSTEM expected")
      scala.sys.error("died parsing notationdecl")
    }
    xSpaceOpt()
    xToken('>')
    handle.notationDecl(notat, extID)
  }

  def reportSyntaxError(pos: Int, str: String) { curInput.reportError(pos, str) }
  def reportSyntaxError(str: String) { reportSyntaxError(pos, str) }
  def reportValidationError(pos: Int, str: String) { reportSyntaxError(pos, str) }

  def push(entityName: String) {
    if (!eof)
      inpStack = curInput :: inpStack

    // can't push before getting next character if needed
    ch

    curInput = replacementText(entityName)
    nextch()
  }

  def pushExternal(systemId: String) {
    if (!eof)
      inpStack = curInput :: inpStack

    // can't push before getting next character if needed
    ch

    curInput = externalSource(systemId)
    nextch()
  }

  def pop() {
    curInput = inpStack.head
    inpStack = inpStack.tail
    lastChRead = curInput.ch
    nextChNeeded = false
    pos = curInput.pos
    reachedEof = false // must be false, because of places where entity refs occur
  }
}
