/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package model
package comment

import reporters.Reporter
import scala.collection._
import scala.util.matching.Regex
import scala.annotation.switch
import scala.reflect.internal.util.{NoPosition, Position}
import language.postfixOps

/** The comment parser transforms raw comment strings into `Comment` objects.
  * Call `parse` to run the parser. Note that the parser is stateless and
  * should only be built once for a given Scaladoc run.
  *
  * @param reporter The reporter on which user messages (error, warnings) should be printed.
  *
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet */
trait CommentFactory { thisFactory: ModelFactory with CommentFactory with MemberLookup=>

  val global: Global
  import global.{ reporter, definitions }

  protected val commentCache = mutable.HashMap.empty[(global.Symbol, TemplateImpl), Comment]

  def addCommentBody(sym: global.Symbol, inTpl: TemplateImpl, docStr: String, docPos: global.Position): global.Symbol = {
    commentCache += (sym, inTpl) -> parse(docStr, docStr, docPos, None)
    sym
  }

  def comment(sym: global.Symbol, currentTpl: Option[DocTemplateImpl], inTpl: DocTemplateImpl): Option[Comment] = {
    val key = (sym, inTpl)
    if (commentCache isDefinedAt key)
      Some(commentCache(key))
    else {
      val c = defineComment(sym, currentTpl, inTpl)
      if (c isDefined) commentCache += (sym, inTpl) -> c.get
      c
    }
  }

  /** A comment is usualy created by the parser, however for some special
    * cases we have to give some `inTpl` comments (parent class for example)
    * to the comment of the symbol.
    * This function manages some of those cases : Param accessor and Primary constructor */
  def defineComment(sym: global.Symbol, currentTpl: Option[DocTemplateImpl], inTpl: DocTemplateImpl):Option[Comment] = {

    //param accessor case
    // We just need the @param argument, we put it into the body
    if( sym.isParamAccessor &&
        inTpl.comment.isDefined &&
        inTpl.comment.get.valueParams.isDefinedAt(sym.encodedName)) {
      val comContent = Some(inTpl.comment.get.valueParams(sym.encodedName))
      Some(createComment(body0 = comContent))
    }

    // Primary constructor case
    // We need some content of the class definition : @constructor for the body,
    // @param and @deprecated, we can add some more if necessary
    else if (sym.isPrimaryConstructor && inTpl.comment.isDefined ) {
      val tplComment = inTpl.comment.get
      // If there is nothing to put into the comment there is no need to create it
      if(tplComment.constructor.isDefined ||
        tplComment.throws != Map.empty ||
        tplComment.valueParams != Map.empty ||
        tplComment.typeParams != Map.empty ||
        tplComment.deprecated.isDefined
        )
        Some(createComment( body0 = tplComment.constructor,
                            throws0 = tplComment.throws,
                            valueParams0 = tplComment.valueParams,
                            typeParams0 = tplComment.typeParams,
                            deprecated0 = tplComment.deprecated
                            ))
      else None
    }

    //other comment cases
    // parse function will make the comment
    else {
      val rawComment = global.expandedDocComment(sym, inTpl.sym).trim
      if (rawComment != "") {
        val tplOpt = if (currentTpl.isDefined) currentTpl else Some(inTpl)
        val c = parse(rawComment, global.rawDocComment(sym), global.docCommentPos(sym), tplOpt)
        Some(c)
      }
      else None
    }

  }

  /* Creates comments with necessary arguments */
  def createComment (
    body0:           Option[Body]     = None,
    authors0:        List[Body]       = List.empty,
    see0:            List[Body]       = List.empty,
    result0:         Option[Body]     = None,
    throws0:         Map[String,Body] = Map.empty,
    valueParams0:    Map[String,Body] = Map.empty,
    typeParams0:     Map[String,Body] = Map.empty,
    version0:        Option[Body]     = None,
    since0:          Option[Body]     = None,
    todo0:           List[Body]       = List.empty,
    deprecated0:     Option[Body]     = None,
    note0:           List[Body]       = List.empty,
    example0:        List[Body]       = List.empty,
    constructor0:    Option[Body]     = None,
    source0:         Option[String]   = None,
    inheritDiagram0: List[String]     = List.empty,
    contentDiagram0: List[String]     = List.empty,
    group0:          Option[Body]     = None,
    groupDesc0:      Map[String,Body] = Map.empty,
    groupNames0:     Map[String,Body] = Map.empty,
    groupPrio0:      Map[String,Body] = Map.empty
  ) : Comment = new Comment{
    val body           = if(body0 isDefined) body0.get else Body(Seq.empty)
    val authors        = authors0
    val see            = see0
    val result         = result0
    val throws         = throws0
    val valueParams    = valueParams0
    val typeParams     = typeParams0
    val version        = version0
    val since          = since0
    val todo           = todo0
    val deprecated     = deprecated0
    val note           = note0
    val example        = example0
    val constructor    = constructor0
    val source         = source0
    val inheritDiagram = inheritDiagram0
    val contentDiagram = contentDiagram0
    val groupDesc      = groupDesc0
    val group          =
      group0 match {
        case Some(Body(List(Paragraph(Chain(List(Summary(Text(groupId)))))))) => Some(groupId.toString.trim)
        case _                                                                => None
      }
    val groupPrio      = groupPrio0 flatMap {
      case (group, body) =>
        try {
          body match {
            case Body(List(Paragraph(Chain(List(Summary(Text(prio))))))) => List(group -> prio.toInt)
            case _                                                       => List()
          }
        } catch {
          case _: java.lang.NumberFormatException => List()
        }
    }
    val groupNames     = groupNames0 flatMap {
      case (group, body) =>
        try {
          body match {
            case Body(List(Paragraph(Chain(List(Summary(Text(name))))))) if (!name.trim.contains("\n")) => List(group -> (name.trim))
            case _                                                       => List()
          }
        } catch {
          case _: java.lang.NumberFormatException => List()
        }
    }

  }

  protected val endOfText = '\u0003'
  protected val endOfLine = '\u000A'

  /** Something that should not have happened, happened, and Scaladoc should exit. */
  protected def oops(msg: String): Nothing =
    throw FatalError("program logic: " + msg)

  /** The body of a line, dropping the (optional) start star-marker,
    * one leading whitespace and all trailing whitespace. */
  protected val CleanCommentLine =
    new Regex("""(?:\s*\*\s?)?(.*)""")

  /** Dangerous HTML tags that should be replaced by something safer,
    * such as wiki syntax, or that should be dropped. */
  protected val DangerousTags =
    new Regex("""<(/?(div|ol|ul|li|h[1-6]|p))( [^>]*)?/?>|<!--.*-->""")

  /** Maps a dangerous HTML tag to a safe wiki replacement, or an empty string
    * if it cannot be salvaged. */
  protected def htmlReplacement(mtch: Regex.Match): String = mtch.group(1) match {
    case "p" | "div" => "\n\n"
    case "h1"  => "\n= "
    case "/h1" => " =\n"
    case "h2"  => "\n== "
    case "/h2" => " ==\n"
    case "h3"  => "\n=== "
    case "/h3" => " ===\n"
    case "h4" | "h5" | "h6" => "\n==== "
    case "/h4" | "/h5" | "/h6" => " ====\n"
    case "li" => "\n *  - "
    case _ => ""
  }

  /** Javadoc tags that should be replaced by something useful, such as wiki
    * syntax, or that should be dropped. */
  protected val JavadocTags =
    new Regex("""\{\@(code|docRoot|inheritDoc|link|linkplain|literal|value)([^}]*)\}""")

  /** Maps a javadoc tag to a useful wiki replacement, or an empty string if it cannot be salvaged. */
  protected def javadocReplacement(mtch: Regex.Match): String = mtch.group(1) match {
    case "code" => "`" + mtch.group(2) + "`"
    case "docRoot"  => ""
    case "inheritDoc" => ""
    case "link"  => "`" + mtch.group(2) + "`"
    case "linkplain" => "`" + mtch.group(2) + "`"
    case "literal"  => mtch.group(2)
    case "value" => "`" + mtch.group(2) + "`"
    case _ => ""
  }

  /** Safe HTML tags that can be kept. */
  protected val SafeTags =
    new Regex("""((&\w+;)|(&#\d+;)|(</?(abbr|acronym|address|area|a|bdo|big|blockquote|br|button|b|caption|cite|code|col|colgroup|dd|del|dfn|em|fieldset|form|hr|img|input|ins|i|kbd|label|legend|link|map|object|optgroup|option|param|pre|q|samp|select|small|span|strong|sub|sup|table|tbody|td|textarea|tfoot|th|thead|tr|tt|var)( [^>]*)?/?>))""")

  protected val safeTagMarker = '\u000E'

  /** A Scaladoc tag not linked to a symbol and not followed by text */
  protected val SingleTag =
    new Regex("""\s*@(\S+)\s*""")

  /** A Scaladoc tag not linked to a symbol. Returns the name of the tag, and the rest of the line. */
  protected val SimpleTag =
    new Regex("""\s*@(\S+)\s+(.*)""")

  /** A Scaladoc tag linked to a symbol. Returns the name of the tag, the name
    * of the symbol, and the rest of the line. */
  protected val SymbolTag =
    new Regex("""\s*@(param|tparam|throws|groupdesc|groupname|groupprio)\s+(\S*)\s*(.*)""")

  /** The start of a scaladoc code block */
  protected val CodeBlockStart =
    new Regex("""(.*?)((?:\{\{\{)|(?:\u000E<pre(?: [^>]*)?>\u000E))(.*)""")

  /** The end of a scaladoc code block */
  protected val CodeBlockEnd =
    new Regex("""(.*?)((?:\}\}\})|(?:\u000E</pre>\u000E))(.*)""")

  /** A key used for a tag map. The key is built from the name of the tag and
    * from the linked symbol if the tag has one.
    * Equality on tag keys is structural. */
  protected sealed abstract class TagKey {
    def name: String
  }

  protected final case class SimpleTagKey(name: String) extends TagKey
  protected final case class SymbolTagKey(name: String, symbol: String) extends TagKey

  /** Parses a raw comment string into a `Comment` object.
    * @param comment The expanded comment string (including start and end markers) to be parsed.
    * @param src     The raw comment source string.
    * @param pos     The position of the comment in source. */
  protected def parse(comment: String, src: String, pos: Position, inTplOpt: Option[DocTemplateImpl] = None): Comment = {
    assert(!inTplOpt.isDefined || inTplOpt.get != null)

    /** The cleaned raw comment as a list of lines. Cleaning removes comment
      * start and end markers, line start markers  and unnecessary whitespace. */
    def clean(comment: String): List[String] = {
      def cleanLine(line: String): String = {
        //replaceAll removes trailing whitespaces
        line.replaceAll("""\s+$""", "") match {
          case CleanCommentLine(ctl) => ctl
          case tl => tl
        }
      }
      val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
      val safeComment = DangerousTags.replaceAllIn(strippedComment, { htmlReplacement(_) })
      val javadoclessComment = JavadocTags.replaceAllIn(safeComment, { javadocReplacement(_) })
      val markedTagComment =
        SafeTags.replaceAllIn(javadoclessComment, { mtch =>
          java.util.regex.Matcher.quoteReplacement(safeTagMarker + mtch.matched + safeTagMarker)
        })
      markedTagComment.lines.toList map (cleanLine(_))
    }

    /** Parses a comment (in the form of a list of lines) to a `Comment`
      * instance, recursively on lines. To do so, it splits the whole comment
      * into main body and tag bodies, then runs the `WikiParser` on each body
      * before creating the comment instance.
      *
      * @param docBody     The body of the comment parsed until now.
      * @param tags        All tags parsed until now.
      * @param lastTagKey  The last parsed tag, or `None` if the tag section hasn't started. Lines that are not tagged
      *                    are part of the previous tag or, if none exists, of the body.
      * @param remaining   The lines that must still recursively be parsed.
      * @param inCodeBlock Whether the next line is part of a code block (in which no tags must be read). */
    def parse0 (
      docBody: StringBuilder,
      tags: Map[TagKey, List[String]],
      lastTagKey: Option[TagKey],
      remaining: List[String],
      inCodeBlock: Boolean
    ): Comment = remaining match {

      case CodeBlockStart(before, marker, after) :: ls if (!inCodeBlock) =>
        if (!before.trim.isEmpty && !after.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, before :: marker :: after :: ls, false)
        else if (!before.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, before :: marker :: ls, false)
        else if (!after.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, marker :: after :: ls, true)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            parse0(docBody, tags + (key -> value), lastTagKey, ls, true)
          case None =>
            parse0(docBody append endOfLine append marker, tags, lastTagKey, ls, true)
        }

      case CodeBlockEnd(before, marker, after) :: ls =>
        if (!before.trim.isEmpty && !after.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, before :: marker :: after :: ls, true)
        if (!before.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, before :: marker :: ls, true)
        else if (!after.trim.isEmpty)
          parse0(docBody, tags, lastTagKey, marker :: after :: ls, false)
        else lastTagKey match {
          case Some(key) =>
            val value =
              ((tags get key): @unchecked) match {
                case Some(b :: bs) => (b + endOfLine + marker) :: bs
                case None => oops("lastTagKey set when no tag exists for key")
              }
            parse0(docBody, tags + (key -> value), lastTagKey, ls, false)
          case None =>
            parse0(docBody append endOfLine append marker, tags, lastTagKey, ls, false)
        }

      case SymbolTag(name, sym, body) :: ls if (!inCodeBlock) =>
        val key = SymbolTagKey(name, sym)
        val value = body :: tags.getOrElse(key, Nil)
        parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

      case SimpleTag(name, body) :: ls if (!inCodeBlock) =>
        val key = SimpleTagKey(name)
        val value = body :: tags.getOrElse(key, Nil)
        parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

      case SingleTag(name) :: ls if (!inCodeBlock) =>
        val key = SimpleTagKey(name)
        val value = "" :: tags.getOrElse(key, Nil)
        parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

      case line :: ls if (lastTagKey.isDefined) =>
        val key = lastTagKey.get
        val value =
          ((tags get key): @unchecked) match {
            case Some(b :: bs) => (b + endOfLine + line) :: bs
            case None => oops("lastTagKey set when no tag exists for key")
          }
        parse0(docBody, tags + (key -> value), lastTagKey, ls, inCodeBlock)

      case line :: ls =>
        if (docBody.length > 0) docBody append endOfLine
        docBody append line
        parse0(docBody, tags, lastTagKey, ls, inCodeBlock)

      case Nil =>
        // Take the {inheritance, content} diagram keys aside, as it doesn't need any parsing
        val inheritDiagramTag = SimpleTagKey("inheritanceDiagram")
        val contentDiagramTag = SimpleTagKey("contentDiagram")

        val inheritDiagramText: List[String] = tags.get(inheritDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val contentDiagramText: List[String] = tags.get(contentDiagramTag) match {
          case Some(list) => list
          case None => List.empty
        }

        val stripTags=List(inheritDiagramTag, contentDiagramTag, SimpleTagKey("template"), SimpleTagKey("documentable"))
        val tagsWithoutDiagram = tags.filterNot(pair => stripTags.contains(pair._1))

        val bodyTags: mutable.Map[TagKey, List[Body]] =
          mutable.Map(tagsWithoutDiagram mapValues {tag => tag map (parseWiki(_, pos, inTplOpt))} toSeq: _*)

        def oneTag(key: SimpleTagKey): Option[Body] =
          ((bodyTags remove key): @unchecked) match {
            case Some(r :: rs) =>
              if (!rs.isEmpty) reporter.warning(pos, "Only one '@" + key.name + "' tag is allowed")
              Some(r)
            case None => None
          }

        def allTags(key: SimpleTagKey): List[Body] =
          (bodyTags remove key) getOrElse Nil

        def allSymsOneTag(key: TagKey): Map[String, Body] = {
          val keys: Seq[SymbolTagKey] =
            bodyTags.keys.toSeq flatMap {
              case stk: SymbolTagKey if (stk.name == key.name) => Some(stk)
              case stk: SimpleTagKey if (stk.name == key.name) =>
                reporter.warning(pos, "Tag '@" + stk.name + "' must be followed by a symbol name")
                None
              case _ => None
            }
          val pairs: Seq[(String, Body)] =
            for (key <- keys) yield {
              val bs = (bodyTags remove key).get
              if (bs.length > 1)
                reporter.warning(pos, "Only one '@" + key.name + "' tag for symbol " + key.symbol + " is allowed")
              (key.symbol, bs.head)
            }
          Map.empty[String, Body] ++ pairs
        }

        val com = createComment (
          body0           = Some(parseWiki(docBody.toString, pos, inTplOpt)),
          authors0        = allTags(SimpleTagKey("author")),
          see0            = allTags(SimpleTagKey("see")),
          result0         = oneTag(SimpleTagKey("return")),
          throws0         = allSymsOneTag(SimpleTagKey("throws")),
          valueParams0    = allSymsOneTag(SimpleTagKey("param")),
          typeParams0     = allSymsOneTag(SimpleTagKey("tparam")),
          version0        = oneTag(SimpleTagKey("version")),
          since0          = oneTag(SimpleTagKey("since")),
          todo0           = allTags(SimpleTagKey("todo")),
          deprecated0     = oneTag(SimpleTagKey("deprecated")),
          note0           = allTags(SimpleTagKey("note")),
          example0        = allTags(SimpleTagKey("example")),
          constructor0    = oneTag(SimpleTagKey("constructor")),
          source0         = Some(clean(src).mkString("\n")),
          inheritDiagram0 = inheritDiagramText,
          contentDiagram0 = contentDiagramText,
          group0          = oneTag(SimpleTagKey("group")),
          groupDesc0      = allSymsOneTag(SimpleTagKey("groupdesc")),
          groupNames0     = allSymsOneTag(SimpleTagKey("groupname")),
          groupPrio0      = allSymsOneTag(SimpleTagKey("groupprio"))
        )

        for ((key, _) <- bodyTags)
          reporter.warning(pos, "Tag '@" + key.name + "' is not recognised")

        com

    }

    parse0(new StringBuilder(comment.size), Map.empty, None, clean(comment), false)

  }

  /** Parses a string containing wiki syntax into a `Comment` object.
    * Note that the string is assumed to be clean:
    *  - Removed Scaladoc start and end markers.
    *  - Removed start-of-line star and one whitespace afterwards (if present).
    *  - Removed all end-of-line whitespace.
    *  - Only `endOfLine` is used to mark line endings. */
  def parseWiki(string: String, pos: Position, inTplOpt: Option[DocTemplateImpl]): Body = {
    assert(!inTplOpt.isDefined || inTplOpt.get != null)

    new WikiParser(string, pos, inTplOpt).document()
  }

  /** TODO
    *
    * @author Ingo Maier
    * @author Manohar Jonnalagedda
    * @author Gilles Dubochet */
  protected final class WikiParser(val buffer: String, pos: Position, inTplOpt: Option[DocTemplateImpl]) extends CharReader(buffer) { wiki =>
    assert(!inTplOpt.isDefined || inTplOpt.get != null)

    var summaryParsed = false

    def document(): Body = {
      nextChar()
      val blocks = new mutable.ListBuffer[Block]
      while (char != endOfText)
        blocks += block()
      Body(blocks.toList)
    }

    /* BLOCKS */

    /** {{{ block ::= code | title | hrule | para }}} */
    def block(): Block = {
      if (checkSkipInitWhitespace("{{{"))
        code()
      else if (checkSkipInitWhitespace('='))
        title()
      else if (checkSkipInitWhitespace("----"))
        hrule()
      else if (checkList)
        listBlock
      else {
        para()
      }
    }

    /** listStyle ::= '-' spc | '1.' spc | 'I.' spc | 'i.' spc | 'A.' spc | 'a.' spc
      * Characters used to build lists and their constructors */
    protected val listStyles = Map[String, (Seq[Block] => Block)]( // TODO Should this be defined at some list companion?
      "- "  -> ( UnorderedList(_) ),
      "1. " -> ( OrderedList(_,"decimal") ),
      "I. " -> ( OrderedList(_,"upperRoman") ),
      "i. " -> ( OrderedList(_,"lowerRoman") ),
      "A. " -> ( OrderedList(_,"upperAlpha") ),
      "a. " -> ( OrderedList(_,"lowerAlpha") )
    )

    /** Checks if the current line is formed with more than one space and one the listStyles */
    def checkList =
      (countWhitespace > 0) && (listStyles.keys exists { checkSkipInitWhitespace(_) })

    /** {{{
      * nListBlock ::= nLine { mListBlock }
      *      nLine ::= nSpc listStyle para '\n'
      * }}}
      * Where n and m stand for the number of spaces. When `m > n`, a new list is nested. */
    def listBlock: Block = {

      /** Consumes one list item block and returns it, or None if the block is
        * not a list or a different list. */
      def listLine(indent: Int, style: String): Option[Block] =
        if (countWhitespace > indent && checkList)
          Some(listBlock)
        else if (countWhitespace != indent || !checkSkipInitWhitespace(style))
          None
        else {
          jumpWhitespace()
          jump(style)
          val p = Paragraph(inline(false))
          blockEnded("end of list line ")
          Some(p)
        }

      /** Consumes all list item blocks (possibly with nested lists) of the
        * same list and returns the list block. */
      def listLevel(indent: Int, style: String): Block = {
        val lines = mutable.ListBuffer.empty[Block]
        var line: Option[Block] = listLine(indent, style)
        while (line.isDefined) {
          lines += line.get
          line = listLine(indent, style)
        }
        val constructor = listStyles(style)
        constructor(lines)
      }

      val indent = countWhitespace
      val style = (listStyles.keys find { checkSkipInitWhitespace(_) }).getOrElse(listStyles.keys.head)
      listLevel(indent, style)
    }

    def code(): Block = {
      jumpWhitespace()
      jump("{{{")
      readUntil("}}}")
      if (char == endOfText)
        reportError(pos, "unclosed code block")
      else
        jump("}}}")
      blockEnded("code block")
      Code(normalizeIndentation(getRead))
    }

    /** {{{ title ::= ('=' inline '=' | "==" inline "==" | ...) '\n' }}} */
    def title(): Block = {
      jumpWhitespace()
      val inLevel = repeatJump("=")
      val text = inline(check("=" * inLevel))
      val outLevel = repeatJump("=", inLevel)
      if (inLevel != outLevel)
        reportError(pos, "unbalanced or unclosed heading")
      blockEnded("heading")
      Title(text, inLevel)
    }

    /** {{{ hrule ::= "----" { '-' } '\n' }}} */
    def hrule(): Block = {
      jumpWhitespace()
      repeatJump("-")
      blockEnded("horizontal rule")
      HorizontalRule()
    }

    /** {{{ para ::= inline '\n' }}} */
    def para(): Block = {
      val p =
        if (summaryParsed)
          Paragraph(inline(false))
        else {
          val s = summary()
          val r =
            if (checkParaEnded) List(s) else List(s, inline(false))
          summaryParsed = true
          Paragraph(Chain(r))
        }
      while (char == endOfLine && char != endOfText)
        nextChar()
      p
    }

    /* INLINES */

    val OPEN_TAG = "^<([A-Za-z]+)( [^>]*)?(/?)>$".r
    val CLOSE_TAG = "^</([A-Za-z]+)>$".r
    private def readHTMLFrom(begin: HtmlTag): String = {
      val list = mutable.ListBuffer.empty[String]
      val stack = mutable.ListBuffer.empty[String]

      begin.close match {
        case Some(HtmlTag(CLOSE_TAG(s))) =>
          stack += s
        case _ =>
          return ""
      }

      do {
        readUntil { char == safeTagMarker || char == endOfText }
        val str = getRead()
        nextChar()

        list += str

        str match {
          case OPEN_TAG(s, _, standalone) => {
            if (standalone != "/") {
              stack += s
            }
          }
          case CLOSE_TAG(s) => {
            if (s == stack.last) {
              stack.remove(stack.length-1)
            }
          }
          case _ => ;
        }
      } while (stack.length > 0 && char != endOfText)

      list mkString ""
    }

    def inline(isInlineEnd: => Boolean): Inline = {

      def inline0(): Inline = {
        if (char == safeTagMarker) {
          val tag = htmlTag()
          HtmlTag(tag.data + readHTMLFrom(tag))
        }
        else if (check("'''")) bold()
        else if (check("''")) italic()
        else if (check("`"))  monospace()
        else if (check("__")) underline()
        else if (check("^"))  superscript()
        else if (check(",,")) subscript()
        else if (check("[[")) link()
        else {
          readUntil { char == safeTagMarker || check("''") || char == '`' || check("__") || char == '^' || check(",,") || check("[[") || check("{{") || isInlineEnd || checkParaEnded || char == endOfLine }
          Text(getRead())
        }
      }

      val inlines: List[Inline] = {
        val iss = mutable.ListBuffer.empty[Inline]
        iss += inline0()
        while (!isInlineEnd && !checkParaEnded) {
          val skipEndOfLine = if (char == endOfLine) {
            nextChar()
            true
          } else {
            false
          }

          val current = inline0()
          (iss.last, current) match {
            case (Text(t1), Text(t2)) if skipEndOfLine =>
              iss.update(iss.length - 1, Text(t1 + endOfLine + t2))
            case (i1, i2) if skipEndOfLine =>
              iss ++= List(Text(endOfLine.toString), i2)
            case _ => iss += current
          }
        }
        iss.toList
      }

      inlines match {
        case Nil => Text("")
        case i :: Nil => i
        case is => Chain(is)
      }

    }

    def htmlTag(): HtmlTag = {
      jump(safeTagMarker)
      readUntil(safeTagMarker)
      if (char != endOfText) jump(safeTagMarker)
      var read = getRead
      HtmlTag(read)
    }

    def bold(): Inline = {
      jump("'''")
      val i = inline(check("'''"))
      jump("'''")
      Bold(i)
    }

    def italic(): Inline = {
      jump("''")
      val i = inline(check("''"))
      jump("''")
      Italic(i)
    }

    def monospace(): Inline = {
      jump("`")
      val i = inline(check("`"))
      jump("`")
      Monospace(i)
    }

    def underline(): Inline = {
      jump("__")
      val i = inline(check("__"))
      jump("__")
      Underline(i)
    }

    def superscript(): Inline = {
      jump("^")
      val i = inline(check("^"))
      if (jump("^")) {
        Superscript(i)
      } else {
        Chain(Seq(Text("^"), i))
      }
    }

    def subscript(): Inline = {
      jump(",,")
      val i = inline(check(",,"))
      jump(",,")
      Subscript(i)
    }

    def summary(): Inline = {
      val i = inline(check("."))
      Summary(
        if (jump("."))
          Chain(List(i, Text(".")))
        else
          i
      )
    }

    def link(): Inline = {
      val SchemeUri = """([^:]+:.*)""".r
      jump("[[")
      var parens = 1
      readUntil { parens += 1; !check("[") }
      getRead // clear the buffer
      val start = "[" * parens
      val stop  = "]" * parens
      //println("link with " + parens + " matching parens")
      readUntil { check(stop) || check(" ") }
      val target = getRead()
      val title =
        if (!check(stop)) Some({
          jump(" ")
          inline(check(stop))
        })
        else None
      jump(stop)

      (target, title) match {
        case (SchemeUri(uri), optTitle) =>
          Link(uri, optTitle getOrElse Text(uri))
        case (qualName, optTitle) =>
          makeEntityLink(optTitle getOrElse Text(target), pos, target, inTplOpt)
      }
    }

    /* UTILITY */

    /** {{{ eol ::= { whitespace } '\n' }}} */
    def blockEnded(blockType: String): Unit = {
      if (char != endOfLine && char != endOfText) {
        reportError(pos, "no additional content on same line after " + blockType)
        jumpUntil(endOfLine)
      }
      while (char == endOfLine)
        nextChar()
    }

    /**
     *  Eliminates the (common) leading spaces in all lines, based on the first line
     *  For indented pieces of code, it reduces the indent to the least whitespace prefix:
     *    {{{
     *       indented example
     *       another indented line
     *       if (condition)
     *         then do something;
     *       ^ this is the least whitespace prefix
     *    }}}
     */
    def normalizeIndentation(_code: String): String = {

      var code = _code.trim
      var maxSkip = Integer.MAX_VALUE
      var crtSkip = 0
      var wsArea = true
      var index = 0
      var firstLine = true
      var emptyLine = true

      while (index < code.length) {
        code(index) match {
          case ' ' =>
            if (wsArea)
              crtSkip += 1
          case c =>
            wsArea = (c == '\n')
            maxSkip = if (firstLine || emptyLine) maxSkip else if (maxSkip <= crtSkip) maxSkip else crtSkip
            crtSkip = if (c == '\n') 0 else crtSkip
            firstLine = if (c == '\n') false else firstLine
            emptyLine = if (c == '\n') true else false
        }
        index += 1
      }

      if (maxSkip == 0)
        code
      else {
        index = 0
        val builder = new StringBuilder
        while (index < code.length) {
          builder.append(code(index))
          if (code(index) == '\n') {
            // we want to skip as many spaces are available, if there are less spaces (like on empty lines, do not
            // over-consume them)
            index += 1
            val limit = index + maxSkip
            while ((index < code.length) && (code(index) == ' ') && index < limit)
              index += 1
          }
          else
            index += 1
        }
        builder.toString
      }
    }

    def checkParaEnded(): Boolean = {
      (char == endOfText) ||
      ((char == endOfLine) && {
        val poff = offset
        val pc = char
        nextChar() // read EOL
        val ok = {
          checkSkipInitWhitespace(endOfLine) ||
          checkSkipInitWhitespace('=') ||
          checkSkipInitWhitespace("{{{") ||
          checkList ||
          checkSkipInitWhitespace('\u003D')
        }
        offset = poff
        char = pc
        ok
      })
    }

    def reportError(pos: Position, message: String) {
      reporter.warning(pos, message)
    }
  }

  protected sealed class CharReader(buffer: String) { reader =>

    var char: Char = _
    var offset: Int = 0

    final def nextChar() {
      if (offset >= buffer.length)
        char = endOfText
      else {
        char = buffer charAt offset
        offset += 1
      }
    }

    final def check(chars: String): Boolean = {
      val poff = offset
      val pc = char
      val ok = jump(chars)
      offset = poff
      char = pc
      ok
    }

    def checkSkipInitWhitespace(c: Char): Boolean = {
      val poff = offset
      val pc = char
      jumpWhitespace()
      val ok = jump(c)
      offset = poff
      char = pc
      ok
    }

    def checkSkipInitWhitespace(chars: String): Boolean = {
      val poff = offset
      val pc = char
      jumpWhitespace()
      val (ok0, chars0) =
        if (chars.charAt(0) == ' ')
          (offset > poff, chars substring 1)
        else
          (true, chars)
      val ok = ok0 && jump(chars0)
      offset = poff
      char = pc
      ok
    }

    def countWhitespace: Int = {
      var count = 0
      val poff = offset
      val pc = char
      while (isWhitespace(char) && char != endOfText) {
        nextChar()
        count += 1
      }
      offset = poff
      char = pc
      count
    }

    /* JUMPERS */

    /** jumps a character and consumes it
      * @return true only if the correct character has been jumped */
    final def jump(ch: Char): Boolean = {
      if (char == ch) {
        nextChar()
        true
      }
      else false
    }

    /** jumps all the characters in chars, consuming them in the process.
      * @return true only if the correct characters have been jumped */
    final def jump(chars: String): Boolean = {
      var index = 0
      while (index < chars.length && char == chars.charAt(index) && char != endOfText) {
        nextChar()
        index += 1
      }
      index == chars.length
    }

    final def checkedJump(chars: String): Boolean = {
      val poff = offset
      val pc = char
      val ok = jump(chars)
      if (!ok) {
        offset = poff
        char = pc
      }
      ok
    }

    final def repeatJump(chars: String, max: Int): Int = {
      var count = 0
      var more = true
      while (more && count < max) {
        if (!checkedJump(chars))
          more = false
        else
          count += 1
      }
      count
    }

    final def repeatJump(chars: String): Int = {
      var count = 0
      var more = true
      while (more) {
        if (!checkedJump(chars))
          more = false
        else
          count += 1
      }
      count
    }

    final def jumpUntil(ch: Char): Int = {
      var count = 0
      while (char != ch && char != endOfText) {
        nextChar()
        count += 1
      }
      count
    }

    final def jumpUntil(chars: String): Int = {
      assert(chars.length > 0)
      var count = 0
      val c = chars.charAt(0)
      while (!check(chars) && char != endOfText) {
        nextChar()
        while (char != c && char != endOfText) {
          nextChar()
          count += 1
        }
      }
      count
    }

    final def jumpUntil(pred: => Boolean): Int = {
      var count = 0
      while (!pred && char != endOfText) {
        nextChar()
        count += 1
      }
      count
    }

    def jumpWhitespace() = jumpUntil(!isWhitespace(char))

    /* READERS */

    private val readBuilder = new mutable.StringBuilder

    final def getRead(): String = {
      val bld = readBuilder.toString
      readBuilder.clear()
      if (bld.length < 6) bld.intern else bld
    }

    final def readUntil(ch: Char): Int = {
      var count = 0
      while (char != ch && char != endOfText) {
        readBuilder += char
        nextChar()
      }
      count
    }

    final def readUntil(chars: String): Int = {
      assert(chars.length > 0)
      var count = 0
      val c = chars.charAt(0)
      while (!check(chars) && char != endOfText) {
        readBuilder += char
        nextChar()
        while (char != c && char != endOfText) {
          readBuilder += char
          nextChar()
        }
      }
      count
    }

    final def readUntil(pred: => Boolean): Int = {
      var count = 0
      while (!pred && char != endOfText) {
        readBuilder += char
        nextChar()
      }
      count
    }

    /* CHARS CLASSES */

    def isWhitespace(c: Char) = c == ' ' || c == '\t'

  }

}
