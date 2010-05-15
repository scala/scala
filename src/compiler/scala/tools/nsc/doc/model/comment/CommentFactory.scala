/* NSC -- new Scala compiler -- Copyright 2007-2010 LAMP/EPFL */

package scala.tools.nsc
package doc
package model
package comment

import reporters.Reporter
import util.Position

import scala.collection._
import scala.util.matching.Regex
import scala.annotation.switch

/** The comment parser transforms raw comment strings into `Comment` objects. Call `parse` to run the parser. Note that
  * the parser is stateless and should only be built once for a given Scaladoc run.
  *
  * @param reporter The reporter on which user messages (error, warnings) should be printed.
  *
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet */
trait CommentFactory { thisFactory: ModelFactory with CommentFactory =>

  val global: Global
  import global.reporter

  private val commentCache = mutable.HashMap.empty[(global.Symbol, TemplateImpl), Comment]

  def addCommentBody(sym: global.Symbol, inTpl: => TemplateImpl, docStr: String, docPos: global.Position): global.Symbol = {
    commentCache += (sym, inTpl) -> parse(docStr, docPos)
    sym
  }

  def comment(sym: global.Symbol, inTpl: => DocTemplateImpl): Option[Comment] = {
    val key = (sym, inTpl)
    if (commentCache isDefinedAt key)
      Some(commentCache(key))
    else { // not reached for use-case comments
      val rawComment = global.expandedDocComment(sym, inTpl.sym).trim
      if (rawComment == "") None else {
        val c = parse(rawComment, global.docCommentPos(sym))
        commentCache += (sym, inTpl) -> c
        Some(c)
      }
    }
  }

  protected val endOfText = '\u0003'
  protected val endOfLine = '\u000A'

  /** Something that should not have happened, happened, and Scaladoc should exit. */
  protected def oops(msg: String): Nothing =
    throw FatalError("program logic: " + msg)

  /** The body of a line, dropping the (optional) start star-marker, one leading whitespace and all trailing whitespace. */
  protected val CleanCommentLine =
    new Regex("""(?:\s*\*\s?)?(.*)""")

  /** Dangerous HTML tags that should be replaced by something safer, such as wiki syntax, or that should be dropped. */
  protected val DangerousTags =
    new Regex("""<(/?(div|ol|ul|li|h[1-6]|p))( [^>]*)?/?>|<!--.*-->""")

  /** Maps a dangerous HTML tag to a safe wiki replacement, or an empty string if it cannot be salvaged. */
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
    case "code" | "/code" => "`"
    case "li" => "\n *  - "
    case _ => ""
  }

  /** Safe HTML tags that can be kept. */
  protected val SafeTags =
    new Regex("""(</?(abbr|acronym|address|area|a|bdo|big|blockquote|br|button|b|caption|code|cite|col|colgroup|dd|del|dfn|em|fieldset|form|hr|img|input|ins|i|kbd|label|legend|link|map|object|optgroup|option|param|pre|q|samp|select|small|span|strong|sub|sup|table|tbody|td|textarea|tfoot|th|thead|tr|tt|var)( [^>]*)?/?>)""")

  protected val safeTagMarker = '\u000E'

  /** A Scaladoc tag not linked to a symbol. Returns the name of the tag, and the rest of the line. */
  protected val SimpleTag =
    new Regex("""\s*@(\S+)\s+(.*)""")

  /** A Scaladoc tag linked to a symbol. Returns the name of the tag, the name of the symbol, and the rest of the
    * line. */
  protected val SymbolTag =
    new Regex("""\s*@(param|tparam|throws)\s+(\S*)\s*(.*)""")

  /** The start of a scaladoc code block */
  protected val CodeBlockStart =
    new Regex("""(.*)\{\{\{(.*)""")

  /** The end of a scaladoc code block */
  protected val CodeBlockEnd =
    new Regex("""(.*)\}\}\}(.*)""")

  /** A key used for a tag map. The key is built from the name of the tag and from the linked symbol if the tag has one.
    * Equality on tag keys is structural. */
  protected sealed abstract class TagKey {
    def name: String
  }

  protected final case class SimpleTagKey(name: String) extends TagKey
  protected final case class SymbolTagKey(name: String, symbol: String) extends TagKey

  /** Parses a raw comment string into a `Comment` object.
    * @param comment The raw comment string (including start and end markers) to be parsed.
    * @param pos     The position of the comment in source. */
  protected def parse(comment: String, pos: Position): Comment = {

    /** The cleaned raw comment as a list of lines. Cleaning removes comment start and end markers, line start markers
      * and unnecessary whitespace. */
    val cleaned: List[String] = {
      def cleanLine(line: String): String = {
        //replaceAll removes trailing whitespaces
        line.replaceAll("""\s+$""", "") match {
          case CleanCommentLine(ctl) => ctl
          case tl => tl
        }
      }
      val strippedComment = comment.trim.stripPrefix("/*").stripSuffix("*/")
      val safeComment = DangerousTags.replaceAllIn(strippedComment, { htmlReplacement(_) })
      val markedTagComment =
        SafeTags.replaceAllIn(safeComment, { mtch =>
          java.util.regex.Matcher.quoteReplacement(safeTagMarker + mtch.matched + safeTagMarker)
        })
      markedTagComment.lines.toList map (cleanLine(_))
    }

    /** Parses a comment (in the form of a list of lines) to a Comment instance, recursively on lines. To do so, it
      * splits the whole comment into main body and tag bodies, then runs the `WikiParser` on each body before creating
      * the comment instance.
      *
      * @param docBody     The body of the comment parsed until now.
      * @param tags        All tags parsed until now.
      * @param lastTagKey  The last parsed tag, or `None` if the tag section hasn't started. Lines that are not tagged
      *                    are part of the previous tag or, if none exists, of the body.
      * @param remaining   The lines that must still recursively be parsed.
      * @param inCodeBlock Whether the next line is part of a code block (in which no tags must be read). */
    def parse0(docBody: String, tags: Map[TagKey, List[String]], lastTagKey: Option[TagKey], remaining: List[String], inCodeBlock: Boolean): Comment = {
      remaining match {

        case CodeBlockStart(before, after) :: ls if (!inCodeBlock) =>
          if (before.trim != "")
            parse0(docBody, tags, lastTagKey, before :: ("{{{" + after) :: ls, false)
          else if (after.trim != "")
            parse0(docBody, tags, lastTagKey, "{{{" :: after :: ls, true)
          else
            parse0(docBody + endOfLine + "{{{", tags, lastTagKey, ls, true)

        case CodeBlockEnd(before, after) :: ls =>
          if (before.trim != "")
            parse0(docBody, tags, lastTagKey, before :: ("}}}" + after) :: ls, true)
          else if (after.trim != "")
            parse0(docBody, tags, lastTagKey, "}}}" :: after :: ls, false)
          else
            parse0(docBody + endOfLine + "}}}", tags, lastTagKey, ls, false)

        case SymbolTag(name, sym, body) :: ls if (!inCodeBlock) =>
          val key = SymbolTagKey(name, sym)
          val value = body :: tags.getOrElse(key, Nil)
          parse0(docBody, tags + (key -> value), Some(key), ls, inCodeBlock)

        case SimpleTag(name, body) :: ls if (!inCodeBlock) =>
          val key = SimpleTagKey(name)
          val value = body :: tags.getOrElse(key, Nil)
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
          val newBody = if (docBody == "") line else docBody + endOfLine + line
          parse0(newBody, tags, lastTagKey, ls, inCodeBlock)

        case Nil =>

          val bodyTags: mutable.Map[TagKey, List[Body]] =
            mutable.Map(tags mapValues (_ map (parseWiki(_, pos))) toSeq: _*)

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

          val com = new Comment {
            val body        = parseWiki(docBody, pos)
            val authors     = allTags(SimpleTagKey("author"))
            val see         = allTags(SimpleTagKey("see"))
            val result      = oneTag(SimpleTagKey("return"))
            val throws      = allSymsOneTag(SimpleTagKey("throws"))
            val valueParams = allSymsOneTag(SimpleTagKey("param"))
            val typeParams  = allSymsOneTag(SimpleTagKey("tparam"))
            val version     = oneTag(SimpleTagKey("version"))
            val since       = oneTag(SimpleTagKey("since"))
            val todo        = allTags(SimpleTagKey("todo"))
            val deprecated  = oneTag(SimpleTagKey("deprecated"))
            val note        = allTags(SimpleTagKey("note"))
            val example     = allTags(SimpleTagKey("example"))
            val short       = body.summary getOrElse Text("no summary matey")
          }

          for ((key, _) <- bodyTags)
            reporter.warning(pos, "Tag '@" + key.name + "' is not recognised")

          com

      }
    }

    parse0("", Map.empty, None, cleaned, false)

  }

  /** Parses a string containing wiki syntax into a `Comment` object. Note that the string is assumed to be clean:
    *  - Removed Scaladoc start and end markers.
    *  - Removed start-of-line star and one whitespace afterwards (if present).
    *  - Removed all end-of-line whitespace.
    *  - Only `endOfLine` is used to mark line endings. */
  def parseWiki(string: String, pos: Position): Body =
    new WikiParser(string.toArray, pos).document()

  /** TODO
    *
    * @author Ingo Maier
    * @author Manohar Jonnalagedda
    * @author Gilles Dubochet */
  protected final class WikiParser(val buffer: Array[Char], pos: Position) extends CharReader(buffer) { wiki =>

    var summaryParsed = false

    def document(): Body = {
      nextChar()
      val blocks = new mutable.ListBuffer[Block]
      while(char != endOfText)
        blocks += block()
      Body(blocks.toList)
    }

    /* BLOCKS */

    /** {{{ block ::= code | title | hrule | para }}} */
    def block(): Block = {
      if (checkSkipInitWhitespace("{{{"))
        code()
      else if (checkSkipInitWhitespace("="))
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
      * Where n and m stand for the number of spaces. When m > n, a new list is nested. */
    def listBlock: Block = {

      /** Consumes one list item block and returns it, or None if the block is not a list or a different list. */
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

      /** Consumes all list item blocks (possibly with nested lists) of the same list and returns the list block. */
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
      Code(getRead)
    }

    /** {{{ title ::= ('=' inline '=' | "==" inline "==" | ...) '\n' }}} */
    def title(): Block = {
      jumpWhitespace()
      val inLevel = repeatJump("=")
      val text = inline(check(Array.fill(inLevel)('=')))
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

    def inline(isInlineEnd: => Boolean): Inline = {

      def inline0(): Inline = {
        if (char == safeTagMarker) htmlTag()
        else if (check("'''")) bold()
        else if (check("''")) italic()
        else if (check("`"))  monospace()
        else if (check("__")) underline()
        else if (check("^"))  superscript()
        else if (check(",,")) subscript()
        else if (check("[[")) link()
        else {
          readUntil { char == safeTagMarker || check("''") || char == '`' || check("__") || char == '^' || check(",,") || check("[[") || isInlineEnd || checkParaEnded || char == endOfLine }
          Text(getRead())
        }
      }

      val inlines: List[Inline] = {
        val iss = mutable.ListBuffer.empty[Inline]
        iss += inline0()
        while(!isInlineEnd && !checkParaEnded) {
          if (char == endOfLine) nextChar()
          val current = inline0()
          (iss.last, current) match {
            case (Text(t1), Text(t2)) =>
              iss.update(iss.length - 1, Text(t1 + endOfLine + t2))
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

    def htmlTag(): Inline = {
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
      readUntil { char == '`' }
      jump("`")
      Monospace(getRead())
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
      jump("^")
      Superscript(i)
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

    def entityLink(query: String): Inline = findTemplate(query) match {
      case Some(tpl) =>
        EntityLink(tpl)
      case None =>
        Text(query)
    }

    def link(): Inline = {
      val SchemeUri = new Regex("""([^:]+:.*)""")
      jump("[[")
      readUntil { check("]]") || check(" ") }
      val target = getRead()
      val title =
        if (!check("]]")) Some({
          jump(" ")
          inline(check("]]"))
        })
        else None
      jump("]]")
      (target, title) match {
        case (SchemeUri(uri), Some(title)) =>
          Link(uri, title)
        case (SchemeUri(uri), None) =>
          Link(uri, Text(uri))
        case (qualName, None) =>
          entityLink(qualName)
        case (qualName, Some(text)) =>
          reportError(pos, "entity link to " + qualName + " cannot have a custom title'" + text + "'")
          entityLink(qualName)
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

    def checkParaEnded(): Boolean = {
      (char == endOfText) ||
      ((char == endOfLine) && {
        val poff = offset
        val pc = char
        nextChar() // read EOL
        val ok = {
          checkSkipInitWhitespace(Array(endOfLine)) ||
          checkSkipInitWhitespace(Array('=')) ||
          checkSkipInitWhitespace(Array('{', '{', '{')) ||
          checkList ||
          checkSkipInitWhitespace(Array('\u003D'))
        }
        offset = poff
        char = pc
        ok
      })
    }

    def reportError(pos: Position, message: String): Unit =
      reporter.warning(pos, message)

  }

  protected sealed class CharReader(buffer: Array[Char]) { reader =>

    var char: Char = _
    var offset: Int = 0

    final def nextChar(): Unit = {
      if (offset >= buffer.length)
        char = endOfText
      else {
        char = buffer(offset)
        offset += 1
      }
    }

    implicit def strintToChars(s: String): Array[Char] = s.toArray

    def store(body: => Unit): String = {
      val pre = offset
      body
      val post = offset
      buffer.toArray.slice(pre, post).toString
    }

    final def check(chars: Array[Char]): Boolean = {
      val poff = offset
      val pc = char
      val ok = jump(chars)
      offset = poff
      char = pc
      ok
    }

    def checkSkipInitWhitespace(chars: Array[Char]): Boolean = {
      val poff = offset
      val pc = char
      jumpWhitespace()
      val (ok0, chars0) =
        if (chars.head == ' ')
          (offset > poff, chars.tail)
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
    final def jump(chars: Array[Char]): Boolean = {
      var index = 0
      while (index < chars.length && char == chars(index) && char != endOfText) {
        nextChar()
        index += 1
      }
      index == chars.length
    }

    final def checkedJump(chars: Array[Char]): Boolean = {
      val poff = offset
      val pc = char
      val ok = jump(chars)
      if (!ok) {
        offset = poff
        char = pc
      }
      ok
    }

    final def repeatJump(chars: Array[Char], max: Int): Int = {
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

    final def repeatJump(chars: Array[Char]): Int = {
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
      while(char != ch && char != endOfText) {
        nextChar()
        count=count+1
      }
      count
    }

    final def jumpUntil(chars: Array[Char]): Int = {
      assert(chars.length > 0)
      var count = 0
      val c = chars(0)
      while(!check(chars) && char != endOfText) {
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
      while(char != ch && char != endOfText) {
        readBuilder += char
        nextChar()
      }
      count
    }

    final def readUntil(chars: Array[Char]): Int = {
      assert(chars.length > 0)
      var count = 0
      val c = chars(0)
      while(!check(chars) && char != endOfText) {
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

    def isWhitespace(c: Char) = (c: @switch) match {
      case ' ' | '\t' => true
      case _ => false
    }

  }

}
