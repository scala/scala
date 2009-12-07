/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: Unapplies.scala 19206 2009-10-21 20:57:27Z extempore $

package scala.tools.nsc
package ast

import symtab._
import util.{Position, NoPosition}
import scala.collection.mutable.{HashMap, ListBuffer, StringBuilder}

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait DocComments { self: SymbolTable =>

  val docComments = new HashMap[Symbol, DocComment] // !!! todo: inherit from comment?

  private val defs = new HashMap[Symbol, Map[String, String]] {
    override def default(key: Symbol) = Map()
  }

  def getDocComment(sym: Symbol): Option[DocComment] = {
    docComments get sym match {
      case None =>
        mapFind(sym.allOverriddenSymbols)(docComments.get)
      case someSym =>
        someSym
    }
  }

  private def mapFind[A, B](xs: Iterable[A])(f: A => Option[B]): Option[B] = {
    var res: Option[B] = None
    val it = xs.iterator
    while (res.isEmpty && it.hasNext) {
      res = f(it.next())
    }
    res
  }

  /** The raw doc comment of symbol `sym`, as it appears in the source text, "" if missing.
   */
  def rawDocComment(sym: Symbol): String = getDocComment(sym) map (_.raw) getOrElse ""

  /** The processed doc comment of symbol `sym`, where
   *  Wiki syntax is expanded and @usecase or @define parts are removed.
   *  No variables are expanded yet. "" if missing.
   */
  def templateDocComment(sym: Symbol): String = getDocComment(sym) map (_.template) getOrElse ""

  /** The cooked doc comment of symbol `sym` after variable expansion, or "" if missing.
   *  @param sym  The symbol for which doc comment is returned
   *  @param site The class for which doc comments are generated
   */
  def cookedDocComment(sym: Symbol, site: Symbol): String =
    getDocComment(sym) map (_.expanded(site)) getOrElse ""

  /** The cooked doc comment of symbol `sym` after variable expansion, or "" if missing.
   *  @param sym  The symbol for which doc comment is returned (site is always the containing class)
   */
  def cookedDocComment(sym: Symbol): String = cookedDocComment(sym, sym.owner)

  /** The position of the doc comment of symbol `sym`, or NoPosition if missing */
  def docCommentPos(sym: Symbol): Position = getDocComment(sym) map (_.pos) getOrElse NoPosition

  def useCases(sym: Symbol, site: Symbol): List[(Symbol, String)] = {
    def getUseCases(dc: DocComment) = {
      for (uc <- dc.useCases; defn <- uc.expandedDefs(site)) yield
        (defn, uc.comment.expanded(site))
    }
    getDocComment(sym) map getUseCases getOrElse List()
  }

  def useCases(sym: Symbol): List[(Symbol, String)] = useCases(sym, sym.owner)

  private val usecasePrefix = "@usecase "
  private val definePrefix  = "@define "

  /** Returns index of string `str` after `in` skipping longest
   *  sequence of space and tab characters.
   */
  private def skipWhitespace(str: String, in: Int): Int = {
    var idx = in
    do {
      idx += 1
    } while (idx < str.length && (str charAt idx) == ' ' || (str charAt idx) == '\t')
    idx
  }

  /** Returns index of string `str` after `in` skipping longest
   *  sequence of space and tab characters, possibly also containing
   *  a single `*' character.
   */
  private def skipLineLead(str: String, in: Int, end: Int): Int = {
    val idx = skipWhitespace(str, in)
    if (idx < end && (str charAt idx) == '*') skipWhitespace(str, idx)
    else idx
  }

  /** Extracts variable name from a string, stripping any pair of surrounding braces */
  private def variableName(str: String): String =
    if (str.length >= 2 && (str charAt 0) == '{' && (str charAt (str.length - 1)) == '}')
      str.substring(1, str.length - 1)
    else
      str

  private def isVarPart(ch: Char) =
    '0' <= ch && ch <= '9' || 'A' <= ch && ch <= 'Z' || 'a' <= ch && ch <= 'z'

  /** Returns index following variable, or start index if no variable was recognized
   */
  private def skipVariable(str: String, start: Int): Int = {
    var idx = start
    if (idx < str.length && (str charAt idx) == '{') {
      do idx += 1
      while (idx < str.length && (str charAt idx) != '}')
      if (idx < str.length) idx + 1 else start
    } else {
      while (idx < str.length && isVarPart(str charAt idx))
        idx += 1
      idx
    }
  }

  private val wikiReplacements = List(
    ("""(\n\s*\*?)(\s*\n)"""    .r, """$1 <p>$2"""),
    ("""\{\{\{(.*(?:\n.*)*)\}\}\}""".r, """<pre>$1</pre>"""),
    ("""`([^`]*)`"""            .r, """<code>$1</code>"""),
    ("""__([^_]*)__"""          .r, """<u>$1</u>"""),
    ("""''([^']*)''"""          .r, """<i>$1</i>"""),
    ("""'''([^']*)'''"""        .r, """<b>$1</b>"""),
    ("""\^([^^]*)\^"""          .r, """<sup>$1</sup>"""),
    (""",,([^,]*),,"""          .r, """<sub>$1</sub>"""))
  private def expandWiki(str: String): String =
    (str /: wikiReplacements) { (str1, regexRepl) => regexRepl._1 replaceAllIn(str1, regexRepl._2) }

  /** Lookup definition of variable.
   *
   *  @param vble  The variable for which a definition is searched
   *  @param owner The current owner in which variable definitions are searched.
   *  @param site  The class for which doc comments are generated
   */
  def lookupVariable(vble: String, site: Symbol): Option[String] =
    if (site == NoSymbol)
      None
    else
      mapFind(site.info.baseClasses)(defs(_).get(vble)) match {
        case None => lookupVariable(vble, site.owner)
        case someStr => someStr
      }

  private var expandCount = 0
  private final val expandLimit = 10

  /** Expand variable occurrences in string `str', until a fix point is reached or
   *  a expandLimit is exceeded.
   *
   *  @param str   The string to be expanded
   *  @param site  The class for which doc comments are generated
   *  @return      Expanded string
   */
  private def expandVariables(str: String, site: Symbol): String =
    if (expandCount < expandLimit) {
      try {
        val out = new StringBuilder
        var start = 0
        var idx = 0
        while (idx < str.length) {
          if ((str charAt idx) == '$') {
            val vstart = idx
            idx = skipVariable(str, idx + 1)
            val vname = variableName(str.substring(vstart + 1, idx))
            if (vname.length > 0) {
              lookupVariable(vname, site) match {
                case Some(replacement) =>
                  out append str.substring(start, vstart)
                  out append replacement
                  start = idx
                case None =>
                  println("no replacement for "+vname) // !!!
              }
            } else {
              idx += 1
            }
          } else {
            idx += 1
          }
        }
        if (out.length == 0) str
        else {
          out append str.substring(start)
          expandVariables(out.toString, site)
        }
      } finally {
        expandCount -= 1
      }
    } else throw new ExpansionLimitExceeded(str)

  case class DocComment(raw: String, pos: Position = NoPosition) {

    lazy val (template, defines, useCases) = {
      val parts = decompose(raw)
      val (defines, usecases) = parts.tail partition (_._1 startsWith definePrefix)
      val templ = expandWiki(parts.head._1)
      (templ,
       defines map (d => expandWiki(d._1)),
       usecases map (decomposeUseCase(_, templ)))
    }

    def expanded(site: Symbol): String =
      expandVariables(template, site)
/*
    expansions get site match {
      case Some(str) => str
      case None => val str =
        expandVariables(template, sym, site)
        expansions += (site -> str)
        str
    }
    private var expansions: Map[Symbol, String] = Map()
*/


    /** Decomposes a comment string into
     *  an initial comment and a list of @define and @usecase clauses, each with starting index
     */
    private def decompose(str: String): List[(String, Int)] = {
      val out = new ListBuffer[(String, Int)]
      var segstart = 0
      var idx = 3  // skip initial "/**"
      val end = str.length - 2  // stop before final "*/"
      var eolIdx = idx
      while (idx < end) {
        if ((str charAt idx) == '\n') {
          eolIdx = idx
          idx = skipLineLead(str, idx, end)
          if ((str charAt idx) == '@' &&
              (str.startsWith(definePrefix, idx) || str.startsWith(usecasePrefix, idx))) {
            var segment = str.substring(segstart, eolIdx)
            if (segstart == 0) segment += "*/"
            out += ((segment, segstart))
            segstart = idx
          }
        } else idx += 1
      }
      if (segstart == 0)
        List((str, 0))
      else {
        out += ((str.substring(segstart, eolIdx), segstart))
        out.toList
      }
    }

    def subPos(start: Int, end: Int) =
      if (pos == NoPosition) NoPosition
      else {
        val start1 = pos.start + start
        val end1 = pos.end + end
        pos withStart start1 withPoint start1 withEnd end1
      }

    def decomposeUseCase(stroff: (String, Int), mainComment: String): UseCase = {
      val str = stroff._1
      val offset = stroff._2
      val start = usecasePrefix.length
      var idx = start
      while (idx < str.length && (str charAt idx) != '\n') idx += 1
      val code = str.substring(start, idx)
      val codePos = subPos(offset + usecasePrefix.length, offset + idx)
      var firstParBreak = mainComment indexOf "<p>"
      if (firstParBreak == -1) firstParBreak = mainComment.length - 2
      val comment = mainComment.substring(0, firstParBreak)+"<p>"+
        str.substring(skipLineLead(str, idx, str.length))+"*/"
      val commentPos = subPos(offset + idx, offset + str.length)
      UseCase(DocComment(comment, commentPos), code, codePos)
    }

    def defineVariables(sym: Symbol) {
      for (str <- defines) {
        val start = definePrefix.length
        var idx = skipVariable(str, start)
        val vble = variableName(str.substring(start, idx))
        if (idx < str.length && (str charAt idx) == ' ') idx += 1
        defs(sym) += vble -> str.substring(idx)
      }
      if (defs(sym).nonEmpty) println("vars of "+sym+" = "+defs(sym))  // !!!
    }
  }

  case class UseCase(comment: DocComment, body: String, pos: Position) {
    var defined: List[Symbol] = List()
    var aliases: List[Symbol] = List()

    def expandedDefs(site: Symbol): List[Symbol] = {

      def select(site: Type, name: Name, orElse: => Type): Type = {
        val member = site.nonPrivateMember(name)
        if (member.isTerm) SingleType(site, member)
        else if (member.isType) site.memberType(member)
        else orElse
      }

      def getSite(name: Name): Type = {
        if (name == nme.this_) site.thisType
        else {
          def findIn(sites: List[Symbol]): Type = sites match {
            case List() => NoType
            case site :: sites1 => select(site.thisType, name, findIn(sites1))
          }
          val (classes, pkgs) = site.ownerChain.span(!_.isPackageClass)
          findIn(classes ::: List(pkgs.head, definitions.RootClass))
        }
      }

      def getType(str: String): Type = {
        val parts = str.split("""\.""").toList
        val partnames = (parts.init map newTermName) ::: List(newTypeName(parts.last))
        (getSite(partnames.head) /: partnames.tail)(select(_, _, NoType))
      }

      val aliasExpansions: List[Type] =
        for (alias <- aliases) yield
          lookupVariable(alias.name.toString.substring(1), site) match {
            case Some(repl) =>
              val tpe = getType(repl)
              if (tpe != NoType) tpe
              else {
                val alias1 = alias.cloneSymbol(definitions.RootClass)
                alias1.name = repl.toTypeName
                TypeRef(NoPrefix, alias1, List())
              }
            case None =>
              TypeRef(NoPrefix, alias, List())
          }

      def subst(sym: Symbol, from: List[Symbol], to: List[Type]): Type =
        if (from.isEmpty) sym.tpe
        else if (from.head == sym) to.head
        else subst(sym, from.tail, to.tail)

      val substAliases = new TypeMap {
        def apply(tp: Type) = mapOver(tp) match {
          case tp1 @ TypeRef(pre, sym, args) if (sym.name.length > 1 && sym.name(0) == '$') =>
            subst(sym, aliases, aliasExpansions) match {
              case TypeRef(pre, sym1, _) =>
                TypeRef(pre, sym1, args)
              case _ =>
                tp1
            }
          case tp1 =>
            tp1
        }
      }

      for (defn <- defined) yield
        defn.cloneSymbol(site).setInfo(
          substAliases(defn.info).asSeenFrom(site.thisType, defn.owner))
    }
  }

  class ExpansionLimitExceeded(str: String) extends Exception
}
