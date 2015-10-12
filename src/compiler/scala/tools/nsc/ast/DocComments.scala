/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import symtab._
import util.DocStrings._
import scala.collection.mutable

/*
 *  @author  Martin Odersky
 *  @version 1.0
 */
trait DocComments { self: Global =>

  val cookedDocComments = mutable.HashMap[Symbol, String]()

  /** The raw doc comment map
   *
   * In IDE, background compilation runs get interrupted by
   * reloading new sourcefiles. This is weak to avoid
   * memleaks due to the doc of their cached symbols
   * (e.g. in baseTypeSeq) between periodic doc reloads.
   */
  val docComments = mutable.WeakHashMap[Symbol, DocComment]()

  def clearDocComments() {
    cookedDocComments.clear()
    docComments.clear()
    defs.clear()
  }

  /** The raw doc comment of symbol `sym`, as it appears in the source text, "" if missing.
   */
  def rawDocComment(sym: Symbol): String =
    docComments get sym map (_.raw) getOrElse ""

  /** The position of the raw doc comment of symbol `sym`, or NoPosition if missing
   *  If a symbol does not have a doc comment but some overridden version of it does,
   *  the position of the doc comment of the overridden version is returned instead.
   */
  def docCommentPos(sym: Symbol): Position =
    getDocComment(sym) map (_.pos) getOrElse NoPosition

  /** A version which doesn't consider self types, as a temporary measure:
   *  an infinite loop has broken out between superComment and cookedDocComment
   *  since r23926.
   */
  private def allInheritedOverriddenSymbols(sym: Symbol): List[Symbol] = {
    if (!sym.owner.isClass) Nil
    else sym.owner.ancestors map (sym overriddenSymbol _) filter (_ != NoSymbol)
  }

  def fillDocComment(sym: Symbol, comment: DocComment) {
    docComments(sym) = comment
    comment.defineVariables(sym)
  }


  def replaceInheritDocToInheritdoc(docStr: String):String  = {
    docStr.replaceAll("""\{@inheritDoc\p{Zs}*\}""", "@inheritdoc")
  }

  /** The raw doc comment of symbol `sym`, minus usecase and define sections, augmented by
   *  missing sections of an inherited doc comment.
   *  If a symbol does not have a doc comment but some overridden version of it does,
   *  the doc comment of the overridden version is copied instead.
   */
  def cookedDocComment(sym: Symbol, docStr: String = ""): String = cookedDocComments.getOrElseUpdate(sym, {
    var ownComment = if (docStr.length == 0) docComments get sym map (_.template) getOrElse ""
                       else DocComment(docStr).template
    ownComment = replaceInheritDocToInheritdoc(ownComment)

    superComment(sym) match {
      case None =>
        // SI-8210 - The warning would be false negative when this symbol is a setter
        if (ownComment.indexOf("@inheritdoc") != -1 && ! sym.isSetter)
          reporter.warning(sym.pos, s"The comment for ${sym} contains @inheritdoc, but no parent comment is available to inherit from.")
        ownComment.replaceAllLiterally("@inheritdoc", "<invalid inheritdoc annotation>")
      case Some(sc) =>
        if (ownComment == "") sc
        else expandInheritdoc(sc, merge(sc, ownComment, sym), sym)
    }
  })

  /** The cooked doc comment of symbol `sym` after variable expansion, or "" if missing.
   *
   *  @param sym  The symbol for which doc comment is returned
   *  @param site The class for which doc comments are generated
   *  @throws ExpansionLimitExceeded  when more than 10 successive expansions
   *                                  of the same string are done, which is
   *                                  interpreted as a recursive variable definition.
   */
  def expandedDocComment(sym: Symbol, site: Symbol, docStr: String = ""): String = {
    // when parsing a top level class or module, use the (module-)class itself to look up variable definitions
    val site1 = if ((sym.isModule || sym.isClass) && site.hasPackageFlag) sym
                else site
    expandVariables(cookedDocComment(sym, docStr), sym, site1)
  }

  /** The list of use cases of doc comment of symbol `sym` seen as a member of class
   *  `site`. Each use case consists of a synthetic symbol (which is entered nowhere else),
   *  of an expanded doc comment string, and of its position.
   *
   *  @param sym  The symbol for which use cases are returned
   *  @param site The class for which doc comments are generated
   *  @throws ExpansionLimitExceeded  when more than 10 successive expansions
   *                                  of the same string are done, which is
   *                                  interpreted as a recursive variable definition.
   */
  def useCases(sym: Symbol, site: Symbol): List[(Symbol, String, Position)] = {
    def getUseCases(dc: DocComment) = {
      val fullSigComment = cookedDocComment(sym)
      for (uc <- dc.useCases; defn <- uc.expandedDefs(sym, site)) yield {
        // use cases comments go through a series of transformations:
        // 1 - filling in missing sections from the full signature
        // 2 - expanding explicit inheritance @inheritdoc tags
        // 3 - expanding variables like $COLL
        val useCaseCommentRaw        = uc.comment.raw
        val useCaseCommentMerged     = merge(fullSigComment, useCaseCommentRaw, defn)
        val useCaseCommentInheritdoc = expandInheritdoc(fullSigComment, useCaseCommentMerged, sym)
        val useCaseCommentVariables  = expandVariables(useCaseCommentInheritdoc, sym, site)
        (defn, useCaseCommentVariables, uc.pos)
      }
    }
    getDocComment(sym) map getUseCases getOrElse List()
  }

  private val wikiReplacements = List(
    ("""(\n\s*\*?)(\s*\n)"""    .r, """$1 <p>$2"""),
    ("""<([^\w/])"""            .r, """&lt;$1"""),
    ("""([^\w/])>"""            .r, """$1&gt;"""),
    ("""\{\{\{(.*(?:\n.*)*)\}\}\}""".r, """<pre>$1</pre>"""),
    ("""`([^`]*)`"""            .r, """<code>$1</code>"""),
    ("""__([^_]*)__"""          .r, """<u>$1</u>"""),
    ("""''([^']*)''"""          .r, """<i>$1</i>"""),
    ("""'''([^']*)'''"""        .r, """<b>$1</b>"""),
    ("""\^([^^]*)\^"""          .r, """<sup>$1</sup>"""),
    (""",,([^,]*),,"""          .r, """<sub>$1</sub>"""))

  /** Returns just the wiki expansion (this would correspond to
   *  a comment in the input format of the JavaDoc tool, modulo differences
   *  in tags.)
   */
  def expandWiki(str: String): String =
    (str /: wikiReplacements) { (str1, regexRepl) => regexRepl._1 replaceAllIn(str1, regexRepl._2) }

  private def getDocComment(sym: Symbol): Option[DocComment] =
    mapFind(sym :: allInheritedOverriddenSymbols(sym))(docComments get _)

  /** The cooked doc comment of an overridden symbol */
  protected def superComment(sym: Symbol): Option[String] =
    allInheritedOverriddenSymbols(sym).iterator map (x => cookedDocComment(x)) find (_ != "")

  private def mapFind[A, B](xs: Iterable[A])(f: A => Option[B]): Option[B] =
    xs collectFirst scala.Function.unlift(f)

  private def isMovable(str: String, sec: (Int, Int)): Boolean =
    startsWithTag(str, sec, "@param") ||
    startsWithTag(str, sec, "@tparam") ||
    startsWithTag(str, sec, "@return")

  /** Merge elements of doccomment `src` into doc comment `dst` for symbol `sym`.
   *  In detail:
   *  1. If `copyFirstPara` is true, copy first paragraph
   *  2. For all parameters of `sym` if there is no @param section
   *     in `dst` for that parameter name, but there is one on `src`, copy that section.
   *  3. If there is no @return section in `dst` but there is one in `src`, copy it.
   */
  def merge(src: String, dst: String, sym: Symbol, copyFirstPara: Boolean = false): String = {
    val srcSections  = tagIndex(src)
    val dstSections  = tagIndex(dst)
    val srcParams    = paramDocs(src, "@param", srcSections)
    val dstParams    = paramDocs(dst, "@param", dstSections)
    val srcTParams   = paramDocs(src, "@tparam", srcSections)
    val dstTParams   = paramDocs(dst, "@tparam", dstSections)
    val out          = new StringBuilder
    var copied       = 0
    var tocopy       = startTag(dst, dstSections dropWhile (!isMovable(dst, _)))

    if (copyFirstPara) {
      val eop = // end of comment body (first para), which is delimited by blank line, or tag, or end of comment
        (findNext(src, 0)(src.charAt(_) == '\n')) min startTag(src, srcSections)
      out append src.substring(0, eop).trim
      copied = 3
      tocopy = 3
    }

    def mergeSection(srcSec: Option[(Int, Int)], dstSec: Option[(Int, Int)]) = dstSec match {
      case Some((start, end)) =>
        if (end > tocopy) tocopy = end
      case None =>
        srcSec match {
          case Some((start1, end1)) => {
            out append dst.substring(copied, tocopy).trim
            out append "\n"
            copied = tocopy
            out append src.substring(start1, end1).trim
          }
          case None =>
        }
    }

    for (params <- sym.paramss; param <- params)
      mergeSection(srcParams get param.name.toString, dstParams get param.name.toString)
    for (tparam <- sym.typeParams)
      mergeSection(srcTParams get tparam.name.toString, dstTParams get tparam.name.toString)
    mergeSection(returnDoc(src, srcSections), returnDoc(dst, dstSections))
    mergeSection(groupDoc(src, srcSections), groupDoc(dst, dstSections))

    if (out.length == 0) dst
    else {
      out append dst.substring(copied)
      out.toString
    }
  }

  /**
   * Expand inheritdoc tags
   *  - for the main comment we transform the inheritdoc into the super variable,
   *  and the variable expansion can expand it further
   *  - for the param, tparam and throws sections we must replace comments on the spot
   *
   * This is done separately, for two reasons:
   * 1. It takes longer to run compared to merge
   * 2. The inheritdoc annotation should not be used very often, as building the comment from pieces severely
   * impacts performance
   *
   * @param parent The source (or parent) comment
   * @param child  The child (overriding member or usecase) comment
   * @param sym    The child symbol
   * @return       The child comment with the inheritdoc sections expanded
   */
  def expandInheritdoc(parent: String, child: String, sym: Symbol): String =
    if (child.indexOf("@inheritdoc") == -1)
      child
    else {
      val parentSections    = tagIndex(parent)
      val childSections     = tagIndex(child)
      val parentTagMap      = sectionTagMap(parent, parentSections)
      val parentNamedParams = Map() +
        ("@param"  -> paramDocs(parent, "@param", parentSections)) +
        ("@tparam" -> paramDocs(parent, "@tparam", parentSections)) +
        ("@throws" -> paramDocs(parent, "@throws", parentSections))

      val out         = new StringBuilder

      def replaceInheritdoc(childSection: String, parentSection: => String) =
        if (childSection.indexOf("@inheritdoc") == -1)
          childSection
        else
          childSection.replaceAllLiterally("@inheritdoc", parentSection)

      def getParentSection(section: (Int, Int)): String = {

        def getSectionHeader = extractSectionTag(child, section) match {
          case param@("@param"|"@tparam"|"@throws")  => param + " "  + extractSectionParam(child, section)
          case other     => other
        }

        def sectionString(param: String, paramMap: Map[String, (Int, Int)]): String =
          paramMap.get(param) match {
            case Some(section) =>
              // Cleanup the section tag and parameter
              val sectionTextBounds = extractSectionText(parent, section)
              cleanupSectionText(parent.substring(sectionTextBounds._1, sectionTextBounds._2))
            case None =>
              reporter.info(sym.pos, "The \"" + getSectionHeader + "\" annotation of the " + sym +
                  " comment contains @inheritdoc, but the corresponding section in the parent is not defined.", force = true)
              "<invalid inheritdoc annotation>"
          }

        child.substring(section._1, section._1 + 7) match {
          case param@("@param "|"@tparam"|"@throws") =>
            sectionString(extractSectionParam(child, section), parentNamedParams(param.trim))
          case _                                     =>
            sectionString(extractSectionTag(child, section), parentTagMap)
        }
      }

      def mainComment(str: String, sections: List[(Int, Int)]): String =
        if (str.trim.length > 3)
          str.trim.substring(3, startTag(str, sections))
        else
          ""

      // Append main comment
      out.append("/**")
      out.append(replaceInheritdoc(mainComment(child, childSections), mainComment(parent, parentSections)))

      // Append sections
      for (section <- childSections)
        out.append(replaceInheritdoc(child.substring(section._1, section._2), getParentSection(section)))

      out.append("*/")
      out.toString
    }

  /** Maps symbols to the variable -> replacement maps that are defined
   *  in their doc comments
   */
  private val defs = mutable.HashMap[Symbol, Map[String, String]]() withDefaultValue Map()

  /** Lookup definition of variable.
   *
   *  @param vble  The variable for which a definition is searched
   *  @param site  The class for which doc comments are generated
   */
  def lookupVariable(vble: String, site: Symbol): Option[String] = site match {
    case NoSymbol => None
    case _        =>
      val searchList =
        if (site.isModule) site :: site.info.baseClasses
        else site.info.baseClasses

      searchList collectFirst { case x if defs(x) contains vble => defs(x)(vble) } match {
        case Some(str) if str startsWith "$" => lookupVariable(str.tail, site)
        case res                             => res orElse lookupVariable(vble, site.owner)
      }
  }

  /** Expand variable occurrences in string `str`, until a fix point is reached or
   *  an expandLimit is exceeded.
   *
   *  @param initialStr   The string to be expanded
   *  @param sym          The symbol for which doc comments are generated
   *  @param site         The class for which doc comments are generated
   *  @return             Expanded string
   */
  protected def expandVariables(initialStr: String, sym: Symbol, site: Symbol): String = {
    val expandLimit = 10

    def expandInternal(str: String, depth: Int): String = {
      if (depth >= expandLimit)
        throw new ExpansionLimitExceeded(str)

      val out         = new StringBuilder
      var copied, idx = 0
      // excluding variables written as \$foo so we can use them when
      // necessary to document things like Symbol#decode
      def isEscaped = idx > 0 && str.charAt(idx - 1) == '\\'
      while (idx < str.length) {
        if ((str charAt idx) != '$' || isEscaped)
          idx += 1
        else {
          val vstart = idx
          idx = skipVariable(str, idx + 1)
          def replaceWith(repl: String) {
            out append str.substring(copied, vstart)
            out append repl
            copied = idx
          }
          variableName(str.substring(vstart + 1, idx)) match {
            case "super"    =>
              superComment(sym) foreach { sc =>
                val superSections = tagIndex(sc)
                replaceWith(sc.substring(3, startTag(sc, superSections)))
                for (sec @ (start, end) <- superSections)
                  if (!isMovable(sc, sec)) out append sc.substring(start, end)
              }
            case "" => idx += 1
            case vname  =>
              lookupVariable(vname, site) match {
                case Some(replacement) => replaceWith(replacement)
                case None              =>
                  val pos = docCommentPos(sym)
                  val loc = pos withPoint (pos.start + vstart + 1)
                  reporter.warning(loc, s"Variable $vname undefined in comment for $sym in $site")
              }
            }
        }
      }
      if (out.length == 0) str
      else {
        out append str.substring(copied)
        expandInternal(out.toString, depth + 1)
      }
    }

    // We suppressed expanding \$ throughout the recursion, and now we
    // need to replace \$ with $ so it looks as intended.
    expandInternal(initialStr, 0).replaceAllLiterally("""\$""", "$")
  }

  // !!! todo: inherit from Comment?
  case class DocComment(raw: String, pos: Position = NoPosition, codePos: Position = NoPosition) {

    /** Returns:
     *   template: the doc comment minus all @define and @usecase sections
     *   defines : all define sections (as strings)
     *   useCases: all usecase sections (as instances of class UseCase)
     */
    lazy val (template, defines, useCases) = {
      val sections = tagIndex(raw)

      val defines = sections filter { startsWithTag(raw, _, "@define") }
      val usecases = sections filter { startsWithTag(raw, _, "@usecase") }

      val end = startTag(raw, (defines ::: usecases).sortBy(_._1))

      (if (end == raw.length - 2) raw else raw.substring(0, end) + "*/",
       defines map { case (start, end) => raw.substring(start, end) },
       usecases map { case (start, end) => decomposeUseCase(start, end) })
    }

    private def decomposeUseCase(start: Int, end: Int): UseCase = {
      val codeStart    = skipWhitespace(raw, start + "@usecase".length)
      val codeEnd      = skipToEol(raw, codeStart)
      val code         = raw.substring(codeStart, codeEnd)
      val codePos      = subPos(codeStart, codeEnd)
      val commentStart = skipLineLead(raw, codeEnd + 1) min end
      val comment      = "/** " + raw.substring(commentStart, end) + "*/"
      val commentPos   = subPos(commentStart, end)

      UseCase(DocComment(comment, commentPos, codePos), code, codePos)
    }

    private def subPos(start: Int, end: Int) =
      if (pos == NoPosition) NoPosition
      else {
        val start1 = pos.start + start
        val end1 = pos.end + end
        pos withStart start1 withPoint start1 withEnd end1
      }

    def defineVariables(sym: Symbol) = {
      val Trim = "(?s)^[\\s&&[^\n\r]]*(.*?)\\s*$".r

      defs(sym) ++= defines.map {
        str => {
          val start = skipWhitespace(str, "@define".length)
          val (key, value) = str.splitAt(skipVariable(str, start))
          key.drop(start) -> value
        }
      } map {
        case (key, Trim(value)) =>
          variableName(key) -> value.replaceAll("\\s+\\*+$", "")
      }
    }
  }

  case class UseCase(comment: DocComment, body: String, pos: Position) {
    var defined: List[Symbol] = List() // initialized by Typer
    var aliases: List[Symbol] = List() // initialized by Typer

    def expandedDefs(sym: Symbol, site: Symbol): List[Symbol] = {

      def select(site: Type, name: Name, orElse: => Type): Type = {
        val member = site.nonPrivateMember(name)
        if (member.isTerm) singleType(site, member)
        else if (member.isType) site.memberType(member)
        else orElse
      }

      def getSite(name: Name): Type = {
        def findIn(sites: List[Symbol]): Type = sites match {
          case List() => NoType
          case site :: sites1 => select(site.thisType, name, findIn(sites1))
        }
        // Previously, searching was taking place *only* in the current package and in the root package
        // now we're looking for it everywhere in the hierarchy, so we'll be able to link variable expansions like
        // immutable.Seq in package immutable
        //val (classes, pkgs) = site.ownerChain.span(!_.isPackageClass)
        //val sites = (classes ::: List(pkgs.head, rootMirror.RootClass)))
        //findIn(sites)
        findIn(site.ownerChain ::: List(rootMirror.EmptyPackage))
      }

      def getType(str: String, variable: String): Type = {
        def getParts(start: Int): List[String] = {
          val end = skipIdent(str, start)
          if (end == start) List()
          else str.substring (start, end) :: {
            if (end < str.length && (str charAt end) == '.') getParts(end + 1)
            else List()
          }
        }
        val parts = getParts(0)
        if (parts.isEmpty) {
          reporter.error(comment.codePos, "Incorrect variable expansion for " + variable + " in use case. Does the " +
                                          "variable expand to wiki syntax when documenting " + site + "?")
          return ErrorType
        }
        val partnames = (parts.init map newTermName) :+ newTypeName(parts.last)
        val (start, rest) = parts match {
          case "this" :: _      => (site.thisType, partnames.tail)
          case _ :: "this" :: _ =>
            site.ownerChain.find(_.name == partnames.head) match {
              case Some(clazz)  => (clazz.thisType, partnames drop 2)
              case _            => (NoType, Nil)
            }
          case _ =>
            (getSite(partnames.head), partnames.tail)
        }
        val result = (start /: rest)(select(_, _, NoType))
        if (result == NoType)
          reporter.warning(comment.codePos, "Could not find the type " + variable + " points to while expanding it " +
                                            "for the usecase signature of " + sym + " in " + site + "." +
                                            "In this context, " + variable + " = \"" + str + "\".")
        result
      }

      /*
       * work around the backticks issue suggested by Simon in
       * https://groups.google.com/forum/?hl=en&fromgroups#!topic/scala-internals/z7s1CCRCz74
       * ideally, we'd have a removeWikiSyntax method in the CommentFactory to completely eliminate the wiki markup
       */
      def cleanupVariable(str: String) = {
        val tstr = str.trim
        if (tstr.length >= 2 && tstr.startsWith("`") && tstr.endsWith("`"))
          tstr.substring(1, tstr.length - 1)
        else
          tstr
      }

      // the Boolean tells us whether we can normalize: if we found an actual type, then yes, we can normalize, else no,
      // use the synthetic alias created for the variable
      val aliasExpansions: List[(Type, Boolean)] =
        for (alias <- aliases) yield
          lookupVariable(alias.name.toString.substring(1), site) match {
            case Some(repl) =>
              val repl2 = cleanupVariable(repl)
              val tpe = getType(repl2, alias.name.toString)
              if (tpe != NoType) (tpe, true)
              else {
                val alias1 = alias.cloneSymbol(rootMirror.RootClass, alias.rawflags, newTypeName(repl2))
                (typeRef(NoPrefix, alias1, Nil), false)
              }
            case None =>
              (typeRef(NoPrefix, alias, Nil), false)
          }

      def subst(sym: Symbol, from: List[Symbol], to: List[(Type, Boolean)]): (Type, Boolean) =
        if (from.isEmpty) (sym.tpe, false)
        else if (from.head == sym) to.head
        else subst(sym, from.tail, to.tail)

      val substAliases = new TypeMap {
        def apply(tp: Type) = mapOver(tp) match {
          case tp1 @ TypeRef(pre, sym, args) if (sym.name.length > 1 && sym.name.startChar == '$') =>
            subst(sym, aliases, aliasExpansions) match {
              case (TypeRef(pre1, sym1, _), canNormalize) =>
                val tpe = typeRef(pre1, sym1, args)
                if (canNormalize) tpe.normalize else tpe
              case _ =>
                tp1
            }
          case tp1 =>
            tp1
        }
      }

      for (defn <- defined) yield {
        defn.cloneSymbol(sym.owner, sym.flags | Flags.SYNTHETIC) modifyInfo (info =>
          substAliases(info).asSeenFrom(site.thisType, sym.owner)
        )
      }
    }
  }

  class ExpansionLimitExceeded(str: String) extends Exception
}
