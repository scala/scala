package scala.tools.nsc
package doc
package model

import comment._

import scala.reflect.internal.util.FakePos //Position

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup {
  thisFactory: ModelFactory =>

  import global._
  import rootMirror.RootPackage, rootMirror.EmptyPackage

  def makeEntityLink(title: Inline, pos: Position, query: String, inTplOpt: Option[DocTemplateImpl]) =
    new EntityLink(title) { lazy val link = memberLookup(pos, query, inTplOpt) }

  def memberLookup(pos: Position, query: String, inTplOpt: Option[DocTemplateImpl]): LinkTo = {
    assert(modelFinished)

    var members = breakMembers(query)
    //println(query + " => " + members)

    // (1) First look in the root package, as most of the links are qualified
    val fromRoot = lookupInRootPackage(pos, members)

    // (2) Or recursively go into each containing template.
    val fromParents = inTplOpt.fold(Stream.empty[DocTemplateImpl]) { tpl =>
      Stream.iterate(tpl)(_.inTemplate)
    }.takeWhile (tpl => tpl != null && !tpl.isRootPackage).map { tpl =>
      lookupInTemplate(pos, members, tpl.asInstanceOf[EntityImpl].sym)
    }

    val syms = (fromRoot +: fromParents) find (!_.isEmpty) getOrElse Nil
    val linkTo = createLinks(syms) match {
      case Nil if !syms.isEmpty =>
        // (3) Look at external links
        syms.flatMap { case (sym, owner) =>

          // reconstruct the original link
          def linkName(sym: Symbol) = {
            def isRoot(s: Symbol) = s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass
            def nameString(s: Symbol) = s.nameString + (if ((s.isModule || s.isModuleClass) && !s.isPackage) "$" else "")
            val packageSuffix = if (sym.isPackage) ".package" else ""

            sym.ownerChain.reverse.filterNot(isRoot(_)).map(nameString(_)).mkString(".") + packageSuffix
          }

          if (sym.isClass || sym.isModule || sym.isTrait || sym.isPackage)
            findExternalLink(sym, linkName(sym))
          else if (owner.isClass || owner.isModule || owner.isTrait || owner.isPackage)
            findExternalLink(sym, linkName(owner) + "@" + externalSignature(sym))
          else
            None
        }
      case links => links
    }

    //println(createLinks(syms))
    //println(linkTo)

    // (4) if we still haven't found anything, create a tooltip, if we found too many, report
    if (linkTo.isEmpty){
      if (!settings.docNoLinkWarnings.value)
        reporter.warning(pos, "Could not find any member to link for \"" + query + "\".")
      Tooltip(query)
    } else {
      if (linkTo.length > 1) {

        val chosen =
          if (linkTo.exists(_.isInstanceOf[LinkToMember]))
            linkTo.collect({case lm: LinkToMember => lm}).min(Ordering[MemberEntity].on[LinkToMember](_.mbr))
          else
            linkTo.head

        def linkToString(link: LinkTo) = {
          val description =
            link match {
              case lm@LinkToMember(mbr, inTpl) => " * " + mbr.kind + " \"" + mbr.signature + "\" in " + inTpl.kind + " " + inTpl.qualifiedName
              case lt@LinkToTpl(tpl) => " * " + tpl.kind + " \"" + tpl.qualifiedName + "\""
              case other => " * " + other.toString
            }
          val chosenInfo =
            if (link == chosen)
              " [chosen]"
            else
              ""
          description + chosenInfo + "\n"
        }
        if (!settings.docNoLinkWarnings.value)
          reporter.warning(pos,
            "The link target \"" + query + "\" is ambiguous. Several (possibly overloaded) members fit the target:\n" +
            linkTo.map(link => linkToString(link)).mkString +
            (if (MemberLookup.showExplanation)
              "\n\n" +
              "Quick crash course on using Scaladoc links\n" +
              "==========================================\n" +
              "Disambiguating terms and types: Prefix terms with '$' and types with '!' in case both names are in use:\n" +
              " - [[scala.collection.immutable.List!.apply class List's apply method]] and\n" +
              " - [[scala.collection.immutable.List$.apply object List's apply method]]\n" +
              "Disambiguating overloaded members: If a term is overloaded, you can indicate the first part of its signature followed by *:\n" +
              " - [[[scala.collection.immutable.List$.fill[A](Int)(⇒A):List[A]* Fill with a single parameter]]]\n" +
              " - [[[scala.collection.immutable.List$.fill[A](Int,Int)(⇒A):List[List[A]]* Fill with a two parameters]]]\n" +
              "Notes: \n" +
              " - you can use any number of matching square brackets to avoid interference with the signature\n" +
              " - you can use \\. to escape dots in prefixes (don't forget to use * at the end to match the signature!)\n" +
              " - you can use \\# to escape hashes, otherwise they will be considered as delimiters, like dots.\n"
            else "")
        )
        chosen
      } else
        linkTo.head
    }
  }

  private abstract class SearchStrategy
  private object BothTypeAndTerm extends SearchStrategy
  private object OnlyType extends SearchStrategy
  private object OnlyTerm extends SearchStrategy

  private def lookupInRootPackage(pos: Position, members: List[String]) =
    lookupInTemplate(pos, members, EmptyPackage) ::: lookupInTemplate(pos, members, RootPackage)

  private def createLinks(syms: List[(Symbol, Symbol)]): List[LinkTo] =
    syms.flatMap { case (sym, owner) =>
      findTemplateMaybe(sym) match {
        case Some(tpl) => LinkToTpl(tpl) :: Nil
        case None =>
          findTemplateMaybe(owner) flatMap { inTpl =>
            inTpl.members find (_.asInstanceOf[EntityImpl].sym == sym) map (LinkToMember(_, inTpl))
          }
      }
    }

  private def lookupInTemplate(pos: Position, members: List[String], container: Symbol): List[(Symbol, Symbol)] = {
    // Maintaining compatibility with previous links is a bit tricky here:
    // we have a preference for term names for all terms except for the last, where we prefer a class:
    // How to do this:
    //  - at each step we do a DFS search with the prefered strategy
    //  - if the search doesn't return any members, we backtrack on the last decision
    //     * we look for terms with the last member's name
    //     * we look for types with the same name, all the way up
    val result = members match {
      case Nil => Nil
      case mbrName::Nil =>
        var syms = lookupInTemplate(pos, mbrName, container, OnlyType) map ((_, container))
        if (syms.isEmpty)
          syms = lookupInTemplate(pos, mbrName, container, OnlyTerm) map ((_, container))
        syms

      case tplName::rest =>
        def completeSearch(syms: List[Symbol]) =
          syms flatMap (lookupInTemplate(pos, rest, _))

        completeSearch(lookupInTemplate(pos, tplName, container, OnlyTerm)) match {
          case Nil => completeSearch(lookupInTemplate(pos, tplName, container, OnlyType))
          case syms => syms
      }
    }
    //println("lookupInTemplate(" + members + ", " + container + ") => " + result)
    result
  }

  private def lookupInTemplate(pos: Position, member: String, container: Symbol, strategy: SearchStrategy): List[Symbol] = {
    val name = member.stripSuffix("$").stripSuffix("!").stripSuffix("*")
    def signatureMatch(sym: Symbol): Boolean = externalSignature(sym).startsWith(name)

    // We need to cleanup the bogus classes created by the .class file parser. For example, [[scala.Predef]] resolves
    // to (bogus) class scala.Predef loaded by the class loader -- which we need to eliminate by looking at the info
    // and removing NoType classes
    def cleanupBogusClasses(syms: List[Symbol]) = { syms.filter(_.info != NoType) }

    def syms(name: Name) = container.info.nonPrivateMember(name.encodedName).alternatives
    def termSyms = cleanupBogusClasses(syms(newTermName(name)))
    def typeSyms = cleanupBogusClasses(syms(newTypeName(name)))

    val result = if (member.endsWith("$"))
      termSyms
    else if (member.endsWith("!"))
      typeSyms
    else if (member.endsWith("*"))
      cleanupBogusClasses(container.info.nonPrivateDecls) filter signatureMatch
    else
      if (strategy == BothTypeAndTerm)
        termSyms ::: typeSyms
      else if (strategy == OnlyType)
        typeSyms
      else if (strategy == OnlyTerm)
        termSyms
      else
        Nil

    //println("lookupInTemplate(" + member + ", " + container + ") => " + result)
    result
  }

  private def breakMembers(query: String): List[String] = {
    // Okay, how does this work? Well: you split on . but you don't want to split on \. => thus the ugly regex
    // query.split((?<=[^\\\\])\\.).map(_.replaceAll("\\."))
    // The same code, just faster:
    var members = List[String]()
    var index = 0
    var last_index = 0
    val length = query.length
    while (index < length) {
      if ((query.charAt(index) == '.' || query.charAt(index) == '#') &&
          ((index == 0) || (query.charAt(index-1) != '\\'))) {

        val member = query.substring(last_index, index).replaceAll("\\\\([#\\.])", "$1")
        // we want to allow javadoc-style links [[#member]] -- which requires us to remove empty members from the first
        // elemnt in the list
        if ((member != "") || (!members.isEmpty))
          members ::= member
        last_index = index + 1
      }
      index += 1
    }
    if (last_index < length)
      members ::= query.substring(last_index, length).replaceAll("\\\\\\.", ".")
    members.reverse
  }
}

object MemberLookup {
  private[this] var _showExplanation = true
  def showExplanation: Boolean = if (_showExplanation) { _showExplanation = false; true } else false
}
