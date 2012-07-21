package scala.tools.nsc
package doc
package model

import comment._

import scala.reflect.internal.util.FakePos //Position

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup {
  thisFactory: ModelFactory =>

  import global._

  def makeEntityLink(title: Inline, pos: Position, query: String, inTplOpt: Option[DocTemplateImpl]) =
    new EntityLink(title) { lazy val link = memberLookup(pos, query, inTplOpt) }

  def memberLookup(pos: Position, query: String, inTplOpt: Option[DocTemplateImpl]): LinkTo = {
    assert(modelFinished)

    var members = breakMembers(query)
    //println(query + " => " + members)

    // (1) Lookup in the root package, as most of the links are qualified
    var linkTo: List[LinkTo] = lookupInRootPackage(pos, members)

    // (2) Recursively go into each
    if (inTplOpt.isDefined) {
      var currentTpl = inTplOpt.get
      while (currentTpl != null && !currentTpl.isRootPackage && (linkTo.isEmpty)) {
        linkTo = lookupInTemplate(pos, members, currentTpl)
        currentTpl = currentTpl.inTemplate
      }
    }

    // (3) Look at external links
    if (linkTo.isEmpty) {
      // TODO: IF THIS IS THE ROOT PACKAGE, LOOK AT EXTERNAL LINKS
    }

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

  private def lookupInRootPackage(pos: Position, members: List[String]) = lookupInTemplate(pos, members, makeRootPackage)

  private def lookupInTemplate(pos: Position, members: List[String], inTpl: DocTemplateImpl): List[LinkTo] = {
    // Maintaining compatibility with previous links is a bit tricky here:
    // we have a preference for term names for all terms except for the last, where we prefer a class:
    // How to do this:
    //  - at each step we do a DFS search with the prefered strategy
    //  - if the search doesn't return any members, we backtrack on the last decision
    //     * we look for terms with the last member's name
    //     * we look for types with the same name, all the way up
    val result = members match {
      case Nil =>
        Nil
      case mbrName::Nil =>
        var members = lookupInTemplate(pos, mbrName, inTpl, OnlyType)
        if (members.isEmpty)
          members = lookupInTemplate(pos, mbrName, inTpl, OnlyTerm)

        members.map(_ match {
          case tpl: DocTemplateEntity => LinkToTpl(tpl)
          case mbr => LinkToMember(mbr, inTpl)
        })

      case tplName::rest =>

        def completeSearch(mbrs: List[MemberImpl]) =
          mbrs.collect({case d:DocTemplateImpl => d}).flatMap(tpl => lookupInTemplate(pos, rest, tpl))

        var members = completeSearch(lookupInTemplate(pos, tplName, inTpl, OnlyTerm))
        if (members.isEmpty)
          members = completeSearch(lookupInTemplate(pos, tplName, inTpl, OnlyType))

        members
    }
    //println("lookupInTemplate(" + members + ", " + inTpl + ") => " + result)
    result
  }

  private def lookupInTemplate(pos: Position, member: String, inTpl: DocTemplateImpl, strategy: SearchStrategy): List[MemberImpl] = {
    val name = member.stripSuffix("$").stripSuffix("!").stripSuffix("*")
    val result = if (member.endsWith("$"))
      inTpl.members.filter(mbr => (mbr.name == name) && (mbr.isTerm))
    else if (member.endsWith("!"))
      inTpl.members.filter(mbr => (mbr.name == name) && (mbr.isType))
    else if (member.endsWith("*"))
      inTpl.members.filter(mbr => (mbr.signature.startsWith(name)))
    else {
      if (strategy == BothTypeAndTerm)
        inTpl.members.filter(_.name == name)
      else if (strategy == OnlyType)
        inTpl.members.filter(mbr => (mbr.name == name) && (mbr.isType))
      else if (strategy == OnlyTerm)
        inTpl.members.filter(mbr => (mbr.name == name) && (mbr.isTerm))
      else
        Nil
    }

    //println("lookupInTemplate(" + member + ", " + inTpl + ") => " + result)
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

        members ::= query.substring(last_index, index).replaceAll("\\\\([#\\.])", "$1")
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