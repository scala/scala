package scala.tools.nsc
package doc
package base

import comment._

/** This trait extracts all required information for documentation from compilation units.
 *  The base trait has been extracted to allow getting light-weight documentation
  * for a particular symbol in the IDE.*/
trait MemberLookupBase {

  val global: Global
  val settings: doc.Settings

  import global._
  def internalLink(sym: Symbol, site: Symbol): Option[LinkTo]
  def chooseLink(links: List[LinkTo]): LinkTo
  def toString(link: LinkTo): String

  import global._
  import definitions.{ NothingClass, AnyClass, AnyValClass, AnyRefClass, ListClass }
  import rootMirror.{RootPackage, EmptyPackage}

  private def isRoot(s: Symbol) = s.isRootSymbol || s.isEmptyPackage || s.isEmptyPackageClass

  def makeEntityLink(title: Inline, pos: Position, query: String, siteOpt: Option[Symbol]) =
    new EntityLink(title) { lazy val link = memberLookup(pos, query, siteOpt) }

  private var showExplanation = true
  private def explanation: String =
    if (showExplanation) {
      showExplanation = false
      """
      |Quick crash course on using Scaladoc links
      |==========================================
      |Disambiguating terms and types: Prefix terms with '$' and types with '!' in case both names are in use:
      | - [[scala.collection.immutable.List!.apply class List's apply method]] and
      | - [[scala.collection.immutable.List$.apply object List's apply method]]
      |Disambiguating overloaded members: If a term is overloaded, you can indicate the first part of its signature followed by *:
      | - [[[scala.collection.immutable.List$.fill[A](Int)(⇒A):List[A]* Fill with a single parameter]]]
      | - [[[scala.collection.immutable.List$.fill[A](Int,Int)(⇒A):List[List[A]]* Fill with a two parameters]]]
      |Notes:
      | - you can use any number of matching square brackets to avoid interference with the signature
      | - you can use \\. to escape dots in prefixes (don't forget to use * at the end to match the signature!)
      | - you can use \\# to escape hashes, otherwise they will be considered as delimiters, like dots.""".stripMargin
    } else ""

  def memberLookup(pos: Position, query: String, siteOpt: Option[Symbol]): LinkTo = {
    var members = breakMembers(query)

    // (1) First look in the root package, as most of the links are qualified
    val fromRoot = lookupInRootPackage(pos, members)

    // (2) Or recursively go into each containing template.
    val fromParents = siteOpt.fold(Stream.empty[Symbol]) { s =>
      Stream.iterate(s)(_.owner)
    }.takeWhile (!isRoot(_)).map {
      lookupInTemplate(pos, members, _)
    }

    val syms = (fromRoot +: fromParents) find (!_.isEmpty) getOrElse Nil

    val links = syms flatMap { case (sym, site) => internalLink(sym, site) } match {
      case Nil =>
        // (3) Look at external links
        syms.flatMap { case (sym, owner) =>
          // reconstruct the original link
          def linkName(sym: Symbol) = {
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
    links match {
      case Nil =>
        if (!settings.docNoLinkWarnings.value)
          reporter.warning(pos, "Could not find any member to link for \"" + query + "\".")
        // (4) if we still haven't found anything, create a tooltip
        Tooltip(query)
      case List(l) => l
      case links => 
        val chosen = chooseLink(links)
        def linkToString(link: LinkTo) = {
          val chosenInfo =
            if (link == chosen) " [chosen]" else ""
          toString(link) + chosenInfo + "\n"
        }
        if (!settings.docNoLinkWarnings.value) {
          val allLinks = links.map(linkToString).mkString
          reporter.warning(pos,
            s"""The link target \"$query\" is ambiguous. Several members fit the target:
            |$allLinks
            |$explanation""".stripMargin)
        }
        chosen
    }
  }

  private sealed trait SearchStrategy
  private case object BothTypeAndTerm extends SearchStrategy
  private case object OnlyType extends SearchStrategy
  private case object OnlyTerm extends SearchStrategy

  private def lookupInRootPackage(pos: Position, members: List[String]) =
    lookupInTemplate(pos, members, EmptyPackage) ::: lookupInTemplate(pos, members, RootPackage)

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
      strategy match {
        case BothTypeAndTerm => termSyms ::: typeSyms
        case OnlyType => typeSyms
        case OnlyTerm => termSyms
      }

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


  def findExternalLink(sym: Symbol, name: String): Option[LinkToExternal] = {
    val sym1 =
      if (sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == NothingClass) ListClass
      else if (sym.isPackage) 
        /* Get package object which has associatedFile ne null */
        sym.info.member(newTermName("package"))
      else sym
    Option(sym1.associatedFile) flatMap (_.underlyingSource) flatMap { src =>
      val path = src.path
      settings.extUrlMapping get path map { url =>
        LinkToExternal(name, url + "#" + name)
      }
    } orElse {
      // Deprecated option.
      settings.extUrlPackageMapping find {
        case (pkg, _) => name startsWith pkg
      } map {
        case (_, url) => LinkToExternal(name, url + "#" + name)
      }
    }
  }

  def externalSignature(sym: Symbol) = {
    sym.info // force it, otherwise we see lazy types
    (sym.nameString + sym.signatureString).replaceAll("\\s", "")
  }
}
