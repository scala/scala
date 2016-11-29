package scala.tools.nsc
package doc
package model

import base._

/** This trait extracts all required information for documentation from compilation units */
trait MemberLookup extends base.MemberLookupBase {
  thisFactory: ModelFactory =>

  import global._
  import definitions.{ NothingClass, AnyClass, AnyValClass, AnyRefClass, ListClass }

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    findTemplateMaybe(sym) match {
      case Some(tpl) => Some(LinkToTpl(tpl))
      case None =>
        findTemplateMaybe(site) flatMap { inTpl =>
          inTpl.members find (_.asInstanceOf[EntityImpl].sym == sym) map (LinkToMember(_, inTpl))
        }
    }

  override def chooseLink(links: List[LinkTo]): LinkTo = {
    val mbrs = links.collect {
      case lm@LinkToMember(mbr: MemberEntity, _) => (mbr, lm)
    }
    if (mbrs.isEmpty)
      links.head
    else
      mbrs.min(Ordering[MemberEntity].on[(MemberEntity, LinkTo)](_._1))._2
  }

  override def toString(link: LinkTo) = link match {
    case LinkToTpl(tpl: EntityImpl) => tpl.sym.toString
    case LinkToMember(mbr: EntityImpl, inTpl: EntityImpl) =>
      mbr.sym.signatureString + " in " + inTpl.sym.toString
    case _ => link.toString
  }

  override def findExternalLink(sym: Symbol, name: String): Option[LinkToExternal] = {
    val sym1 =
      if (sym == AnyClass || sym == AnyRefClass || sym == AnyValClass || sym == NothingClass) ListClass
      else if (sym.hasPackageFlag)
        /* Get package object which has associatedFile ne null */
        sym.info.member(newTermName("package"))
      else sym
    def classpathEntryFor(s: Symbol): Option[String] = {
      Option(s.associatedFile).flatMap(_.underlyingSource).map { src =>
        val path = src.canonicalPath
        if(path.endsWith(".class")) { // Individual class file -> Classpath entry is root dir
          var nesting = s.ownerChain.count(_.hasPackageFlag)
          if(nesting > 0) {
            val p = 0.until(nesting).foldLeft(src) {
              case (null, _) => null
              case (f, _) => f.container
            }
            if(p eq null) path else p.canonicalPath
          } else path
        } else path // JAR file (and fallback option)
      }
    }
    classpathEntryFor(sym1) flatMap { path =>
      settings.extUrlMapping get path map { url =>
        LinkToExternal(name, url + "#" + name)
      }
    }
  }

  override def warnNoLink = !settings.docNoLinkWarnings.value
}
