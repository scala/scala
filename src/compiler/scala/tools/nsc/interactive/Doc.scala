/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Eugene Vigdorchik
 */

package scala.tools.nsc
package interactive

import doc.base._
import comment._
import scala.reflect.internal.util.SourceFile
import scala.xml.NodeSeq

sealed trait DocResult
final case class UrlResult(url: String) extends DocResult
final case class HtmlResult(comment: Comment) extends DocResult

trait Doc extends MemberLookupBase with CommentFactoryBase { self: interactive.Global =>
  override val settings: doc.Settings

  val global: this.type = this
 
  def chooseLink(links: List[LinkTo]): LinkTo

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    if (sym.isClass || sym.isModule)
      Some(LinkToTpl(sym))
    else
      if ((site.isClass || site.isModule) && site.info.members.toList.contains(sym))
        Some(LinkToMember(sym, site))
      else None

  override def toString(link: LinkTo) = ask { () =>
    link match {
      case LinkToMember(mbr: Symbol, site: Symbol) =>
        mbr.signatureString + " in " + site.toString
      case LinkToTpl(sym: Symbol) => sym.toString
      case _ => link.toString
    }
  }

 /**
  * Gets the documentation either as a Comment object or as a URL.
  * @param sym      The symbol whose documentation should be retrieved.
  * @param site     The place where sym is observed.
  * @param source   The source file to look for the symbol.
  */
  def retrieve(sym: Symbol, site: Symbol, source: SourceFile): Option[DocResult] = {
    debugLog("Retrieve documentation for "+sym)
    assert(onCompilerThread, "!onCompilerThread")

    val html = withTempUnit(source){ u =>
      val mirror = forceDocComment(sym, site, u)
      if (mirror eq NoSymbol)
        None
      else {
        val expanded = expandedDocComment(mirror, site)
        if (!expanded.isEmpty) {
          val comment = parseAtSymbol(expanded, rawDocComment(mirror), docCommentPos(mirror), Some(site))
          Some(HtmlResult(comment))
        } else
          None
      }
    }

    html orElse {
      val sig = externalSignature(sym)
      findExternalLink(sym, sig) map { link => UrlResult(link.url) }
    }
  }
}
