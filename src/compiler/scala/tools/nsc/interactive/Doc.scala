/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Eugene Vigdorchik
 */

package scala.tools.nsc
package interactive

import doc.base._
import comment._
import scala.xml.NodeSeq

sealed trait DocResult
final case class UrlResult(url: String) extends DocResult
final case class HtmlResult(comment: Comment) extends DocResult

trait Doc extends MemberLookupBase with CommentFactoryBase { self: interactive.Global =>
  override val settings: doc.Settings

  val global: this.type = this
 
  def chooseLink(links: List[LinkTo]): LinkTo

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    ask { () => 
      if (sym.isClass || sym.isModule)
        Some(LinkToTpl(sym))
      else
        if ((site.isClass || site.isModule) && site.info.members.toList.contains(sym))
          Some(LinkToMember(sym, site))
        else None
    }

  override def toString(link: LinkTo) = ask { () =>
    link match {
      case LinkToMember(mbr: Symbol, site: Symbol) =>
        mbr.signatureString + " in " + site.toString
      case LinkToTpl(sym: Symbol) => sym.toString
      case _ => link.toString
    }
  }

  def retrieve(sym: Symbol, site: Symbol): Option[DocResult] = ask { () =>
    if (sym.pos ne NoPosition) {
      // Either typer has been run and we don't find DocDef,
      // or we force the targeted typecheck here.
      // In both cases doc comment maps should be filled for our symbol.
      val du =
        for(unit <- unitOfFile get sym.sourceFile;
            dt <- unit.body find { t =>
              t match {
                case DocDef(cmt, defn) if defn.symbol == sym => true
                case _ => false
              }
            }) yield (dt, unit)

      for ((dt, unit) <- du) {
        debugLog("Found DocDef tree")
        val prevPos = unit.targetPos
        try {
          unit.targetPos = dt.pos
          typeCheck(unit)
        } finally
          unit.targetPos = prevPos
      }

      val expanded = expandedDocComment(sym, site)
      if (!expanded.isEmpty) {
        val comment = parseAtSymbol(expanded, rawDocComment(sym), docCommentPos(sym), Some(site))
        Some(HtmlResult(comment))
      } else None
    } else {
      val sig = externalSignature(sym)
      findExternalLink(sym, sig) map { link => UrlResult(link.url) }
    }
  }
}
