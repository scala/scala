/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
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

abstract class Doc(val settings: doc.Settings) extends MemberLookupBase with CommentFactoryBase {

  override val global: interactive.Global
  import global._
 
  def chooseLink(links: List[LinkTo]): LinkTo

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    ask { () => 
      if (sym.isClass || sym.isModule)
        Some(LinkToTpl(sym))
      else
        if ((site.isClass || site.isModule) && site.info.members.toList.contains(sym))
          Some(LinkToMember(sym, site))
        else
          None
    }

  override def toString(link: LinkTo) = ask { () =>
    link match {
      case LinkToMember(mbr: Symbol, site: Symbol) =>
        mbr.signatureString + " in " + site.toString
      case LinkToTpl(sym: Symbol) => sym.toString
      case _ => link.toString
    }
  }

  def retrieve(sym: Symbol, site: Symbol): Option[DocResult] = {
    val sig = ask { () => externalSignature(sym) }
    findExternalLink(sym, sig) map { link => UrlResult(link.url) } orElse {
      val resp = new Response[Tree]
      // Ensure docComment tree is type-checked.
      val pos = ask { () => docCommentPos(sym) }
      askTypeAt(pos, resp)
      resp.get.left.toOption flatMap { _ =>
        ask { () =>
          val comment = parseAtSymbol(expandedDocComment(sym), rawDocComment(sym), pos, Some(site))
          Some(HtmlResult(comment))
        }
      }
    }
  }
}
