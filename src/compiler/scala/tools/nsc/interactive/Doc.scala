/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author  Eugene Vigdorchik
 */

package scala.tools.nsc
package interactive

import doc.model.{ LinkTo, LinkToTpl, LinkToMember }
import doc.model.base._
import doc.model.comment._
import scala.xml.{ Comment => _, _ }
import doc.html.HtmlPageBase

sealed trait DocResult
final case class UrlResult(url: String) extends DocResult
final case class HtmlResult(html: String) extends DocResult

class Doc(val global: interactive.Global, val settings: doc.Settings)
  extends MemberLookupBase with CommentFactoryBase with HtmlPageBase {

  import global._
 
  //TODO: proper links handling
  def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean) = inlineToHtml(text)

  override def internalLink(sym: Symbol, site: Symbol): Option[LinkTo] =
    getUnitOf(sym.pos.source) flatMap { _ =>
      if (sym.isClass || sym.isModule)
        Some(LinkToTpl(sym))
      else
        if ((site.isClass || site.isModule) && site.info.members.toList.contains(sym))
          Some(LinkToMember(sym, site))
        else
          None
    }

  override def chooseLink(links: List[LinkTo]): LinkTo = links.head

  def retrieve(sym: Symbol, site: Symbol): Option[DocResult] = {
    val sig = externalSignature(sym)
    findExternalLink(sym, sig) map { link => UrlResult(link.url) } orElse {
      val resp = new Response[Tree]
      // Ensure docComment tree is type-checked.
      askTypeAt(sym.pos, resp)
      resp.get.left.toOption flatMap { _ =>
        val comment = parseAtSymbol(expandedDocComment(sym), rawDocComment(sym), docCommentPos(sym), Some(site))
        Some(HtmlResult(toHtml(comment)))
      }
    }
  }

  def toHtml(comment: Comment): String = {
    val html =
      <html><body>
	  { commentToHtml(comment) ++ tagsToHtml(comment) }
      </body></html>
    xml.Xhtml.toXhtml(html)
  }
}
