/* NSC -- new Scala compiler
 * Copyright 2007-2012 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import model._
import comment._

import scala.xml.{XML, NodeSeq}
import scala.xml.dtd.{DocType, PublicID}
import scala.collection._
import java.io.Writer

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage extends Page with HtmlPageBase { thisPage =>
  /** The title of this page. */
  protected def title: String

  /** The page description */
  protected def description: String =
    // unless overwritten, will display the title in a spaced format, keeping - and .
    title.replaceAll("[^a-zA-Z0-9\\.\\-]+", " ").replaceAll("\\-+", " - ").replaceAll(" +", " ")

  /** The page keywords */
  protected def keywords: String =
    // unless overwritten, same as description, minus the " - "
    description.replaceAll(" - ", " ")

  /** Additional header elements (links, scripts, meta tags, etc.) required for this page. */
  protected def headers: NodeSeq

  /** The body of this page. */
  def body: NodeSeq

  def writeFor(site: HtmlFactory) {
    val doctype =
      DocType("html", PublicID("-//W3C//DTD XHTML 1.1//EN", "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"), Nil)
    val html =
      <html>
        <head>
          <title>{ title }</title>
          <meta name="description" content={ description }/>
          <meta name="keywords" content={ keywords }/>
          <meta http-equiv="content-type" content={ "text/html; charset=" + site.encoding }/>
          { headers }
        </head>
        { body }
      </html>

    writeFile(site) { (w: Writer) =>
      w.write("<?xml version='1.0' encoding='" + site.encoding + "'?>\n")
      w.write(doctype.toString + "\n")
      w.write(xml.Xhtml.toXhtml(html))
    }

    if (site.universe.settings.docRawOutput.value)
      writeFile(site, ".raw") {
        // we're only interested in the body, as this will go into the diff
        _.write(body.text)
      }

    //XML.save(pageFile.getPath, html, site.encoding, xmlDecl = false, doctype = doctype)
  }

  override def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean) = link match {
    case LinkToTpl(dtpl: DocTemplateEntity) =>
      if (hasLinks)
        <a href={ relativeLinkTo(dtpl) } class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</a>
      else
        <span class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</span>
    case LinkToMember(mbr: MemberEntity, inTpl: DocTemplateEntity) =>
      if (hasLinks)
        <a href={ relativeLinkTo(inTpl) + "#" + mbr.signature } class="extmbr" name={ mbr.qualifiedName }>{ inlineToHtml(text) }</a>
      else
        <span class="extmbr" name={ mbr.qualifiedName }>{ inlineToHtml(text) }</span>
    case Tooltip(tooltip) =>
      <span class="extype" name={ tooltip }>{ inlineToHtml(text) }</span>
    case LinkToExternal(name, url) =>
      <a href={ url } class="extype" target="_top">{ inlineToHtml(text) }</a>
    case _ =>
      inlineToHtml(text)
  }

  def typeToHtml(tpes: List[model.TypeEntity], hasLinks: Boolean): NodeSeq = tpes match {
    case Nil =>
      NodeSeq.Empty
    case List(tpe) =>
      typeToHtml(tpe, hasLinks)
    case tpe :: rest =>
      typeToHtml(tpe, hasLinks) ++ scala.xml.Text(" with ") ++ typeToHtml(rest, hasLinks)
  }

  def typeToHtml(tpe: model.TypeEntity, hasLinks: Boolean): NodeSeq = {
    val string = tpe.name
    def toLinksOut(inPos: Int, starts: List[Int]): NodeSeq = {
      if (starts.isEmpty && (inPos == string.length))
        NodeSeq.Empty
      else if (starts.isEmpty)
        scala.xml.Text(string.slice(inPos, string.length))
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        scala.xml.Text(string.slice(inPos, starts.head)) ++ toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): NodeSeq = {
      val (link, width) = tpe.refEntity(inPos)
      val text = comment.Text(string.slice(inPos, inPos + width))
      linkToHtml(text, link, hasLinks) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      scala.xml.Text(string)
  }

  def typesToHtml(tpess: List[model.TypeEntity], hasLinks: Boolean, sep: NodeSeq): NodeSeq = tpess match {
    case Nil         => NodeSeq.Empty
    case tpe :: Nil  => typeToHtml(tpe, hasLinks)
    case tpe :: tpes => typeToHtml(tpe, hasLinks) ++ sep ++ typesToHtml(tpes, hasLinks, sep)
  }

  def hasPage(e: DocTemplateEntity) = {
    e.isPackage || e.isTrait || e.isClass || e.isObject || e.isCaseClass
  }

  /** Returns the HTML code that represents the template in `tpl` as a hyperlinked name. */
  def templateToHtml(tpl: TemplateEntity, name: String = null) = tpl match {
    case dTpl: DocTemplateEntity =>
      if (hasPage(dTpl)) {
        <a href={ relativeLinkTo(dTpl) } class="extype" name={ dTpl.qualifiedName }>{ if (name eq null) dTpl.name else name }</a>
      } else {
        scala.xml.Text(if (name eq null) dTpl.name else name)
      }
    case ndTpl: NoDocTemplate =>
      scala.xml.Text(if (name eq null) ndTpl.name else name)
  }

  /** Returns the HTML code that represents the templates in `tpls` as a list of hyperlinked names. */
  def templatesToHtml(tplss: List[TemplateEntity], sep: NodeSeq): NodeSeq = tplss match {
    case Nil         => NodeSeq.Empty
    case tpl :: Nil  => templateToHtml(tpl)
    case tpl :: tpls => templateToHtml(tpl) ++ sep ++ templatesToHtml(tpls, sep)
  }

  /** Returns the _big image name corresponding to the DocTemplate Entity (upper left icon) */
  def docEntityKindToBigImage(ety: DocTemplateEntity) =
    if (ety.isTrait && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "trait_to_object_big.png"
    else if (ety.isTrait) "trait_big.png"
    else if (ety.isClass && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "class_to_object_big.png"
    else if (ety.isClass) "class_big.png"
    else if ((ety.isAbstractType || ety.isAliasType) && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None) "type_to_object_big.png"
    else if ((ety.isAbstractType || ety.isAliasType)) "type_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isClass) "object_to_class_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isTrait) "object_to_trait_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && (ety.companion.get.isAbstractType || ety.companion.get.isAliasType)) "object_to_trait_big.png"
    else if (ety.isObject) "object_big.png"
    else if (ety.isPackage) "package_big.png"
    else "class_big.png"  // FIXME: an entity *should* fall into one of the above categories, but AnyRef is somehow not
}
