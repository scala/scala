/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import model._
import comment._

import xml.{XML, NodeSeq}
import xml.dtd.{DocType, PublicID}
import scala.collection._
import java.io.Writer

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage extends Page { thisPage =>
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

  /** Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
    * node sequence if it is not. */
  def commentToHtml(comment: Option[Comment]): NodeSeq =
    (comment map (commentToHtml(_))) getOrElse NodeSeq.Empty

  /** Transforms a comment into an styled HTML tree representing its body. */
  def commentToHtml(comment: Comment): NodeSeq =
    bodyToHtml(comment.body)

  def bodyToHtml(body: Body): NodeSeq =
    body.blocks flatMap (blockToHtml(_))

  def blockToHtml(block: Block): NodeSeq = block match {
    case Title(in, 1) => <h3>{ inlineToHtml(in) }</h3>
    case Title(in, 2) => <h4>{ inlineToHtml(in) }</h4>
    case Title(in, 3) => <h5>{ inlineToHtml(in) }</h5>
    case Title(in, _) => <h6>{ inlineToHtml(in) }</h6>
    case Paragraph(in) => <p>{ inlineToHtml(in) }</p>
    case Code(data) =>
      <pre>{ SyntaxHigh(data) }</pre> //<pre>{ xml.Text(data) }</pre>
    case UnorderedList(items) =>
      <ul>{ listItemsToHtml(items) }</ul>
    case OrderedList(items, listStyle) =>
      <ol class={ listStyle }>{ listItemsToHtml(items) }</ol>
    case DefinitionList(items) =>
      <dl>{items map { case (t, d) => <dt>{ inlineToHtml(t) }</dt><dd>{ blockToHtml(d) }</dd> } }</dl>
    case HorizontalRule() =>
      <hr/>
  }

  def listItemsToHtml(items: Seq[Block]) =
    items.foldLeft(xml.NodeSeq.Empty){ (xmlList, item) =>
      item match {
        case OrderedList(_, _) | UnorderedList(_) =>  // html requires sub ULs to be put into the last LI
          xmlList.init ++ <li>{ xmlList.last.child ++ blockToHtml(item) }</li>
        case Paragraph(inline) =>
          xmlList :+ <li>{ inlineToHtml(inline) }</li>  // LIs are blocks, no need to use Ps
        case block =>
          xmlList :+ <li>{ blockToHtml(block) }</li>
      }
  }

  def inlineToHtml(inl: Inline): NodeSeq = inl match {
    case Chain(items) => items flatMap (inlineToHtml(_))
    case Italic(in) => <i>{ inlineToHtml(in) }</i>
    case Bold(in) => <b>{ inlineToHtml(in) }</b>
    case Underline(in) => <u>{ inlineToHtml(in) }</u>
    case Superscript(in) => <sup>{ inlineToHtml(in) }</sup>
    case Subscript(in) => <sub>{ inlineToHtml(in) }</sub>
    case Link(raw, title) => <a href={ raw }>{ inlineToHtml(title) }</a>
    case Monospace(in) => <code>{ inlineToHtml(in) }</code>
    case Text(text) => xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => xml.Unparsed(tag)
    case EntityLink(target, template) => template() match {
      case Some(tpl) =>
        templateToHtml(tpl)
      case None =>
        xml.Text(target)
    }
  }

  def typeToHtml(tpes: List[model.TypeEntity], hasLinks: Boolean): NodeSeq = tpes match {
    case Nil =>
      sys.error("Internal Scaladoc error")
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
        xml.Text(string.slice(inPos, string.length))
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        xml.Text(string.slice(inPos, starts.head)) ++ toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): NodeSeq = {
      val (tpl, width) = tpe.refEntity(inPos)
      (tpl match {
        case dtpl:DocTemplateEntity if hasLinks =>
          <a href={ relativeLinkTo(dtpl) } class="extype" name={ dtpl.qualifiedName }>{
            string.slice(inPos, inPos + width)
          }</a>
        case tpl =>
          <span class="extype" name={ tpl.qualifiedName }>{ string.slice(inPos, inPos + width) }</span>
      }) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      xml.Text(string)
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
        xml.Text(if (name eq null) dTpl.name else name)
      }
    case ndTpl: NoDocTemplate =>
      xml.Text(if (name eq null) ndTpl.name else name)
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
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isClass) "object_to_class_big.png"
    else if (ety.isObject && !ety.companion.isEmpty && ety.companion.get.visibility.isPublic && ety.companion.get.inSource != None && ety.companion.get.isTrait) "object_to_trait_big.png"
    else if (ety.isObject) "object_big.png"
    else if (ety.isPackage) "package_big.png"
    else "class_big.png"  // FIXME: an entity *should* fall into one of the above categories, but AnyRef is somehow not

}
