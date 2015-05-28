/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala
package tools
package nsc
package doc
package html

import base._
import base.comment._
import model._

import scala.xml.NodeSeq
import scala.xml.Elem
import scala.xml.dtd.{DocType, PublicID}
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
    val doctype = DocType("html")
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
      w.write(doctype.toString + "\n")
      w.write(xml.Xhtml.toXhtml(html))
      w.write('\n')
    }

    if (site.universe.settings.docRawOutput)
      writeFile(site, ".raw") {
        // we're only interested in the body, as this will go into the diff
        _.write(body.text)
      }
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
      <pre>{ SyntaxHigh(data) }</pre> //<pre>{ scala.xml.Text(data) }</pre>
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
    case Link(raw, title) => <a href={ raw } target="_blank">{ inlineToHtml(title) }</a>
    case Monospace(in) => <code>{ inlineToHtml(in) }</code>
    case Text(text) => scala.xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => scala.xml.Unparsed(tag)
    case EntityLink(target, link) => linkToHtml(target, link, hasLinks = true)
  }

  def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean) = link match {
    case LinkToTpl(dtpl: TemplateEntity) =>
      if (hasLinks)
        <a href={ relativeLinkTo(dtpl) } class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</a>
      else
        <span class="extype" name={ dtpl.qualifiedName }>{ inlineToHtml(text) }</span>
    case LinkToMember(mbr: MemberEntity, inTpl: TemplateEntity) =>
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

  object Image extends Enumeration {
    val Trait, Class, Type, Object, Package = Value
  }

  /** Returns the _big image name and the alt attribute
   *  corresponding to the DocTemplate Entity (upper left icon) */
  def docEntityKindToBigImage(ety: DocTemplateEntity) = {
    def entityToImage(e: DocTemplateEntity) =
      if (e.isTrait)                              Image.Trait
      else if (e.isClass)                         Image.Class
      else if (e.isAbstractType || e.isAliasType) Image.Type
      else if (e.isObject)                        Image.Object
      else if (e.isPackage)                       Image.Package
      else {
        // FIXME: an entity *should* fall into one of the above categories,
        // but AnyRef is somehow not
        Image.Class
      }

    val image = entityToImage(ety)
    val companionImage = ety.companion filter {
      e => e.visibility.isPublic && ! e.inSource.isEmpty
    } map { entityToImage }

    (image, companionImage) match {
      case (from, Some(to)) =>
        ((from + "_to_" + to + "_big.png").toLowerCase, from + "/" + to)
      case (from, None) =>
        ((from + "_big.png").toLowerCase, from.toString)
    }
  }

  def permalink(template: Entity, isSelf: Boolean = true): Elem =
    <span class="permalink">
      <a href={ memberToUrl(template, isSelf) } title="Permalink" target="_top">
        <img src={ relativeLinkTo(List("permalink.png", "lib")) } alt="Permalink" />
      </a>
    </span>
	
  def docEntityKindToCompanionTitle(ety: DocTemplateEntity, baseString: String = "See companion") = 
    ety.companion match{
	  case Some(companion) => 
	    s"$baseString${
		if(companion.isObject) " object"
		else if(companion.isTrait) " trait"
		else if(companion.isClass) " class"
		else ""
		}"
	  case None => baseString
	}

  def companionAndPackage(tpl: DocTemplateEntity): Elem =
    <span class="morelinks">{
      tpl.companion match {
        case Some(companionTpl) =>
          val objClassTrait =
            if (companionTpl.isObject) s"object ${tpl.name}"
            else if (companionTpl.isTrait) s"trait ${companionTpl.name}"
            else s"class ${companionTpl.name}"
          <div>
            Related Docs:
            <a href={relativeLinkTo(tpl.companion.get)} title={docEntityKindToCompanionTitle(tpl)}>{objClassTrait}</a>
            | {templateToHtml(tpl.inTemplate, s"package ${tpl.inTemplate.name}")}
          </div>
        case None =>
          <div>Related Doc:
            {templateToHtml(tpl.inTemplate, s"package ${tpl.inTemplate.name}")}
          </div>
      }
    }</span>

  def memberToUrl(template: Entity, isSelf: Boolean = true): String = {
    val (signature: Option[String], containingTemplate: TemplateEntity) = template match {
      case dte: DocTemplateEntity if (!isSelf) => (Some(dte.signature), dte.inTemplate)
      case dte: DocTemplateEntity => (None, dte)
      case me: MemberEntity => (Some(me.signature), me.inTemplate)
      case tpl => (None, tpl)
    }

    def hashFromPath(templatePath: List[String]): String =
      ((templatePath.head.replace(".html", "") :: templatePath.tail).reverse).mkString(".")

    val containingTemplatePath = templateToPath(containingTemplate)
    val url = "../" * (containingTemplatePath.size - 1) + "index.html"
    val hash = hashFromPath(containingTemplatePath)
    s"$url#$hash" + signature.map("@" + _).getOrElse("")
  }
}
