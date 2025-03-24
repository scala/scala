/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools
package nsc
package doc
package html

import base._
import base.comment._
import model._
import scala.reflect.internal.Reporter
import scala.collection.{immutable, _}
import java.io.Writer
import java.net.URI

import javax.xml.stream.XMLOutputFactory

import scala.annotation.nowarn

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage extends Page { thisPage =>
  import HtmlTags._

  /** The title of this page. */
  protected def title: String

  /** ScalaDoc reporter for error handling */
  protected def docletReporter: Reporter

  /** The page description */
  protected def description: String =
    // unless overwritten, will display the title in a spaced format, keeping - and .
    title.replaceAll("[^a-zA-Z0-9\\.\\-]+", " ").replaceAll("\\-+", " - ").replaceAll(" +", " ")

  /** The page keywords */
  protected def keywords: String =
    // unless overwritten, same as description, minus the " - "
    description.replaceAll(" - ", " ")

  /** Additional header elements (links, scripts, meta tags, etc.) required for this page. */
  protected def headers: Elems

  /** The body of this page. */
  def body: Elems

  def writeHtml(encoding: String)(w: Writer) = {
    val html =
      Html(Head(Meta(`http-equiv` = "X-UA-Compatible", content = "IE=edge") ::
                Meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no") ::
                HtmlTags.Title(elems= Txt(title)) ::
                Meta(name = "description", content = description) ::
                Meta(name = "keywords", content = keywords) ::
                Meta(`http-equiv` = "content-type", content = s"text/html; charset=${encoding}") :: headers) :: body)

    val xsw = XMLOutputFactory.newInstance.createXMLStreamWriter(w)
    xsw.writeDTD("<!DOCTYPE html >")
    html.toXhtml(xsw, w)
    xsw.flush()
    xsw.close()
    w.write('\n')
  }

  def writeFor(site: HtmlFactory): Unit = {
    writeFile(site)(writeHtml(site.encoding))

    if (site.universe.settings.docRawOutput.value)
      writeFile(site, ".raw") {
        // we're only interested in the body, as this will go into the diff
        _.write(textOf(body))
      }
  }

  /** Transforms an optional comment into an styled HTML tree representing its body if it is defined, or into an empty
    * node sequence if it is not. */
  def commentToHtml(comment: Option[Comment]): Elems =
    (comment map (commentToHtml(_))) getOrElse NoElems

  /** Transforms a comment into an styled HTML tree representing its body. */
  def commentToHtml(comment: Comment): Elems =
    bodyToHtml(comment.body)

  def bodyToHtml(body: comment.Body): Elems =
    body.blocks.toList flatMap (blockToHtml(_))

  def blockToHtml(block: Block): Elems = (block match {
    case comment.Title(in, 1)          => H(3, inlineToHtml(in))
    case comment.Title(in, 2)          => H(4, inlineToHtml(in))
    case comment.Title(in, 3)          => H(5, inlineToHtml(in))
    case comment.Title(in, _)          => H(6, inlineToHtml(in))
    case Paragraph(in)                 => P(inlineToHtml(in))
    case comment.Code(data)            => Pre(SyntaxHigh(data)) //<pre>{ Txt(data) }</pre>
    case UnorderedList(items)          => Ul(listItemsToHtml(items))
    case OrderedList(items, listStyle) => Ol(`class`= listStyle, elems= listItemsToHtml(items))
    case DefinitionList(items)         => Dl(items.toList flatMap { case (t, d) => Dt(inlineToHtml(t)) :: Dd(blockToHtml(d)) :: NoElems })
    case HorizontalRule()              => Hr
    case tbl: comment.Table                    => tableToHtml(tbl)
  }) :: NoElems

  def listItemsToHtml(items: Seq[Block]) =
    items.foldLeft(NoElems){ (xmlList, item) =>
      item match {
        // html requires sub ULs to be put into the last LI
        case OrderedList(_, _) | UnorderedList(_) => xmlList.init ++ Li(xmlList.last.elems ++ blockToHtml(item))
        case Paragraph(inline)                    => xmlList :+ Li(inlineToHtml(inline)) // LIs are blocks, no need to use Ps
        case block                                => xmlList :+ Li(blockToHtml(block))
      }
  }

  def inlineToHtml(inl: Inline): Elems = inl match {
    case Chain(items)             => items.toList flatMap (inlineToHtml(_))
    case Italic(in)               => I(inlineToHtml(in)) :: NoElems
    case Bold(in)                 => B(inlineToHtml(in)) :: NoElems
    case Underline(in)            => U(inlineToHtml(in)) :: NoElems
    case Superscript(in)          => Sup(inlineToHtml(in)) :: NoElems
    case Subscript(in)            => Sub(inlineToHtml(in)) :: NoElems
    case comment.Link(raw, title) => A(href = raw, target = "_blank", elems = inlineToHtml(title)) :: NoElems
    case Monospace(in)            => Code(inlineToHtml(in)) :: NoElems
    case Text(text)               => Txt(text) :: NoElems
    case Summary(in)              => inlineToHtml(in)
    case HtmlTag(tag)             => Raw(tag) :: NoElems
    case EntityLink(target, link) => linkToHtml(target, link, hasLinks = true)
  }

  def linkToHtml(text: Inline, link: LinkTo, hasLinks: Boolean): Elems = link match {
    case LinkToTpl(dtpl: TemplateEntity) =>
      (if (hasLinks) A(href=relativeLinkTo(dtpl), `class` = "extype", id=dtpl.qualifiedName, name=dtpl.qualifiedName, elems=inlineToHtml(text))
      else Span(`class` = "extype", name=dtpl.qualifiedName, elems=inlineToHtml(text))) :: NoElems
    case LinkToMember(mbr: MemberEntity, inTpl: TemplateEntity) =>
      (if (hasLinks) A(href=relativeLinkTo(inTpl) + "#" + mbr.signature, `class`= "extmbr", id=mbr.qualifiedName, name=mbr.qualifiedName, elems=inlineToHtml(text))
      else Span(`class`= "extmbr", name=mbr.qualifiedName, elems=inlineToHtml(text))) :: NoElems
    case Tooltip(tooltip) =>
      Span(`class` = "extype", name=tooltip, elems = inlineToHtml(text)) :: NoElems
    case LinkToExternalTpl(name, baseUrlString, dtpl: TemplateEntity) =>
      val baseUrl = new URI(Page.makeUrl(baseUrlString, Page.templateToPath(dtpl)))
      val url = if (name.isEmpty) baseUrl
                else new URI(baseUrl.getScheme, baseUrl.getSchemeSpecificPart, name)
      (if (hasLinks) A(href=url.toString, `class` = "extype", id=dtpl.qualifiedName, name=dtpl.qualifiedName, elems = inlineToHtml(text) )
      else Span(`class` = "extype", name=dtpl.qualifiedName, elems= inlineToHtml(text))) :: NoElems
    case _ =>
      inlineToHtml(text)
  }

  private def tableToHtml(table: comment.Table): Elem = {

    val comment.Table(header, columnOptions, rows) = table

    val colClass = Map(
      ColumnOption.ColumnOptionLeft -> "doctbl-left",
      ColumnOption.ColumnOptionCenter -> "doctbl-center",
      ColumnOption.ColumnOptionRight -> "doctbl-right"
    )
    val cc = columnOptions.map(colClass)
    Table(
      thead = THead(
        Tr(
          (header.cells zip cc).toList.map{ case (cell, cls) => Th(cell.blocks.flatMap(blockToHtml).toList, `class` = cls)}
        ) :: immutable.Nil
      ),
      tbody = if (rows.nonEmpty) {
        TBody(
          rows.toList.map {
            row => Tr((row.cells zip cc).toList.map{ case (cell, cls) => Td(elems = cell.blocks.flatMap(blockToHtml).toList, `class`=cls) })
          }
        )
      } else null,
      `class` = "doctbl")
  }

  def typeToHtml(tpes: List[model.TypeEntity], hasLinks: Boolean): Elems = tpes match {
    case Nil         => NoElems
    case List(tpe)   => typeToHtml(tpe, hasLinks)
    case tpe :: rest => typeToHtml(tpe, hasLinks) ++ (Txt(" with ") :: typeToHtml(rest, hasLinks))
  }

  def typeToHtml(tpe: model.TypeEntity, hasLinks: Boolean): Elems = {
    val string = tpe.name
    def toLinksOut(inPos: Int, starts: List[Int]): Elems = {
      if (starts.isEmpty && (inPos == string.length))
        NoElems
      else if (starts.isEmpty)
        Txt(string.slice(inPos, string.length)) :: NoElems
      else if (inPos == starts.head)
        toLinksIn(inPos, starts)
      else {
        Txt(string.slice(inPos, starts.head)) :: toLinksIn(starts.head, starts)
      }
    }
    def toLinksIn(inPos: Int, starts: List[Int]): Elems = {
      val (link, width) = tpe.refEntity(inPos)
      val text = comment.Text(string.slice(inPos, inPos + width))
      linkToHtml(text, link, hasLinks) ++ toLinksOut(inPos + width, starts.tail)
    }
    if (hasLinks)
      toLinksOut(0, tpe.refEntity.keySet.toList)
    else
      Txt(string) :: NoElems
  }

  def typesToHtml(tpess: List[model.TypeEntity], hasLinks: Boolean, sep: Elems): Elems = tpess match {
    case Nil         => NoElems
    case tpe :: Nil  => typeToHtml(tpe, hasLinks)
    case tpe :: tpes => typeToHtml(tpe, hasLinks) ++ sep ++ typesToHtml(tpes, hasLinks, sep)
  }

  def hasPage(e: DocTemplateEntity) = {
    e.isPackage || e.isTrait || e.isClass || e.isObject || e.isCase
  }

  /** Returns the HTML code that represents the template in `tpl` as a hyperlinked name. */
  def templateToHtml(tpl: TemplateEntity, name: String = null): Elem = {
    val txt = Txt(if (name eq null) tpl.name else name)
    tpl match {
      case dTpl: DocTemplateEntity if hasPage(dTpl) => A(href= relativeLinkTo(dTpl), id= dTpl.qualifiedName, name= dTpl.qualifiedName, `class` = "extype", elems = txt :: NoElems)
      case _ => txt
    }
  }

  /** Returns the HTML code that represents the templates in `tpls` as a list of hyperlinked names. */
  def templatesToHtml(tplss: List[TemplateEntity], sep: Elems): Elems = tplss match {
    case Nil         => NoElems
    case tpl :: Nil  => templateToHtml(tpl) :: NoElems
    case tpl :: tpls => templateToHtml(tpl) :: sep ++ templatesToHtml(tpls, sep)
  }

  object Image extends Enumeration {
    val Trait, Class, Type, Object, Package = Value
  }

  def permalink(template: Entity, isSelf: Boolean = true): Elem =
    Span(`class`= "permalink", elems=
      A(href=memberToUrl(template), title="Permalink", elems =
        I(`class`="material-icons", elems=Txt("\uE157"))
        ))

  def docEntityImageClass(tpl: DocTemplateEntity): String =
    tpl.kind + tpl.companion.fold("")("-companion-" + _.kind)

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

  def companionAndPackage(tpl: DocTemplateEntity): Elems =
    Span(`class`= "morelinks", elems =
      tpl.companion match {
        case Some(companionTpl) =>
          val objClassTrait =
            if (companionTpl.isObject) s"object ${tpl.name}"
            else if (companionTpl.isTrait) s"trait ${companionTpl.name}"
            else s"class ${companionTpl.name}"
          Div(elems = Txt("Companion ") :: A(href= relativeLinkTo(companionTpl), title= docEntityKindToCompanionTitle(tpl), elems=Txt(objClassTrait)) :: NoElems)
        case None => NoElems
      })

  private def memberToUrl(template: Entity): String = {
    val (signature: Option[String], containingTemplate: TemplateEntity) = {
      template match {
        case dte: DocTemplateEntity => (None, dte)
        case me: MemberEntity => (Some(me.signature), me.inTemplate)
        case tpl => (None, tpl)
      }
    }: @nowarn("msg=match may not be exhaustive")

    val templatePath = templateToPath(containingTemplate)
    val url = "../" * (thisPage.path.size - 1) + templatePath.reverse.mkString("/")
    url + signature.map("#" + _).getOrElse("")
  }
}
