/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
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
import scala.reflect.NameTransformer
import java.nio.channels.Channels
import java.io.{FileOutputStream, File}

/** An html page that is part of a Scaladoc site.
  * @author David Bernard
  * @author Gilles Dubochet */
abstract class HtmlPage { thisPage =>

  /** The path of this page, relative to the API site. `path.tail` is a list of folder names leading to this page (from
    * closest package to one-above-root package), `path.head` is the file name of this page. Note that `path` has a
    * length of at least one. */
  def path: List[String]

  /** The title of this page. */
  protected def title: String

  /** Additional header elements (links, scripts, meta tags, etc.) required for this page. */
  protected def headers: NodeSeq

  /** The body of this page. */
  protected def body: NodeSeq

  /** Writes this page as a file. The file's location is relative to the generator's site root, and the encoding is
    * also defined by the generator.
    * @param generator The generator that is writing this page. */
  def writeFor(site: HtmlFactory): Unit = {
    val doctype =
      DocType("html", PublicID("-//W3C//DTD XHTML 1.1//EN", "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"), Nil)
    val html =
      <html>
        <head>
          <title>{ title }</title>
          <meta http-equiv="content-type" content={ "text/html; charset=" + site.encoding }/>
		      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
          { headers }
        </head>
        { body }
      </html>
    val pageFile = new File(site.siteRoot, absoluteLinkTo(thisPage.path))
    val pageFolder = pageFile.getParentFile
    if (!pageFolder.exists) pageFolder.mkdirs()
    val fos = new FileOutputStream(pageFile.getPath)
    val w = Channels.newWriter(fos.getChannel, site.encoding)
    try {
      w.write("<?xml version='1.0' encoding='" + site.encoding + "'?>\n")
      w.write( doctype.toString + "\n")
      w.write(xml.Xhtml.toXhtml(html))
    }
    finally {
      w.close()
      fos.close()
    }
    //XML.save(pageFile.getPath, html, site.encoding, xmlDecl = false, doctype = doctype)
  }

  def templateToPath(tpl: TemplateEntity): List[String] = {
    def doName(tpl: TemplateEntity): String =
      NameTransformer.encode(tpl.name) + (if (tpl.isObject) "$" else "")
    def downPacks(pack: Package): List[String] =
      if (pack.isRootPackage) Nil else (doName(pack) :: downPacks(pack.inTemplate))
    def downInner(nme: String, tpl: TemplateEntity): (String, Package) = {
      tpl.inTemplate match {
        case inPkg: Package => (nme + ".html", inPkg)
        case inTpl => downInner(doName(inTpl) + "$" + nme, inTpl)
      }
    }
    val (file, pack) =
      tpl match {
        case p: Package => ("package.html", p)
        case _ => downInner(doName(tpl), tpl)
      }
    file :: downPacks(pack)
  }

  /** A relative link from this page to some destination class entity.
    * @param destEntity The class or object entity that the link will point to. */
  def relativeLinkTo(destClass: TemplateEntity): String =
    relativeLinkTo(templateToPath(destClass))

  /** A relative link from this page to some destination page in the Scaladoc site.
    * @param destPage The page that the link will point to. */
  def relativeLinkTo(destPage: HtmlPage): String = {
    relativeLinkTo(destPage.path)
  }

  /** A relative link from this page to some destination path.
    * @param destPath The path that the link will point to. */
  def relativeLinkTo(destPath: List[String]): String = {
    def relativize(from: List[String], to: List[String]): List[String] = (from, to) match {
      case (f :: fs, t :: ts) if (f == t) => // both paths are identical to that point
        relativize(fs, ts)
      case (fss, tss) =>
        List.fill(fss.length - 1)("..") ::: tss
    }
    relativize(thisPage.path.reverse, destPath.reverse).mkString("/")
  }

  def absoluteLinkTo(destPath: List[String]): String = {
    destPath.reverse.mkString("/")
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
    case Code(data) => <pre>{ xml.Text(data) }</pre>
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
    case EntityLink(entity) => templateToHtml(entity)
    case Monospace(text) => <code>{ xml.Text(text) }</code>
    case Text(text) => xml.Text(text)
    case Summary(in) => inlineToHtml(in)
    case HtmlTag(tag) => xml.Unparsed(tag)
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

  /** Returns the HTML code that represents the template in `tpl` as a hyperlinked name. */
  def templateToHtml(tpl: TemplateEntity) = tpl match {
    case dTpl: DocTemplateEntity =>
      <a href={ relativeLinkTo(dTpl) } class="extype" name={ dTpl.qualifiedName }>{ dTpl.name }</a>
    case ndTpl: NoDocTemplate =>
      xml.Text(ndTpl.name)
  }

  /** Returns the HTML code that represents the templates in `tpls` as a list of hyperlinked names. */
  def templatesToHtml(tplss: List[TemplateEntity], sep: NodeSeq): NodeSeq = tplss match {
    case Nil         => NodeSeq.Empty
    case tpl :: Nil  => templateToHtml(tpl)
    case tpl :: tpls => templateToHtml(tpl) ++ sep ++ templatesToHtml(tpls, sep)
  }

  def docEntityKindToString(ety: DocTemplateEntity) =
  	if (ety.isTrait) "trait"
  	else if (ety.isClass) "class"
  	else if (ety.isObject) "object"
  	else if (ety.isPackage) "package"
  	else "class"	// FIXME: an entity *should* fall into one of the above categories, but AnyRef is somehow not
}
