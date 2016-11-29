/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala
package tools.nsc.doc.html

import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base.comment
import java.io.{FileOutputStream, File}
import scala.reflect.NameTransformer
import java.nio.channels.Channels
import java.io.Writer

abstract class Page {
  thisPage =>

  /** The path of this page, relative to the API site. `path.tail` is a list
    * of folder names leading to this page (from closest package to
    * one-above-root package), `path.head` is the file name of this page.
    * Note that `path` has a length of at least one. */
  def path: List[String]

  def absoluteLinkTo(path: List[String]) = path.reverse.mkString("/")

  def createFileOutputStream(site: HtmlFactory, suffix: String = "") = {
    val file = new File(site.siteRoot, absoluteLinkTo(thisPage.path) + suffix)
    val folder = file.getParentFile
    if (! folder.exists) {
      folder.mkdirs
    }
    new FileOutputStream(file.getPath)
  }

  def writeFile(site: HtmlFactory, suffix: String = "")(fn: Writer => Unit) = {
    val fos = createFileOutputStream(site, suffix)
    val w = Channels.newWriter(fos.getChannel, site.encoding)
    try {
      fn(w)
    }
    finally {
      w.close()
      fos.close()
    }
  }

  /** Writes this page as a file. The file's location is relative to the
    * generator's site root, and the encoding is also defined by the generator.
    * @param site The generator that is writing this page. */
  def writeFor(site: HtmlFactory): Unit

  def kindToString(mbr: MemberEntity) =
    mbr match {
      case c: Class => if (c.isCaseClass) "case class" else "class"
      case _: Trait => "trait"
      case _: Package => "package"
      case _: Object => "object"
      case _: AbstractType => "type"
      case _: AliasType => "type"
      case _: Constructor => "new"
      case v: Def => "def"
      case v: Val if (v.isLazyVal) => "lazy val"
      case v: Val if (v.isVal) => "val"
      case v: Val if (v.isVar) => "var"
      case _ => sys.error("Cannot create kind for: " + mbr + " of class " + mbr.getClass)
    }

  def templateToPath(tpl: TemplateEntity): List[String] = {
    def doName(tpl: TemplateEntity): String =
      (if (tpl.inPackageObject) "package$$" else "") + NameTransformer.encode(tpl.name) + (if (tpl.isObject) "$" else "")
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
        case p: Package => ("index.html", p)
        case _ => downInner(doName(tpl), tpl)
      }
    file :: downPacks(pack)
  }

  /** A relative link from this page to some destination class entity.
    * @param destClass The class or object entity that the link will point to. */
  def relativeLinkTo(destClass: TemplateEntity): String =
    relativeLinkTo(templateToPath(destClass))

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

  def hasCompanion(mbr: TemplateEntity): Boolean = mbr match {
    case dtpl: DocTemplateEntity => dtpl.companion.isDefined
    case _ => false
  }

  protected def inlineToStr(inl: comment.Inline): String = inl match {
    case comment.Chain(items) => items flatMap (inlineToStr(_)) mkString ""
    case comment.Italic(in) => inlineToStr(in)
    case comment.Bold(in) => inlineToStr(in)
    case comment.Underline(in) => inlineToStr(in)
    case comment.Monospace(in) => inlineToStr(in)
    case comment.Text(text) => text
    case comment.Summary(in) => inlineToStr(in)
    case comment.EntityLink(comment.Text(text), _) => text
    case _ => inl.toString
  }
}
