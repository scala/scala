/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc.doc.html

import scala.tools.nsc.doc.model._
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
    * @param generator The generator that is writing this page. */
  def writeFor(site: HtmlFactory): Unit

  def docEntityKindToString(ety: DocTemplateEntity) =
  	if (ety.isTrait) "trait"
  	else if (ety.isCaseClass) "case class"
  	else if (ety.isClass) "class"
  	else if (ety.isObject) "object"
  	else if (ety.isPackage) "package"
  	else "class"	// FIXME: an entity *should* fall into one of the above categories, but AnyRef is somehow not

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
}
