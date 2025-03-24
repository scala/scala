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
package tools.nsc.doc.html

import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.base.comment._
import java.io.{FileOutputStream, File}
import scala.reflect.NameTransformer
import java.nio.channels.Channels
import java.io.Writer

object Page {
  def inlineToStr(inl: Inline): String = inl match {
    case Chain(items) => items flatMap (inlineToStr(_)) mkString ""
    case Italic(in) => inlineToStr(in)
    case Bold(in) => inlineToStr(in)
    case Underline(in) => inlineToStr(in)
    case Superscript(in) => inlineToStr(in)
    case Subscript(in) => inlineToStr(in)
    case Link(raw, title) => inlineToStr(title)
    case Monospace(in) => inlineToStr(in)
    case Text(text) => text
    case Summary(in) => inlineToStr(in)
    case HtmlTag(tag) => "<[^>]*>".r.replaceAllIn(tag, "")
    case EntityLink(in, _) => inlineToStr(in)
  }

  def inlineToStrForTitleTag(inl: Inline): String =
    inlineToStr(inl).split("\n").map(_.trim).mkString(" ")

  def templateToPath(tpl: TemplateEntity): List[String] = {
    def doName(tpl: TemplateEntity): String =
      (if (tpl.inPackageObject) "package$$" else "") + NameTransformer.encode(tpl.name) + (if (tpl.isObject) "$" else "")
    def downPacks(pack: TemplateEntity): List[String] =
      if (pack.isRootPackage) Nil else (doName(pack) :: downPacks(pack.inTemplate))
    def downInner(nme: String, tpl: TemplateEntity): (String, TemplateEntity) = {
      if(tpl.inTemplate.isPackage) {
        (nme + ".html", tpl.inTemplate)
      } else {
        downInner(doName(tpl.inTemplate) + "$" + nme, tpl.inTemplate)
      }
    }
    val (file, pack) =
      if(tpl.isPackage) {
        ("index.html", tpl)
      } else {
        downInner(doName(tpl), tpl)
      }
    file :: downPacks(pack)
  }

  def makeUrl(baseUrl : String, path : List[String]) : String = baseUrl.stripSuffix("/") + "/" + path.reverse.mkString("/")

  /** A relative link from a page's path to some destination path.
    * @param destPath The path that the link will point to. */
  def relativeLinkTo(thisPagePath: List[String], destPath: List[String]): String = {
    def relativize(from: List[String], to: List[String]): List[String] = (from, to) match {
      case (f :: fs, t :: ts) if (f == t) => // both paths are identical to that point
        relativize(fs, ts)
      case (fss, tss) =>
        List.fill(fss.length - 1)("..") ::: tss
    }
    relativize(thisPagePath.reverse, destPath.reverse).mkString("/")
  }
}

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
      case c: Class => if (c.isCase) "case class" else "class"
      case _: Trait => "trait"
      case _: Package => "package"
      case o: Object => if (o.isCase) "case object" else "object"
      case _: AbstractType => "type"
      case _: AliasType => "type"
      case _: Constructor => "new"
      case v: Def => "def"
      case v: Val if (v.isLazyVal) => "lazy val"
      case v: Val if (v.isVal) => "val"
      case v: Val if (v.isVar) => "var"
      case _ => throw new IllegalArgumentException(s"Cannot create kind for: $mbr of class ${mbr.getClass}")
    }

  def templateToPath(tpl: TemplateEntity): List[String] = Page.templateToPath(tpl)

  /** A relative link from this page to some destination class entity.
    * @param destClass The class or object entity that the link will point to. */
  def relativeLinkTo(destClass: TemplateEntity): String =
    Page.relativeLinkTo(thisPage.path, templateToPath(destClass))

  /** A relative link from a page's path to some destination path.
    * @param destPath The path that the link will point to. */
  def relativeLinkTo(destPath: List[String]): String =
    Page.relativeLinkTo(thisPage.path, destPath)

  def hasCompanion(mbr: TemplateEntity): Boolean = mbr match {
    case dtpl: DocTemplateEntity => dtpl.companion.isDefined
    case _ => false
  }
}
