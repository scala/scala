package scala.tools.nsc.doc.html
import scala.tools.nsc.doc.model._
import java.io.{FileOutputStream, File}
import scala.reflect.NameTransformer

abstract class Page {
  thisPage =>

  /** The path of this page, relative to the API site. `path.tail` is a list of folder names leading to this page (from
    * closest package to one-above-root package), `path.head` is the file name of this page. Note that `path` has a
    * length of at least one. */
  def path: List[String]

  def absoluteLinkTo(path: List[String]) = path.reverse.mkString("/")

  def createFileOutputStream(site: HtmlFactory) = {
    val file = new File(site.siteRoot, absoluteLinkTo(thisPage.path))
    val folder = file.getParentFile
    if (! folder.exists) {
      folder.mkdirs
    }
    new FileOutputStream(file.getPath)
  }

  /** Writes this page as a file. The file's location is relative to the generator's site root, and the encoding is
    * also defined by the generator.
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

  def isExcluded(dtpl: DocTemplateEntity) = {
    val qname = dtpl.qualifiedName
    ( ( qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") ||
       qname.startsWith("scala.Function") || qname.startsWith("scala.runtime.AbstractFunction")
     ) && !(
      qname == "scala.Tuple1" || qname == "scala.Tuple2" ||
      qname == "scala.Product" || qname == "scala.Product1" || qname == "scala.Product2" ||
      qname == "scala.Function" || qname == "scala.Function1" || qname == "scala.Function2" ||
      qname == "scala.runtime.AbstractFunction0" || qname == "scala.runtime.AbstractFunction1" ||
      qname == "scala.runtime.AbstractFunction2"
    )
   )
  }
}
