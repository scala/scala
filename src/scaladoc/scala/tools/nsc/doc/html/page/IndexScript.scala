/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc.doc.html.page

import scala.tools.nsc.doc
import scala.tools.nsc.doc.model.{Package, DocTemplateEntity}
import scala.tools.nsc.doc.html.{Page, HtmlFactory}
import scala.util.parsing.json.{JSONObject, JSONArray}

class IndexScript(universe: doc.Universe, index: doc.Index) extends Page {
  def path = List("index.js")

  override def writeFor(site: HtmlFactory) {
    writeFile(site) {
      _.write("Index.PACKAGES = " + packages.toString() + ";")
    }
  }

  val packages = {
    val pairs = allPackagesWithTemplates.toIterable.map(_ match {
      case (pack, templates) => {
        val merged = mergeByQualifiedName(templates)

        val ary = merged.keys.toList.sortBy(_.toLowerCase).map(key => {
          val pairs = merged(key).map(
            t => kindToString(t) -> relativeLinkTo(t)
          ) :+ ("name" -> key)

          JSONObject(scala.collection.immutable.Map(pairs : _*))
        })

        pack.qualifiedName -> JSONArray(ary)
      }
    }).toSeq

    JSONObject(scala.collection.immutable.Map(pairs : _*))
  }

  def mergeByQualifiedName(source: List[DocTemplateEntity]) = {
    var result = Map[String, List[DocTemplateEntity]]()

    for (t <- source) {
      val k = t.qualifiedName
      result += k -> (result.getOrElse(k, List()) :+ t)
    }

    result
  }

  def allPackages = {
    def f(parent: Package): List[Package] = {
      parent.packages.flatMap(
        p => f(p) :+ p
      )
    }
    f(universe.rootPackage).sortBy(_.toString)
  }

  def allPackagesWithTemplates = {
    Map(allPackages.map((key) => {
      key -> key.templates.collect {
        case t: DocTemplateEntity if !t.isPackage && !universe.settings.hardcoded.isExcluded(t.qualifiedName) => t
      }
    }) : _*)
  }
}
