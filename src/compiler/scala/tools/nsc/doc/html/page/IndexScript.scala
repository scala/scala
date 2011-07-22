package scala.tools.nsc.doc.html.page
import scala.tools.nsc.doc
import scala.tools.nsc.doc.model.{Package, DocTemplateEntity}
import scala.tools.nsc.doc.html.{Page, HtmlFactory}
import java.nio.channels.Channels
import scala.util.parsing.json.{JSONObject, JSONArray}

class IndexScript(universe: doc.Universe, index: doc.Index) extends Page {
  def path = List("index.js")

  override def writeFor(site: HtmlFactory): Unit = {
    val stream = createFileOutputStream(site)
    val writer = Channels.newWriter(stream.getChannel, site.encoding)
    try {
      writer.write("Index.PACKAGES = " + packages.toString() + ";")
    }
    finally {
      writer.close
      stream.close
    }
  }

  val packages = {
    val pairs = allPackagesWithTemplates.toIterable.map(_ match {
      case (pack, templates) => {
        val merged = mergeByQualifiedName(templates)

        val ary = merged.keys.toList.sortBy(_.toLowerCase).map(key => {
          val pairs = merged(key).map(
            t => docEntityKindToString(t) -> relativeLinkTo(t)
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
      key -> key.templates.filter(t => !t.isPackage && !isExcluded(t))
    }) : _*)
  }
}
