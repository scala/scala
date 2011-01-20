/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._

import scala.collection._
import scala.xml._
import scala.util.parsing.json.{JSONObject, JSONArray}

class Index(universe: Universe, indexModel: IndexModelFactory#IndexModel) extends HtmlPage {

  def path = List("index.html")

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value else "" ) +
    ( if (!s.docversion.isDefault) (" " + s.docversion.value) else "" )
  }

  val headers =
    <xml:group>
      <link href={ relativeLinkTo{List("index.css", "lib")} }  media="screen" type="text/css" rel="stylesheet"/>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery-ui.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.layout.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("index.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("scheduler.js", "lib")} }></script>
    </xml:group>

  val body =
    <body>
      <div id="library">
        <img class='class icon' width="13" height="13" src={ relativeLinkTo{List("class.png", "lib")} }/>
        <img class='trait icon' width="13" height="13" src={ relativeLinkTo{List("trait.png", "lib")} }/>
        <img class='object icon' width="13" height="13" src={ relativeLinkTo{List("object.png", "lib")} }/>
        <img class='package icon' width="13" height="13" src={ relativeLinkTo{List("package.png", "lib")} }/>
      </div>
      { browser }
      <div id="content" class="ui-layout-center">
        <iframe name="template" src={ relativeLinkTo{List("package.html")} }/>
      </div>
    </body>


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

  def browser =
	<xml:group>
    <div id="browser" class="ui-layout-west">
      <div class="ui-west-north">{
        <div class="letters">
	      { for(l <- indexModel.keySet.toList.sortBy( _.toString )) yield { // TODO there should be a better way to do that
	          val ch = if(l=='#') "%23" else l // url encoding if needed
              <a target="template" href={"index/index-"+ch+".html"}>{l.toUpper}</a> ++ xml.Text(" ")
          } }
	    </div>
      }</div>
      <div class="ui-west-center">
      <div id="filter"></div>
      <div class="pack" id="tpl">{
        def packageElem(pack: model.Package): NodeSeq = {
          <xml:group>
            { if (!pack.isRootPackage)
                <h3><a class="tplshow" href={ relativeLinkTo(pack) }>{ pack.qualifiedName }</a></h3>
              else NodeSeq.Empty
            }
            <ol class="templates">{
              val tpls: Map[String, Seq[DocTemplateEntity]] =
                (pack.templates filter (t => !t.isPackage && !isExcluded(t) )) groupBy (_.name)

              val placeholderSeq: NodeSeq = <div class="placeholder"></div>

              def createLink(entity: DocTemplateEntity, includePlaceholder: Boolean, includeText: Boolean) = {
                val entityType = docEntityKindToString(entity)
                val linkContent = (
                  { if (includePlaceholder) placeholderSeq else NodeSeq.Empty }
                  ++
                  { if (includeText) <span class="tplLink">{ Text(packageQualifiedName(entity)) }</span> else NodeSeq.Empty }
                )
                <a class="tplshow" href={ relativeLinkTo(entity) }><span class={ entityType }>({ Text(entityType) })</span>{ linkContent }</a>
              }

              for (tn <- tpls.keySet.toSeq sortBy (_.toLowerCase)) yield {
                val entities = tpls(tn)
                val row = (entities find (e => e.isPackage || e.isObject), entities find (e => e.isTrait || e.isClass))

                val itemContents = row match {
                  case (Some(obj), None) => createLink(obj, includePlaceholder = true, includeText = true)

                  case (maybeObj, Some(template)) =>
                    val firstLink = maybeObj match {
                      case Some(obj) => createLink(obj, includePlaceholder = false, includeText = false)
                      case None => placeholderSeq
                    }

                    firstLink ++ createLink(template, includePlaceholder = false, includeText = true)

                  case _ => // FIXME: this default case should not be necessary. For some reason AnyRef is not a package, object, trait, or class
                    val entry = entities.head
                    placeholderSeq ++ createLink(entry, includePlaceholder = false, includeText = true)
                }

                <li title={ entities.head.qualifiedName }>{ itemContents }</li>
              }
            }</ol>
            <ol class="packages"> {
              for (sp <- pack.packages sortBy (_.name.toLowerCase)) yield
                <li id={
                  "package-" + toId(sp.qualifiedName)
                } class="pack" title={ sp.qualifiedName }>{ packageElem(sp) }</li>
            }</ol>
          </xml:group>
        }
        packageElem(universe.rootPackage)
      }</div></div>{ scriptElement }
    </div>
    </xml:group>

  def toId(str: String) = {
    val pattern = "[^A-Za-z0-9-]".r
    pattern.replaceSomeIn(str, (m: scala.util.matching.Regex.Match) => {
      Some("-" + m.group(0).charAt(0).toInt)
    })
  }

  def mergeByQualifiedName(source: List[DocTemplateEntity]): Map[String, List[DocTemplateEntity]]= {
    var result = Map[String, List[DocTemplateEntity]]()

    for (t <- source) {
      val k = t.qualifiedName
      result += k -> (result.getOrElse(k, List()) :+ t)
    }

    result
  }

  def scriptElement = {
    val packages = allPackagesWithTemplates.toIterable.map(_ match {
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

    val obj =
      JSONObject(scala.collection.immutable.Map(packages : _*)).toString()

    <script type="text/javascript">
      Index.PACKAGES = {scala.xml.Unparsed(obj)};
    </script>
  }

  def allPackagesWithTemplates: Map[Package, List[DocTemplateEntity]] = {
    Map(allPackages.map((key) => {
      key -> key.templates.filter(t => !t.isPackage && !isExcluded(t))
    }) : _*)
  }

  def allPackages: List[Package] = {
    def f(parent: Package): List[Package] = {
      parent.packages.flatMap(
        p => f(p) :+ p
      )
    }
    f(universe.rootPackage).sortBy(_.toString)
  }

  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
