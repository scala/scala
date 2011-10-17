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

class Index(universe: doc.Universe, index: doc.Index) extends HtmlPage {

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
        <img class='class icon' src={ relativeLinkTo{List("class.png", "lib")} }/>
        <img class='trait icon' src={ relativeLinkTo{List("trait.png", "lib")} }/>
        <img class='object icon' src={ relativeLinkTo{List("object.png", "lib")} }/>
        <img class='package icon' src={ relativeLinkTo{List("package.png", "lib")} }/>
      </div>
      { browser }
      <div id="content" class="ui-layout-center">
        <iframe name="template" src={ relativeLinkTo{List("package.html")} }/>
      </div>
    </body>

  def browser =
    <div id="browser" class="ui-layout-west">
      <div class="ui-west-center">
      <div id="filter"></div>
      <div class="pack" id="tpl">{
        def packageElem(pack: model.Package): NodeSeq = {
          <xml:group>
            { if (!pack.isRootPackage)
                <a class="tplshow" href={ relativeLinkTo(pack) } target="template">{ pack.qualifiedName }</a>
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
                <a class="tplshow" href={ relativeLinkTo(entity) } target="template"><span class={ entityType }>({ Text(entityType) })</span>{ linkContent }</a>
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
                <li class="pack" title={ sp.qualifiedName }>{ packageElem(sp) }</li>
            }</ol>
          </xml:group>
        }
        packageElem(universe.rootPackage)
      }</div></div><script src="index.js"></script>
    </div>

  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name
    else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
