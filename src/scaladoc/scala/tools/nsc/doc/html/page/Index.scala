/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._
import scala.collection._
import scala.xml._

class Index(universe: doc.Universe, val index: doc.Index) extends HtmlPage {

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
        <img class='class icon' alt='class icon' src={ relativeLinkTo{List("class.png", "lib")} }/>
        <img class='trait icon' alt='trait icon' src={ relativeLinkTo{List("trait.png", "lib")} }/>
        <img class='object icon' alt='trait icon' src={ relativeLinkTo{List("object.png", "lib")} }/>
        <img class='package icon' alt='trait icon' src={ relativeLinkTo{List("package.png", "lib")} }/>
      </div>
      { browser }
      <div id="content" class="ui-layout-center">
        <iframe id="template" name="template" src={ relativeLinkTo{List("package.html")} }/>
      </div>
    </body>

  def letters: NodeSeq =
    '_' +: ('a' to 'z') map {
      char => {
        val label = if (char == '_') '#' else char.toUpper

        index.firstLetterIndex.get(char) match {
          case Some(_) =>
            <a target="template" href={ "index/index-" + char + ".html" }>{
              label
            }</a>
          case None => <span>{ label }</span>
        }
      }
    }

  def deprecated: NodeSeq = if (index.hasDeprecatedMembers)
      <a target="template" href="deprecated-list.html">deprecated</a>
    else
      <span>deprecated</span>

  def browser =
    <div id="browser" class="ui-layout-west">
      <div class="ui-west-center">
      <div id="filter">
          <div id="textfilter"></div>
          <div id="letters">{ letters } &#8211; { deprecated }</div>
      </div>
      <div class="pack" id="tpl">{
        def packageElem(pack: model.Package): NodeSeq = {
          <xml:group>
            { if (!pack.isRootPackage)
                <a class="tplshow" href={ relativeLinkTo(pack) } target="template">{ pack.qualifiedName }</a>
              else NodeSeq.Empty
            }
            <ol class="templates">{
              val tpls: Map[String, Seq[DocTemplateEntity]] =
                (pack.templates collect {
                  case t: DocTemplateEntity if !t.isPackage && !universe.settings.hardcoded.isExcluded(t.qualifiedName) => t
                }) groupBy (_.name)

              val placeholderSeq: NodeSeq = <div class="placeholder"></div>

              def createLink(entity: DocTemplateEntity, includePlaceholder: Boolean, includeText: Boolean) = {
                val entityType = kindToString(entity)
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
