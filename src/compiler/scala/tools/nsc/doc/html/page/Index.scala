/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._

import scala.collection._
import scala.xml._

class Index(universe: Universe) extends HtmlPage {

  def path = List("index.html")

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value else "" ) +
    ( if (!s.docversion.isDefault) (" " + s.docversion.value) else "" )
  }

  def headers =
    <xml:group>
      <link href={ relativeLinkTo(List("index.css", "lib")) }  media="screen" type="text/css" rel="stylesheet"/>
      <script type="text/javascript" src={ relativeLinkTo{List("index.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("scheduler.js", "lib")} }></script>
    </xml:group>

  def body =
    <body>
      <div id="library">
        <img class='class icon' src='lib/class.png'/>
        <img class='trait icon' src='lib/trait.png'/>
        <img class='object icon' src='lib/object.png'/>
        <img class='package icon' src='lib/package.png'/>
      </div>
      <div id="browser">
        <div id="filter"></div>
        <div class="pack" id="tpl">{
          def isExcluded(dtpl: DocTemplateEntity) = {
            val qname = dtpl.qualifiedName
            (qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") || qname.startsWith("scala.Function")) &&
              !(qname=="scala.Function1" || qname=="scala.Function2" || qname=="scala.Function" ||
                      qname=="scala.Product1" || qname=="scala.Product2" || qname=="scala.Product" ||
                      qname=="scala.Tuple1" || qname=="scala.Tuple2")
          }
          def packageElem(pack: model.Package): NodeSeq = {
            <xml:group>
              { if (!pack.isRootPackage)
                  <h3><a class="tplshow" href={ relativeLinkTo(pack) }>{ pack.qualifiedName }</a></h3>
                else NodeSeq.Empty
              }
              <ol class="templates">{
                val tpls: Map[String, Seq[DocTemplateEntity]] =
                  (pack.templates filter (t => !t.isPackage && !isExcluded(t) )) groupBy (_.name)
                for (tn <- tpls.keySet.toSeq sortBy (_.toLowerCase)) yield {
                  val entries = tpls(tn) sortWith { (less, more) => less.isTrait || more.isObject }
                  def doEntry(ety: DocTemplateEntity, firstEty: Boolean): NodeSeq = {
                    val etyTpe =
                      if (ety.isTrait) "trait" else if (ety.isClass) "class" else if (ety.isObject) "object" else "package"
                    <a class="tplshow" href={ relativeLinkTo(ety) }>
                      { if (firstEty) Text(packageQualifiedName(ety)) else NodeSeq.Empty }
                      <span class={ etyTpe }>({ Text(etyTpe) })</span>
                    </a>
                  }
                  <li title={ entries.head.qualifiedName }>{
                    doEntry(entries.head, true) ++ (entries.tail map (doEntry(_, false)))
                  }</li>
                }
              }</ol>
              <ol class="packages"> {
                for (sp <- pack.packages sortBy (_.name.toLowerCase)) yield
                  <li class="pack" title={ sp.qualifiedName }>{ packageElem(sp) }</li>
              }</ol>
            </xml:group>
          }
          packageElem(universe.rootPackage)
        }</div>
      </div>
		  <div id="content">
		  	<iframe name="template" src={ relativeLinkTo{List("package.html")} }/>
		  </div>
    </body>


  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
