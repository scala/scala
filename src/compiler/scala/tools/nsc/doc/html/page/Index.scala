/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html
package page

import model._

import scala.collection._
import scala.xml._

class Index(modelRoot: Package) extends HtmlPage {

  def path = List("index.html")

  def title = "Scaladoc: all classes and objects"

  def headers =
    <xml:group>
		  <style type="text/css">
		    @import url({ relativeLinkTo(List("index.css", "lib")) }) screen;
		  </style>
		  <script type="text/javascript" src={ relativeLinkTo{List("index.js", "lib")} }></script>
    </xml:group>

  def body =
    <body>
      <div id="browser">
        <input id="quickflt" type="text" accesskey="/"/>
        <div id="tpl">{
          def packageElem(pack: model.Package): NodeSeq = {
            <xml:group>
              { if (!pack.isRootPackage)
                  <h3><a class="tplshow" href={ relativeLinkTo(pack) }>{ pack.qualifiedName }</a></h3>
                else NodeSeq.Empty
              }
              <ol class="templates">{
                val tpls: Map[String, Seq[DocTemplateEntity]] =
                  (pack.templates filter (!_.isPackage)) groupBy (_.name)
                for (tn <- tpls.keySet.toSeq sortWith (_.toLowerCase < _.toLowerCase)) yield {
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
                for (sp <- pack.packages sortWith (_.name.toLowerCase < _.name.toLowerCase)) yield
                  <li>{ packageElem(sp) }</li>
              }</ol>
            </xml:group>
          }
          packageElem(modelRoot)
        }</div>
      </div>
		  <div id="content">
		  	<iframe src={ relativeLinkTo{List("package.html")} }/>
		  </div>
    </body>


  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
