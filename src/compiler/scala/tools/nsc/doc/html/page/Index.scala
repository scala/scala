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
          def isExcluded(t : DocTemplateEntity) = {
            val qname=t.qualifiedName
            (qname.startsWith("scala.Tuple") || qname.startsWith("scala.Product") ||
            qname.startsWith("scala.Function")) &&
            !(qname=="scala.Function1" || qname=="scala.Function2" || qname=="scala.Function"
             || qname=="scala.Product1" || qname=="scala.Product2" || qname=="scala.Product"
             || qname=="scala.Tuple1" || qname=="scala.Tuple2")
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
