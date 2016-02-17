/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda, Felix Mulder
 */

package scala.tools.nsc
package doc
package html
package page

import model._
import scala.collection._
import scala.xml._

class Index(universe: doc.Universe, val index: doc.Index, rep: ScalaDocReporter) extends HtmlPage {

  def reporter = rep

  def path = List("index.html")

  def title = ""

  val headers =
    <xml:group>
      <link href={ relativeLinkTo{List("index.css", "lib")} }  media="screen" type="text/css" rel="stylesheet"/>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
      <script type="text/javascript" src={ relativeLinkTo{List("index.js", "lib")} }></script>
      <script type="text/javascript" src="index.js"></script>
      <script type="text/javascript" src={ relativeLinkTo{List("scheduler.js", "lib")} }></script>
    </xml:group>

  val body =
    <body>
      { search }
      <div id="search-results">
        <div id="results-content">
          <div id="entity-results"></div>
          <div id="member-results"></div>
        </div>
      </div>
      <div id="content" style="-webkit-overflow-scrolling: touch;">
        <iframe id="template" name="template" src={ relativeLinkTo{List("package.html")} }/>
      </div>
    </body>

  def search =
    <div id="search">
        <span id="doc-title">
          {universe.settings.doctitle.value}
          <span id="doc-version">
          {
            val version = universe.settings.docversion.value

            if (version.length > "XX.XX.XX-XXX".length) {
              reporter.warning(null,
                s"doc-version ($version) is too long to be displayed in the webview")
              ""
            } else version
          }
          </span>
        </span>
        <span class="close-results"><span class="left">&lt;</span> Back</span>
        <div id="textfilter">
          <span class="input">
            <input autocapitalize="none" placeholder="Search" id="index-input" type="text" accesskey="/"/>
            <span class="clear">✖</span>
          </span>
        </div>
    </div>

  def packageQualifiedName(ety: DocTemplateEntity): String =
    if (ety.inTemplate.isPackage) ety.name
    else (packageQualifiedName(ety.inTemplate) + "." + ety.name)

}
