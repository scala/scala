/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Pedro Furlanetto
 */

package scala.tools.nsc
package doc
package html
package page


class ReferenceIndex(letter: Char, index: doc.Index, universe: Universe) extends HtmlPage {

  def path = List("index-"+letter+".html","index")

  def title = {
    val s = universe.settings
    ( if (!s.doctitle.isDefault) s.doctitle.value else "" ) +
    ( if (!s.docversion.isDefault) (" " + s.docversion.value) else "" )
  }

  def headers =
    <xml:group>
      <link href={ relativeLinkTo(List("ref-index.css", "lib")) }  media="screen" type="text/css" rel="stylesheet"/>
      <script type="text/javascript" src={ relativeLinkTo{List("jquery.js", "lib")} }></script>
    </xml:group>

  def body =
    <body>
      { for(groups <- index.firstLetterIndex(letter)) yield {
      <div class="entry">
        <div class="name">{ groups._1 }</div>
        <div class="occurrences">
          { for(owner <- groups._2.view) yield {
            templateToHtml(owner) ++ xml.Text(" ")
          } }
        </div>
      </div>
       } }
    </body>

}
