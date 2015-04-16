/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala
package tools
package nsc
package doc
package html
package page

import doc.model._

class DeprecatedIndex(universe: Universe, index: doc.Index) extends HtmlPage {

  def path = List("deprecated-list.html")

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


  private def entry(name: String, methods: Iterable[MemberEntity]) = {
    val occurrences = methods.filter(_.deprecation.isDefined).map(method =>
      templateToHtml(method.inDefinitionTemplates.head)
    ).toList.distinct

    <div class="entry">
      <div class="name">{ name }</div>
      <div class="occurrences">{
        for (owner <- occurrences) yield owner ++ scala.xml.Text(" ")
      }</div>
    </div>
  }

  def deprecatedEntries = {
    val available =  ('_' +: ('a' to 'z')).flatMap(index.firstLetterIndex.get)

    for (group <- available;
         value <- group if value._2.find(_.deprecation.isDefined).isDefined)
       yield value
  }

  def body =
    <body>{
      for(value <- deprecatedEntries) yield
        entry(value._1, value._2.view)
    }</body>

}
