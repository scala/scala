/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Pedro Furlanetto
 */

package scala
package tools
package nsc
package doc
package html
package page

import doc.model._

class ReferenceIndex(letter: Char, index: doc.Index, universe: Universe) extends HtmlPage {

  def path = List("index-"+letter+".html", "index")

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
    val occurrences = methods.map(method => {
      val html = templateToHtml(method.inDefinitionTemplates.head)
      if (method.deprecation.isDefined) {
        <strike>{ html }</strike>
      } else {
        html
      }
    }).toList.distinct

    <div class="entry">
      <div class="name">{
        if (methods.find { ! _.deprecation.isDefined } != None)
          name
        else
          <strike>{ name }</strike>
      }</div>
      <div class="occurrences">{
        for (owner <- occurrences) yield owner ++ scala.xml.Text(" ")
      }</div>
    </div>
  }

  def body =
    <body>{
      for(groups <- index.firstLetterIndex(letter)) yield
        entry(groups._1, groups._2.view)
    }</body>

}
