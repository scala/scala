/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import model._
import java.io.{ File => JFile }
import io.{ Streamable, Directory }
import scala.collection._
import page.diagram._

/** A class that can generate Scaladoc sites to some fixed root folder.
  * @author David Bernard
  * @author Gilles Dubochet */
class HtmlFactory(val universe: doc.Universe, index: doc.Index) {

  /** The character encoding to be used for generated Scaladoc sites.
    * This value is currently always UTF-8. */
  def encoding: String = "UTF-8"

  def siteRoot: JFile = new JFile(universe.settings.outdir.value)

  def libResources = List(
    "index.js",
    "jquery-ui.js",
    "jquery.js",
    "jquery.layout.js",
    "scheduler.js",
    "diagrams.js",
    "template.js",
    "tools.tooltip.js",
    "modernizr.custom.js",

    "index.css",
    "ref-index.css",
    "template.css",
    "diagrams.css",

    "class.png",
    "class_big.png",
    "class_diagram.png",
    "object.png",
    "object_big.png",
    "object_diagram.png",
    "package.png",
    "package_big.png",
    "trait.png",
    "trait_big.png",
    "trait_diagram.png",
    "type.png",
    "type_big.png",
    "type_diagram.png",

    "class_to_object_big.png",
    "object_to_class_big.png",
    "trait_to_object_big.png",
    "object_to_trait_big.png",
    "type_to_object_big.png",
    "object_to_type_big.png",

    "arrow-down.png",
    "arrow-right.png",
    "filter_box_left.png",
    "filter_box_left2.gif",
    "filter_box_right.png",
    "filterbg.gif",
    "filterboxbarbg.gif",
    "filterboxbg.gif",

    "constructorsbg.gif",
    "defbg-blue.gif",
    "defbg-green.gif",
    "filterboxbarbg.png",
    "fullcommenttopbg.gif",
    "ownderbg2.gif",
    "ownerbg.gif",
    "ownerbg2.gif",
    "packagesbg.gif",
    "signaturebg.gif",
    "signaturebg2.gif",
    "typebg.gif",
    "conversionbg.gif",
    "valuemembersbg.gif",

    "navigation-li-a.png",
    "navigation-li.png",
    "remove.png",
    "selected-right.png",
    "selected.png",
    "selected2-right.png",
    "selected2.png",
    "selected-right-implicits.png",
    "selected-implicits.png",
    "unselected.png",

    "permalink.png"
  )

  /** Generates the Scaladoc site for a model into the site root.
    * A scaladoc site is a set of HTML and related files
    * that document a model extracted from a compiler run.
    */
  def generate() {

    def copyResource(subPath: String) {
      val bytes = new Streamable.Bytes {
        val p = "/scala/tools/nsc/doc/html/resource/" + subPath
        val inputStream = getClass.getResourceAsStream(p)
        assert(inputStream != null, p)
      }.toByteArray()
      val dest = Directory(siteRoot) / subPath
      dest.parent.createDirectory()
      val out = dest.toFile.bufferedOutput()
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    libResources foreach (s => copyResource("lib/" + s))

    new page.Index(universe, index) writeFor this
    new page.IndexScript(universe, index) writeFor this
    if (index.hasDeprecatedMembers)
      new page.DeprecatedIndex(universe, index) writeFor this
    try {
      writeTemplates(_ writeFor this)
      for (letter <- index.firstLetterIndex) {
        new html.page.ReferenceIndex(letter._1, index, universe) writeFor this
      }
    } finally {
      DiagramStats.printStats(universe.settings)
      universe.dotRunner.cleanup()
    }
  }

  def writeTemplates(writeForThis: HtmlPage => Unit) {
    val written = mutable.HashSet.empty[DocTemplateEntity]

    def writeTemplate(tpl: DocTemplateEntity) {
      if (!(written contains tpl)) {
        val diagramGenerator: DiagramGenerator = new DotDiagramGenerator(universe.settings, universe.dotRunner)
        writeForThis(new page.Template(universe, diagramGenerator, tpl))
        written += tpl
        tpl.templates collect { case d: DocTemplateEntity => d } map writeTemplate
      }
    }

    writeTemplate(universe.rootPackage)
  }
}
