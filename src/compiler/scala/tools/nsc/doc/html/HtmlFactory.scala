/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import model._
import java.io.{ File => JFile }
import io.{ Streamable, Directory }
import scala.collection._

/** A class that can generate Scaladoc sites to some fixed root folder.
  * @author David Bernard
  * @author Gilles Dubochet */
class HtmlFactory(val universe: doc.Universe, index: doc.Index) {
  /** The character encoding to be used for generated Scaladoc sites. This value is currently always UTF-8. */
  def encoding: String = "UTF-8"

  def siteRoot: JFile = new JFile(universe.settings.outdir.value)

  /** Generates the Scaladoc site for a model into the site root. A scaladoc site is a set of HTML and related files
    * that document a model extracted from a compiler run.
    * @param model The model to generate in the form of a sequence of packages. */
  def generate() {

    def copyResource(subPath: String) {
      val bytes = new Streamable.Bytes {
        val inputStream = getClass.getResourceAsStream("/scala/tools/nsc/doc/html/resource/" + subPath)
        assert(inputStream != null)
      }.toByteArray
      val dest = Directory(siteRoot) / subPath
      dest.parent.createDirectory()
      val out = dest.toFile.bufferedOutput()
      try out.write(bytes, 0, bytes.length)
      finally out.close()
    }

    copyResource("lib/jquery.js")
    copyResource("lib/jquery-ui.js")
    copyResource("lib/jquery.layout.js")
    copyResource("lib/tools.tooltip.js")
    copyResource("lib/scheduler.js")
    copyResource("lib/index.js")
    copyResource("lib/template.js")

    copyResource("lib/index.css")
    copyResource("lib/ref-index.css")
    copyResource("lib/template.css")

    copyResource("lib/class.png")
    copyResource("lib/class_big.png")
    copyResource("lib/object.png")
    copyResource("lib/object_big.png")
    copyResource("lib/trait.png")
    copyResource("lib/trait_big.png")
    copyResource("lib/package.png")
    copyResource("lib/package_big.png")

    copyResource("lib/arrow-down.png")
    copyResource("lib/arrow-right.png")
    copyResource("lib/filter_box_left.png")
    copyResource("lib/filter_box_right.png")
    copyResource("lib/filter_box_left2.gif")
    copyResource("lib/filterbg.gif")
    copyResource("lib/filterboxbarbg.gif")
    copyResource("lib/filterboxbg.gif")

    copyResource("lib/constructorsbg.gif")
    copyResource("lib/defbg-blue.gif")
    copyResource("lib/defbg-green.gif")
    copyResource("lib/fullcommenttopbg.gif")
    copyResource("lib/ownderbg2.gif")
    copyResource("lib/ownerbg.gif")
    copyResource("lib/ownerbg2.gif")
    copyResource("lib/signaturebg.gif")
    copyResource("lib/signaturebg2.gif")
    copyResource("lib/packagesbg.gif")
    copyResource("lib/typebg.gif")
    copyResource("lib/valuemembersbg.gif")
    copyResource("lib/filterboxbarbg.png")

    copyResource("lib/remove.png")
    copyResource("lib/navigation-li-a.png")
    copyResource("lib/navigation-li.png")
    copyResource("lib/selected-right.png")
    copyResource("lib/selected.png")
    copyResource("lib/selected2-right.png")
    copyResource("lib/selected2.png")
    copyResource("lib/unselected.png")

    new page.Index(universe, index) writeFor this

    writeTemplates(page => page.writeFor(this))

    for(letter <- index.firstLetterIndex) {
      new html.page.ReferenceIndex(letter._1, index, universe) writeFor this
    }
  }

  def writeTemplates(writeForThis: HtmlPage => Unit): Unit = {
    val written = mutable.HashSet.empty[DocTemplateEntity]

    def writeTemplate(tpl: DocTemplateEntity): Unit =
      if (!(written contains tpl)) {
        writeForThis(new page.Template(tpl))
        written += tpl
        tpl.templates map (writeTemplate(_))
      }

    writeTemplate(universe.rootPackage)
  }

}
