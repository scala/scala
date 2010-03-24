/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  David Bernard, Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package html

import reporters.Reporter
import model._

import java.io.{FileOutputStream, File}
import scala.collection._

/** A class that can generate Scaladoc sites to some fixed root folder.
  * @author David Bernard
  * @author Gilles Dubochet */
class HtmlFactory(val universe: Universe) {

  /** The character encoding to be used for generated Scaladoc sites. This value is currently always UTF-8. */
  def encoding: String = "UTF-8"

  /** The character encoding to be used for generated Scaladoc sites. This value is defined by the generator's
    * settings. */
  def siteRoot: File = new File(universe.settings.outdir.value)

  /** Generates the Scaladoc site for a model into the site toot. A scaladoc site is a set of HTML and related files
    * that document a model extracted from a compiler run.
    * @param model The model to generate in the form of a sequence of packages. */
  def generate(universe: Universe): Unit = {

    def copyResource(subPath: String) {
      val buf = new Array[Byte](1024)
      val in = getClass.getResourceAsStream("/scala/tools/nsc/doc/html/resource/" + subPath)
      assert(in != null)
      val dest = new File(siteRoot, subPath)
      dest.getParentFile.mkdirs()
      val out = new FileOutputStream(dest)
      try {
        var len = 0
        while ({len = in.read(buf); len != -1})
          out.write(buf, 0, len)
      }
      finally {
        in.close()
        out.close()
      }
    }

    copyResource("lib/jquery.js")
    copyResource("lib/tools.tooltip.js")
    copyResource("lib/scheduler.js")
    copyResource("lib/index.css")
    copyResource("lib/index.js")
    copyResource("lib/template.css")
    copyResource("lib/template.js")
    copyResource("lib/class.png")
    copyResource("lib/class_big.png")
    copyResource("lib/object.png")
    copyResource("lib/object_big.png")
    copyResource("lib/trait.png")
    copyResource("lib/trait_big.png")
    copyResource("lib/package.png")
    copyResource("lib/package_big.png")
    copyResource("lib/filter_box_left.png")
    copyResource("lib/filter_box_right.png")
    copyResource("lib/remove.png")

    new page.Index(universe) writeFor this

    val written = mutable.HashSet.empty[DocTemplateEntity]

    def writeTemplate(tpl: DocTemplateEntity): Unit = {
      new page.Template(tpl) writeFor this
      written += tpl
      tpl.templates filter { t => !(written contains t) } map (writeTemplate(_))
    }

    writeTemplate(universe.rootPackage)

  }

}
