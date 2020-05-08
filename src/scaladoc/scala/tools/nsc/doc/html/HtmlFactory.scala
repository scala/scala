/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package doc
package html

import model._
import java.io.{ File => JFile }
import io.{ Streamable, Directory }
import scala.collection._
import page.diagram._
import scala.reflect.internal.Reporter

/** A class that can generate Scaladoc sites to some fixed root folder.
  * @author David Bernard
  * @author Gilles Dubochet */
class HtmlFactory(val universe: doc.Universe, val reporter: Reporter) {
  import page.IndexScript

  /** The character encoding to be used for generated Scaladoc sites.
    * This value is currently always UTF-8. */
  def encoding: String = "UTF-8"

  def siteRoot: JFile = new JFile(universe.settings.outdir.value)

  def libResources = List(
    "class.svg",
    "object.svg",
    "trait.svg",
    "package.svg",
    "class_comp.svg",
    "object_comp.svg",
    "trait_comp.svg",
    "object_comp_trait.svg",
    "abstract_type.svg",
    "lato-v11-latin-100.eot",
    "lato-v11-latin-100.ttf",
    "lato-v11-latin-100.woff",
    "lato-v11-latin-regular.eot",
    "lato-v11-latin-regular.ttf",
    "lato-v11-latin-regular.woff",
    "open-sans-v13-latin-regular.eot",
    "open-sans-v13-latin-regular.ttf",
    "open-sans-v13-latin-regular.woff",
    "open-sans-v13-latin-400i.eot",
    "open-sans-v13-latin-400i.ttf",
    "open-sans-v13-latin-400i.woff",
    "open-sans-v13-latin-700.eot",
    "open-sans-v13-latin-700.ttf",
    "open-sans-v13-latin-700.woff",
    "open-sans-v13-latin-700i.eot",
    "open-sans-v13-latin-700i.ttf",
    "open-sans-v13-latin-700i.woff",
    "source-code-pro-v6-latin-700.eot",
    "source-code-pro-v6-latin-700.ttf",
    "source-code-pro-v6-latin-700.woff",
    "source-code-pro-v6-latin-regular.eot",
    "source-code-pro-v6-latin-regular.ttf",
    "source-code-pro-v6-latin-regular.woff",
    "MaterialIcons-Regular.eot",
    "MaterialIcons-Regular.ttf",
    "MaterialIcons-Regular.woff",

    "index.js",
    "jquery.mousewheel.min.js",
    "jquery.panzoom.min.js",
    "scheduler.js",
    "diagrams.js",
    "template.js",
    "modernizr.custom.js",

    "index.css",
    "ref-index.css",
    "template.css",
    "diagrams.css",

    "class_diagram.png",
    "object_diagram.png",
    "trait_diagram.png",
    "type_diagram.png",

    "ownderbg2.gif",
    "ownerbg.gif",
    "ownerbg2.gif"
  )

  final def webjarResources = List(
    ("jquery.min.js", "9/aliU8dGd2tb6OSsuzixeV4y/faTqgFtohetphbbj0=")
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

    def copyWebjarResource(resourceName: String, expectedSRI: String): Unit = {
      import java.security.MessageDigest
      import java.util.Base64
      val md = MessageDigest.getInstance("SHA-256")
      val base64encoder = Base64.getEncoder

      // https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity
      def calsSubResourceIntegrity(input: String): String = {
        val messageDigest = md.digest(input.getBytes)
        val base64encoded = base64encoder.encode(messageDigest)
        new String(base64encoded, "UTF-8")
      }

      val bytes = new Streamable.Bytes {
        val p = "/" + resourceName
        val inputStream = getClass.getResourceAsStream(p)
        assert(inputStream != null, p)
      }.toByteArray()
      val fileContent = new String(bytes)
      val actualSRI = calsSubResourceIntegrity(fileContent)
      if (expectedSRI != actualSRI)
        throw new Exception(s"Subresource Integrity unmatched on ${resourceName}. Could be wrong webjar or hijacked: $actualSRI")

      val dest = Directory(siteRoot) / "lib" / resourceName
      dest.parent.createDirectory()
      dest.toFile.writeAll(fileContent)
    }

    libResources foreach (s => copyResource("lib/" + s))
    webjarResources foreach { case (resourceName, integrity) =>
      copyWebjarResource(resourceName, integrity)
    }

    IndexScript(universe) writeFor this

    try {
      writeTemplates(_ writeFor this)
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
        writeForThis(page.EntityPage(universe, diagramGenerator, tpl, reporter))
        written += tpl
        tpl.templates collect { case d: DocTemplateEntity => d } map writeTemplate
      }
    }

    writeTemplate(universe.rootPackage)
  }
}
