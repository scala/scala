/**
 * @author Damien Obrist
 * @author Vlad Ureche
 */
package scala.tools.nsc
package doc
package html
package page
package diagram

import scala.xml.NodeSeq
import scala.tools.nsc.doc.html.HtmlPage
import scala.tools.nsc.doc.model.diagram.Diagram
import scala.tools.nsc.doc.model.DocTemplateEntity

trait DiagramGenerator {

  /**
   * Generates a visualization of the internal representation
   * of a diagram.
   *
   * @param d   The model of the diagram
   * @param p   The page the diagram will be embedded in (needed for link generation)
   * @return    The HTML to be embedded in the Scaladoc page
   */
  def generate(d: Diagram, t: DocTemplateEntity, p: HtmlPage):NodeSeq
}

object DiagramGenerator {

  // TODO: This is tailored towards the dot generator, since it's the only generator. In the future it should be more
  // general.

  private[this] var dotRunner: DotRunner = null
  private[this] var settings: doc.Settings = null

  def initialize(s: doc.Settings) =
    settings = s

  def getDotRunner() = {
    if (dotRunner == null)
      dotRunner = new DotRunner(settings)
    dotRunner
  }

  def cleanup() = {
    DiagramStats.printStats(settings)
    if (dotRunner != null) {
      dotRunner.cleanup()
      dotRunner = null
    }
  }
}