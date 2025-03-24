/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
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
package page
package diagram

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
  def generate(d: Diagram, t: DocTemplateEntity, p: HtmlPage): HtmlTags.Elems
}
