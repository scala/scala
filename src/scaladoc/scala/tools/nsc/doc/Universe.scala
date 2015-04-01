/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.doc

import scala.tools.nsc.doc.html.page.diagram.DotRunner

/**
 * Class to hold common dependencies across Scaladoc classes.
 * @author Pedro Furlanetto
 * @author Gilles Dubochet
 */
trait Universe {
  def settings: Settings
  def rootPackage: model.Package
  def dotRunner: DotRunner
}
