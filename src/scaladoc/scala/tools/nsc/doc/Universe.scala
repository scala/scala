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
