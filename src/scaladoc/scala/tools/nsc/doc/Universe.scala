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

package scala.tools.nsc.doc

/**
 * Class to hold common dependencies across Scaladoc classes.
 * @author Pedro Furlanetto
 * @author Gilles Dubochet
 */
trait Universe {
  def settings: Settings
  def rootPackage: model.Package
}
