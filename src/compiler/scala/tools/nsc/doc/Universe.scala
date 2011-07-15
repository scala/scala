/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
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
