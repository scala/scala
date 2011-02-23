/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author Gilles Dubochet
 */

package scala.tools.nsc
package doc
package model


/** A value that is passed as an argument to a value parameter. */
trait ValueArgument {

  /** The parameter as argument to which this value is passed, if it is known. */
  def parameter: Option[ValueParam]

  /** The expression that calculates the value. */
  def value: TreeEntity

}
