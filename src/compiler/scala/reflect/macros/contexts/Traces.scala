/* NSC -- new Scala compiler
 * Copyright 2012-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect.macros
package contexts

trait Traces extends util.Traces {
  self: Context =>

  def globalSettings = universe.settings
}
