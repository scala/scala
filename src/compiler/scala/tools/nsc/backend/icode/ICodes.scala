/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend
package icode

/** Glue together ICode parts.
 *
 *  @author Iulian Dragos
 */
abstract class ICodes extends AnyRef with Members with Primitives {
  val global: Global
}

