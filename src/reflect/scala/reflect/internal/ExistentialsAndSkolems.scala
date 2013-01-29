/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import scala.collection.{ mutable, immutable }
import util._

/** The name of this trait defines the eventual intent better than
 *  it does the initial contents.
 */
trait ExistentialsAndSkolems {
  self: SymbolTable =>

  /** Map a list of type parameter symbols to skolemized symbols, which
   *  can be deskolemized to the original type parameter. (A skolem is a
   *  representation of a bound variable when viewed inside its scope.)
   *  !!!Adriaan: this does not work for hk types.
   */
  def deriveFreshSkolems(tparams: List[Symbol]): List[Symbol] = {
    class Deskolemizer extends LazyType {
      override val typeParams = tparams
      val typeSkolems  = typeParams map (_.newTypeSkolem setInfo this)
      override def complete(sym: Symbol) {
        // The info of a skolem is the skolemized info of the
        // actual type parameter of the skolem
        sym setInfo sym.deSkolemize.info.substSym(typeParams, typeSkolems)
      }
    }
    (new Deskolemizer).typeSkolems
  }
}
