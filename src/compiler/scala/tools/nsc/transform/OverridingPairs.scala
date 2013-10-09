/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab.Flags._
import scala.reflect.internal.SymbolPairs

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields pairs of corresponding symbols visible in some base class,
 *  unless there's a parent class that already contains the same pairs.
 *  Most of the logic is in SymbolPairs, which contains generic
 *  pair-oriented traversal logic.
 */
abstract class OverridingPairs extends SymbolPairs {
  import global._

  class Cursor(base: Symbol) extends super.Cursor(base) {
    lazy val relatively = new RelativeTo(base.thisType)

    /** Symbols to exclude: Here these are constructors and private/artifact symbols,
     *  including bridges. But it may be refined in subclasses.
     */
    override protected def exclude(sym: Symbol) = (sym hasFlag PRIVATE | ARTIFACT) || sym.isConstructor

    /** Types always match. Term symbols match if their member types
     *  relative to `self` match.
     */
    override protected def matches(sym1: Symbol, sym2: Symbol) = sym1.isType || (
         (sym1.owner != sym2.owner)
      && !exclude(sym2)
      && relatively.matches(sym1, sym2)
    )
  }
}
