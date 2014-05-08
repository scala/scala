/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc.backend.jvm

import scala.tools.nsc.Global
import PartialFunction._

/**
 * This trait contains code shared between GenBCode and GenICode that depends on types defined in
 * the compiler cake (Global).
 */
final class BCodeICodeCommon[G <: Global](val global: G) {
  import global._

  /** Some useful equality helpers. */
  def isNull(t: Tree) = cond(t) { case Literal(Constant(null)) => true }
  def isLiteral(t: Tree) = cond(t) { case Literal(_) => true }
  def isNonNullExpr(t: Tree) = isLiteral(t) || ((t.symbol ne null) && t.symbol.isModule)

  /** If l or r is constant null, returns the other ; otherwise null */
  def ifOneIsNull(l: Tree, r: Tree) = if (isNull(l)) r else if (isNull(r)) l else null
}
