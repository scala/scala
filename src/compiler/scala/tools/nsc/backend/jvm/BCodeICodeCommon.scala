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
final class BCodeICodeCommon[I <: BackendInterface](val global: I) {
  import global._

  def isNull(t: Tree) = global.isNull(t)
  def isLiteral(t: Tree) = global.isLiteral(t)
  def isNonNullExpr(t: Tree) = global.isNonNullExpr(t)

  def ifOneIsNull(l: Tree, r: Tree) = global.ifOneIsNull(l,r)
}
