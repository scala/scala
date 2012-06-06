/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package reify

import scala.Array.canBuildFrom
import scala.compat.Platform.EOL
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.Global

trait NodePrinters { self: scala.tools.nsc.ast.NodePrinters =>

  val global: Global
  import global._

  object reifiedNodeToString extends Function2[Tree, Tree, String] {
    def apply(prefix: Tree, tree: Tree): String = {
      "temporarily disabled until reification is repaired"
    }
  }
}