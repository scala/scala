/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import scala.tools.util.Position;

class TreeCheckers: Global {

  object treeChecker extends Traverser {
    override def traverse(tree: Tree): unit = {
      if (tree.pos == Position.NOPOS)
	throw new FatalError("tree without position: " + tree)
      else if (tree.tpe == null && phase.id >= typeCheckPhase.id)
	throw new FatalError("tree without type: " + tree)
    }
  }
}

