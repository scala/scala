/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.tools.nsc.symtab._;

/** Glue together ICode parts.
 */
abstract class ICodes extends AnyRef
                                 with Members
                                 with BasicBlocks
                                 with Opcodes
                                 with TypeStacks
                                 with TypeKinds
                                 with ExceptionHandlers
                                 with Primitives
                                 with Linearizers
{
  val global: Global;

  /** The ICode representation of classes */
  var classes: List[IClass] = _;

  /** The ICode linearizer. */
  val linearizer: Linearizer =
    if (global.settings.Xlinearizer.value == "rpo")
      new ReversePostOrderLinearizer();
    else if (global.settings.Xlinearizer.value == "dfs")
      new DepthFirstLinerizer();
    else if (global.settings.Xlinearizer.value == "normal")
      new NormalLinearizer();
    else
      global.abort("Unknown linearizer: " + global.settings.Xlinearizer.value);

  def init = { classes = Nil }
}

