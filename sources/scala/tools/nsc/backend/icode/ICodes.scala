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
{
  val global: Global;

  /** The ICode representation of classes */
  var classes: List[IClass] = Nil;
}

