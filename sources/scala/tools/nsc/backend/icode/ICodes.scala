/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.backend.icode;

/** Glue together ICode parts.
 */
abstract class ICodes: Global extends AnyRef
                                 with Members
                                 with BasicBlocks
                                 with Opcodes
                                 with TypeStacks
{
  import opcodes._;

  /** The ICode representation of classes */
  var classes: List[IClass] = Nil;
}

