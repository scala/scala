/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import java.io.PrintWriter;

import scala.tools.nsc.symtab._;
import scala.collection.mutable.HashMap;

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
  var classes: HashMap[global.Symbol, IClass] = new HashMap();

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


  /** Print all classes and basic blocks. Used for debugging. */
  def dump: Unit = {
    val printer = new global.icodePrinter.TextPrinter(new PrintWriter(System.out, true),
                                                      new global.icodes.DumpLinearizer());

    global.icodes.classes.values foreach { c => printer.printClass(c); }
  }


  def init = { }
}

