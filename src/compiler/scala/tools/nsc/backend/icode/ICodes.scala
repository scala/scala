/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import java.io.PrintWriter;

import scala.tools.nsc.symtab._;
import scala.collection.mutable.HashMap;
import analysis.Liveness;

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

  def dump(m: global.icodes.IMethod) = {
    val printer = new global.icodePrinter.TextPrinter(new PrintWriter(System.out, true),
                                                      new global.icodes.DumpLinearizer());
    printer.printMethod(m);
  }

  /** Merge together blocks that have a single successor which has a
   * single predecessor. Exception handlers are taken into account (they
   * might force to break a block of straight line code like that).
   *
   * This method should be most effective after heavy inlining.
   */
  def normalize(m: IMethod): Unit = if (m.code ne null) {
    Console.println("Method " + m);
    val mergeablePairs =
      for (val b <- m.code.blocks.toList;
         b.successors.length == 1;
         val succ = b.successors.head;
         succ.predecessors.length == 1;
         succ.predecessors.head == b;
         !(m.exh.contains { (e: ExceptionHandler) => e.covers(b) && !e.covers(succ) }))
        yield Pair(b, succ)
    ()
  }

  object liveness extends Liveness {
    val global: ICodes.this.global.type = ICodes.this.global;
  }

  def init = { }
}

