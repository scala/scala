/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend.icode.analysis

import scala.collection.mutable.{HashMap, Map}
import scala.collection.immutable.{Set, ListSet}

/**
 * Compute liveness information for local variables.
 *
 * @author Iulian Dragos
 */
abstract class Liveness {
  val global: Global
  import global._
  import icodes._

  /** The lattice for this analysis.   */
  object livenessLattice extends CompleteLattice {
    type Elem = Set[Local]

    val top: Elem = new ListSet[Local]() {
      override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    }

    val bottom: Elem = new ListSet[Local]() {
      override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    }

    def lub2(exceptional: Boolean)(a: Elem, b: Elem): Elem = a ++ b
  }

  final class LivenessAnalysis extends DataFlowAnalysis[livenessLattice.type] {
    type P = BasicBlock
    val lattice = livenessLattice

    var method: IMethod = _

    val gen: Map[BasicBlock, Set[Local]] = new HashMap()
    val kill:Map[BasicBlock, Set[Local]] = new HashMap()

    def init(m: IMethod) {
      this.method = m
      gen.clear
      kill.clear

      for (b <- m.code.blocks.toList;
           (g, k) = genAndKill(b)) {
        gen  += (b -> g)
        kill += (b -> k)
      }

      init {
        worklist ++= m.code.blocks.toList
        m.code.blocks.foreach { b =>
          in(b)  = lattice.bottom
          out(b) = lattice.bottom
        }
      }
    }

    import opcodes._

    /** Return the gen and kill sets for this block. */
    def genAndKill(b: BasicBlock): (Set[Local], Set[Local]) = {
      var genSet = new ListSet[Local]
      var killSet = new ListSet[Local]
      for (i <- b) i match {
        case LOAD_LOCAL(local)  if (!killSet(local)) => genSet = genSet + local
        case STORE_LOCAL(local) if (!genSet(local))  => killSet = killSet + local
        case _ => ()
      }
      Pair(genSet, killSet)
    }

    override def run {
      backwardAnalysis(blockTransfer)
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.code.startBlock)
          assert(lattice.bottom != in(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited?"));
      }
    }

    def blockTransfer(b: BasicBlock, out: lattice.Elem): lattice.Elem =
      gen(b) ++ (out -- kill(b))

    /** Abstract interpretation for one instruction. Very important:
     *  liveness is a backward DFA, so this method should be used to compute
     *  liveness *before* the given instruction `i'.
     */
    def interpret(out: lattice.Elem, i: Instruction): lattice.Elem = {
      var in = out

      if (settings.debug.value) {
        log("- " + i)
        log("out: " + out)
        log("\n")
      }

      i match {
        case LOAD_LOCAL(l) => in += l
        case STORE_LOCAL(l) => in -= l
        case _ =>
          ()
      }
      in
    } /* def interpret */

    override def toString(): String = {
      val buf = new StringBuilder()
      for (b <- method.code.blocks.toList) {
        buf.append("\nlive-in(" + b + ")=" + in(b) + "\nlive-out(" + b + ")=" + out(b));
      }
      buf.toString()
    }
  } /* Liveness analysis */
}
