/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend.icode
package analysis

import scala.collection.{ mutable, immutable }
import immutable.ListSet

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
  object livenessLattice extends SemiLattice {
    type Elem = Set[Local]

    object top extends ListSet[Local] with ReferenceEquality
    object bottom extends ListSet[Local] with ReferenceEquality

    def lub2(exceptional: Boolean)(a: Elem, b: Elem): Elem = a ++ b
  }

  final class LivenessAnalysis extends DataFlowAnalysis[livenessLattice.type] {
    type P = BasicBlock
    val lattice                                   = livenessLattice
    var method: IMethod                           = _
    val gen: mutable.Map[BasicBlock, Set[Local]]  = perRunCaches.newMap()
    val kill: mutable.Map[BasicBlock, Set[Local]] = perRunCaches.newMap()

    def init(m: IMethod) {
      this.method = m
      gen.clear()
      kill.clear()

      m foreachBlock { b =>
        val (g, k) = genAndKill(b)
        gen  += (b -> g)
        kill += (b -> k)
      }

      init {
        m foreachBlock { b =>
          worklist += b
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

    override def run() {
      backwardAnalysis(blockTransfer)
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.startBlock)
          assert(lattice.bottom != in(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited?"));
      }
    }

    def blockTransfer(b: BasicBlock, out: lattice.Elem): lattice.Elem =
      gen(b) ++ (out -- kill(b))

    /** Abstract interpretation for one instruction. Very important:
     *  liveness is a backward DFA, so this method should be used to compute
     *  liveness *before* the given instruction `i`.
     */
    def interpret(out: lattice.Elem, i: Instruction): lattice.Elem = {
      debuglog("- " + i + "\nout: " + out + "\n")
      i match {
        case LOAD_LOCAL(l)  => out + l
        case STORE_LOCAL(l) => out - l
        case _              => out
      }
    }
    override def toString() =
      (method.blocks map (b => "\nlive-in(%s)=%s\nlive-out(%s)=%s".format(b, in(b), b, out(b)))).mkString
  } /* Liveness analysis */
}
