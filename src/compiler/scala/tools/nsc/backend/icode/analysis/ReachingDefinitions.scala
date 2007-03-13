/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id:  $

package scala.tools.nsc.backend.icode.analysis

import compat.StringBuilder
import scala.collection.mutable.{HashMap, Map}
import scala.collection.immutable.{Set, ListSet, HashSet}

/** Compute reaching definitions. We are only interested in reaching
 *  definitions for local variables, since values on the stack
 *  behave as-if in SSA form: the closest instruction which produces a value
 *  on the stack is a reaching definition.
 */
abstract class ReachingDefinitions {
  val global: Global
  import global._
  import icodes._

  /** The lattice for reaching definitions. Elements are
   *  a triple (local variable, basic block, index of instruction of that basic block)
   */
  object rdefLattice extends CompleteLattice {
    type Definition = (Local, BasicBlock, Int)
    type Elem = Set[Definition]

    val top: Elem = new ListSet[Definition]() {
      override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    }

    val bottom: Elem = new ListSet[Definition]() {
      override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]
    }

    def lub2(a: Elem, b: Elem): Elem = a incl b
  }

  class ReachingDefinitionsAnalysis extends DataFlowAnalysis[rdefLattice.type] {
    type P = BasicBlock
    val lattice = rdefLattice
    import lattice.Definition
    import lattice.Elem

    var method: IMethod = _

    val gen: Map[BasicBlock, Set[Definition]] = new HashMap()
    val kill:Map[BasicBlock, Set[Local]]      = new HashMap()

    def init(m: IMethod): Unit = {
      this.method = m
      gen.clear
      kill.clear

      for (val b <- m.code.blocks.toList;
           val (g, k) = genAndKill(b)) {
        gen  += b -> g
        kill += b -> k
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

    def genAndKill(b: BasicBlock): (Set[Definition], Set[Local]) = {
      var genSet: Set[Definition] = new HashSet
      var killSet: Set[Local] = new HashSet
      for (val (i, idx) <- b.toList.zipWithIndex) i match {
        case STORE_LOCAL(local) =>
          killSet = killSet + local
          genSet  = updateReachingDefinition(b, idx, genSet)
        case _ => ()
      }
      (genSet, killSet)
    }

    override def run: Unit = {
      forwardAnalysis(blockTransfer)
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.code.startBlock)
          assert(lattice.bottom != in(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited?"));
      }
    }

    import opcodes._
    def updateReachingDefinition(b: BasicBlock, idx: Int, rd: Set[Definition]): Set[Definition] = {
      val STORE_LOCAL(local) = b(idx)
      var tmp = local
      (rd filter { case (l, _, _) => l != tmp }) + ((tmp, b, idx))
    }

    private def blockTransfer(b: BasicBlock, in: Set[Definition]): Set[Definition] =
      (in filter { case (l, _, _) => !kill(b)(l) }) incl gen(b)

    /** Return the reaching definitions corresponding to the point after idx. */
    def interpret(b: BasicBlock, idx: Int, in: lattice.Elem): Elem = {
      var out = in
      b(idx) match {
        case STORE_LOCAL(l1) =>
          out = updateReachingDefinition(b, idx, out)
        case _ =>
          ()
      }

      out
    }

    override def toString: String = {
      val sb = new compat.StringBuilder
      sb.append("rdef: \n")
      for (val b <- method.code.blocks)
        sb.append("rdef_entry(" + b + ")= " + in(b)).append("\nrdef_exit(" + b + ")= " + out(b))
      sb.toString()
    }

  }
}
