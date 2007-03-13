/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc.backend.opt;

import scala.collection._
import scala.collection.immutable.{Map, HashMap, Set, HashSet};
import scala.tools.nsc.backend.icode.analysis.LubError;
import scala.tools.nsc.symtab._;

/**
 */
abstract class DeadCodeElimination extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "dce";

  /** Create a new phase */
  override def newPhase(p: Phase) = new DeadCodeEliminationPhase(p);

  /** Dead code elimination phase.
   */
  class DeadCodeEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val dce = new DeadCode();

    override def apply(c: IClass): Unit =
      if (settings.Xdce.value)
        dce.analyzeClass(c)
  }

  /** Remove dead code.
   */
  class DeadCode {
    def analyzeClass(cls: IClass): Unit =
      cls.methods.foreach { m =>
//        analyzeMethod(m);
	collectRDef(m)
      }

    val a = new liveness.LivenessAnalysis();
    val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis;

    /** Use-def chain: give the reaching definitions at the beginning of given instruction. */
    var defs: Map[(BasicBlock, Int), Set[rdef.lattice.Definition]] = HashMap.empty

    /** Useful instructions which have not been scanned yet. */
    val worklist: mutable.Set[(BasicBlock, Int)] = new mutable.HashSet

    /** collect reaching definitions and initial useful instructions for this method. */
    def collectRDef(m: IMethod): Unit = if (m.code ne null) {
      log("rdef on " + m);
      defs = HashMap.empty; worklist.clear
      rdef.init(m);
      rdef.run;

      for (val bb <- m.code.blocks.toList) {
        var rd = rdef.in(bb);
        Console.println("** Block " + bb + " **")
        for (val Pair(i, idx) <- bb.toList.zipWithIndex) {
          i match {
            case LOAD_LOCAL(l) => defs((bb, idx)) = rd
            case RETURN(_) => worklist += ((bb, idx))
            case CALL_METHOD(m, _) if isSideEffecting(m) => worklist += ((bb, idx))
            case _ => ()
          }
          rd = rdef.interpret(bb, idx, rd);
        }
      }
    }

    /** Mark useful instructions. Instructions in the worklist are each inspected and their
     *  dependecies are marked useful too, and added to the worklist.
     */
    def mark {
      while (!worklist.isEmpty) {
        val (bb, idx) = worklist.elements.next
        worklist -= ((bb, idx))

        val instr = bb(idx)
        assert(!instr.useful)
        instr match {
          case LOAD_LOCAL(l1) =>
            for (val (_, bb1, idx1) <- defs((bb, idx)); !bb1(idx1).useful)
              worklist += ((bb1, idx1))
          case _ =>
            for (val (bb1, idx1) <- bb.findDefs(idx, 0); !bb1(idx1).useful)
              worklist += ((bb1, idx1))
        }
      }
    }

    def analyzeMethod(m: IMethod): Unit = if (m.code ne null) {
      log("DCE on " + m);
      a.init(m);
      a.run;

      for (val bb <- m.code.blocks.toList) {
        var live = a.out(bb);
        for (val Pair(i, pos) <- bb.toList.zipWithIndex.reverse) {
          i match {
            case STORE_LOCAL(l) if (!live(l)) =>
              removeDefUse(bb, i);
            case _ => ()
          }
          live = a.interpret(live, i);
        }
      }
    }

    /** Remove a pair def-use, if safe to do so. The `use' is given by index
     *  in the basic block. The `def' is the closest previous instruction which
     *  produced the top value on the stack.
     */
    def removeDefUse(bb: BasicBlock, use: Instruction): Unit = {
      val usePos = bb.indexOf(use)
      bb.findDef(usePos) match {
        case Some(defPos) =>
          val definition = bb(defPos);
          if (isSafeToRemove(definition)) {
            log("Removing instructions at BB " + bb + " def: " + definition + ", use: " + use);

            if (definition.consumed == 0) {
              bb.removeInstructionsAt(defPos, usePos)
            } else {
              bb.removeInstructionsAt(usePos);
              bb.replaceInstruction(definition, definition.consumedTypes map DROP);
            }
          }
        case None =>
          bb.replaceInstruction(use, use.consumedTypes map DROP);
          log("Replaced dead " + use + " by DROP in bb " + bb);
      }
    }

    /** Is 'sym' a side-effecting method? TODO: proper analysis.  */
    private def isSideEffecting(sym: Symbol): Boolean = {
      sym.isClassConstructor // for testing only
    }

    def isSafeToRemove(i: Instruction): Boolean = i match {
/*      case LOAD_LOCAL(l) => true
      case LOAD_FIELD(_, _) => true
      case THIS(_) => true
*/
      case CALL_METHOD(m, style) =>
        (m.isClassConstructor &&
         definitions.refClass.values.contains(m.owner));
      case _ => true;
    }

  } /* DeadCode */
}
