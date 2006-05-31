/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id: $

package scala.tools.nsc.backend.opt;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.backend.icode.analysis.LubError;
import scala.tools.nsc.symtab._;

/**
 */
abstract class DeadCodeElimination extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "deadcode";

  /** Create a new phase */
  override def newPhase(p: Phase) = new DeadCodeEliminationPhase(p);

  /** Dead code elimination phase.
   */
  class DeadCodeEliminationPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName;
    override def newFlags = phaseNewFlags;

    override def erasedTypes = true;
		val dce = new DeadCode();

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]");
      if (settings.Xdce.value)
        classes.values foreach dce.analyzeClass;
    }
    override def apply(unit: CompilationUnit): Unit =
      abort("Dead code elimination works on icode classes, not on compilation units!");
  }

  /** Remove dead code.
   */
  class DeadCode {
    def analyzeClass(cls: IClass): Unit =
      cls.methods.foreach { m =>
        analyzeMethod(m);
      }

    val a = new liveness.LivenessAnalysis();

    def analyzeMethod(m: IMethod): Unit = if (m.code ne null) {
      log("DCE on " + m);
      a.init(m);
      a.run;

      for (val bb <- m.code.blocks.toList;
           val Pair(i, pos) <- bb.toList.zipWithIndex.reverse) {
        var live = a.out(bb);

        i match {
          case STORE_LOCAL(l) if (!live(l)) =>
            removeDefUse(bb, pos);
          case _ => ()
        }
        live = a.interpret(live, i);
      }
    }

    /** Remove a pair def-use, if safe to do so. The `use' is given by index
     *  in the basic block. The `def' is the closest previous instruction which
     *  produced the top value on the stack.
     */
    def removeDefUse(bb: BasicBlock, use: Int): Unit = {
      bb.findDef(use) match {
        case Some(defPos) =>
          val definition = bb(defPos);
          if (isSafeToRemove(definition)) {
            log("Removing instructions at BB " + bb + " def: " + definition + ", use: " + bb(use));

            if (definition.consumed == 0) {
              bb.removeInstructionsAt(defPos, use)
            } else {
              bb.replaceInstruction(definition, definition.consumedTypes map DROP);
              bb.removeInstructionsAt(use);
            }
          }
        case None =>
        	val i = bb(use);
          bb.replaceInstruction(i, i.consumedTypes map DROP);
          log("Replaced dead " + i + " by DROP in bb " + bb);
      }
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