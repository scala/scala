/* NSC -- new scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc
package backend.opt

import scala.collection._
import scala.collection.immutable.{Map, HashMap, Set, HashSet}
import scala.tools.nsc.backend.icode.analysis.LubError
import scala.tools.nsc.symtab._

/**
 */
abstract class DeadCodeElimination extends SubComponent {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "dce"

  /** Create a new phase */
  override def newPhase(p: Phase) = new DeadCodeEliminationPhase(p)

  /** Dead code elimination phase.
   */
  class DeadCodeEliminationPhase(prev: Phase) extends ICodePhase(prev) {

    def name = phaseName
    val dce = new DeadCode()

    override def apply(c: IClass) {
      if (settings.Xdce.value)
        dce.analyzeClass(c)
    }
  }

  /** closures that are instantiated at least once, after dead code elimination */
  val liveClosures: mutable.Set[Symbol] = new mutable.HashSet()

  /** Remove dead code.
   */
  class DeadCode {

    def analyzeClass(cls: IClass) {
      cls.methods.foreach { m =>
        this.method = m
        dieCodeDie(m)
      }
    }

    val rdef = new reachingDefinitions.ReachingDefinitionsAnalysis;

    /** Use-def chain: give the reaching definitions at the beginning of given instruction. */
    var defs: Map[(BasicBlock, Int), Set[rdef.lattice.Definition]] = HashMap.empty

    /** Useful instructions which have not been scanned yet. */
    val worklist: mutable.Set[(BasicBlock, Int)] = new mutable.LinkedHashSet

    /** what instructions have been marked as useful? */
    val useful: mutable.Map[BasicBlock, mutable.BitSet] = new mutable.HashMap

    /** what local variables have been accessed at least once? */
    var accessedLocals: List[Local] = Nil

    /** the current method. */
    var method: IMethod = _

    /** Map instructions who have a drop on some control path, to that DROP instruction. */
    val dropOf: mutable.Map[(BasicBlock, Int), (BasicBlock, Int)] = new mutable.HashMap()

    def dieCodeDie(m: IMethod) {
      if (m.code ne null) {
        log("dead code elimination on " + m);
        dropOf.clear
        m.code.blocks.clear
        accessedLocals = m.params.reverse
        m.code.blocks ++= linearizer.linearize(m)
        collectRDef(m)
        mark
        sweep(m)
        accessedLocals = accessedLocals.removeDuplicates
        if ((m.locals -- accessedLocals).length > 0) {
          log("Removed dead locals: " + (m.locals -- accessedLocals))
          m.locals = accessedLocals.reverse
        }
      }
    }

    /** collect reaching definitions and initial useful instructions for this method. */
    def collectRDef(m: IMethod): Unit = if (m.code ne null) {
      defs = HashMap.empty; worklist.clear; useful.clear;
      rdef.init(m);
      rdef.run;

      for (bb <- m.code.blocks.toList) {
        useful(bb) = new mutable.BitSet(bb.size)
        var rd = rdef.in(bb);
        for (Pair(i, idx) <- bb.toList.zipWithIndex) {
          i match {
            case LOAD_LOCAL(l) =>
              defs = defs + Pair(((bb, idx)), rd.vars)
//              Console.println(i + ": " + (bb, idx) + " rd: " + rd + " and having: " + defs)
            case RETURN(_) | JUMP(_) | CJUMP(_, _, _, _) | CZJUMP(_, _, _, _) | STORE_FIELD(_, _) |
                 THROW()   | STORE_ARRAY_ITEM(_) | SCOPE_ENTER(_) | SCOPE_EXIT(_) | STORE_THIS(_) |
                 LOAD_EXCEPTION() | SWITCH(_, _) | MONITOR_ENTER() | MONITOR_EXIT() => worklist += ((bb, idx))
            case CALL_METHOD(m1, _) if isSideEffecting(m1) => worklist += ((bb, idx)); log("marking " + m1)
            case CALL_METHOD(m1, SuperCall(_)) =>
              worklist += ((bb, idx)) // super calls to constructor
            case DROP(_) =>
              val necessary = rdef.findDefs(bb, idx, 1) exists { p =>
                val (bb1, idx1) = p
                bb1(idx1) match {
                  case CALL_METHOD(m1, _) if isSideEffecting(m1) => true
                  case LOAD_EXCEPTION() | DUP(_) | LOAD_MODULE(_) => true
                  case _ =>
                    dropOf((bb1, idx1)) = (bb, idx)
//                    println("DROP is innessential: " + i + " because of: " + bb1(idx1) + " at " + bb1 + ":" + idx1)
                    false
                }
              }
              if (necessary) worklist += ((bb, idx))
            case _ => ()
          }
          rd = rdef.interpret(bb, idx, rd)
        }
      }
    }

    /** Mark useful instructions. Instructions in the worklist are each inspected and their
     *  dependecies are marked useful too, and added to the worklist.
     */
    def mark {
//      log("Starting with worklist: " + worklist)
      while (!worklist.isEmpty) {
        val (bb, idx) = worklist.iterator.next
        worklist -= ((bb, idx))
        if (settings.debug.value)
          log("Marking instr: \tBB_" + bb + ": " + idx + " " + bb(idx))

        val instr = bb(idx)
        if (!useful(bb)(idx)) {
          useful(bb) += idx
          dropOf.get(bb, idx) match {
            case Some((bb1, idx1)) => useful(bb1) += idx1
            case None => ()
          }
          instr match {
            case LOAD_LOCAL(l1) =>
              for ((l2, bb1, idx1) <- defs((bb, idx)) if l1 == l2; if !useful(bb1)(idx1)) {
                log("\tAdding " + bb1(idx1))
                worklist += ((bb1, idx1))
              }

            case nw @ NEW(REFERENCE(sym)) =>
              assert(nw.init ne null, "null new.init at: " + bb + ": " + idx + "(" + instr + ")")
              worklist += findInstruction(bb, nw.init)
              if (inliner.isClosureClass(sym)) {
                liveClosures += sym
              }
            case LOAD_EXCEPTION() =>
              ()

            case _ =>
              for ((bb1, idx1) <- rdef.findDefs(bb, idx, instr.consumed) if !useful(bb1)(idx1)) {
                log("\tAdding " + bb1(idx1))
                worklist += ((bb1, idx1))
              }
          }
        }
      }
    }

    def sweep(m: IMethod) {
      val compensations = computeCompensations(m)

      for (bb <- m.code.blocks.toList) {
//        Console.println("** Sweeping block " + bb + " **")
        val oldInstr = bb.toList
        bb.open
        bb.clear
        for (Pair(i, idx) <- oldInstr.zipWithIndex) {
          if (useful(bb)(idx)) {
//            log(" " + i + " is useful")
            bb.emit(i, i.pos)
            compensations.get(bb, idx) match {
              case Some(is) => is foreach bb.emit
              case None => ()
            }
            // check for accessed locals
            i match {
              case LOAD_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case STORE_LOCAL(l) if !l.arg =>
                accessedLocals = l :: accessedLocals
              case _ => ()
            }
          } else {
            i match {
              case NEW(REFERENCE(sym)) =>
                log("skipped object creation: " + sym + "inside " + m)
              case _ => ()
            }
            if (settings.debug.value) log("Skipped: bb_" + bb + ": " + idx + "( " + i + ")")
          }
        }

        if (bb.size > 0)
          bb.close
        else
          log("empty block encountered")
      }
    }

    private def computeCompensations(m: IMethod): mutable.Map[(BasicBlock, Int), List[Instruction]] = {
      val compensations: mutable.Map[(BasicBlock, Int), List[Instruction]] = new mutable.HashMap

      for (bb <- m.code.blocks.toList) {
        assert(bb.closed, "Open block in computeCompensations")
        for ((i, idx) <- bb.toList.zipWithIndex) {
          if (!useful(bb)(idx)) {
            for ((consumedType, depth) <- i.consumedTypes.reverse.zipWithIndex) {
              log("Finding definitions of: " + i + "\n\t" + consumedType + " at depth: " + depth)
              val defs = rdef.findDefs(bb, idx, 1, depth)
              for (d <- defs) {
                if (!compensations.isDefinedAt(d))
                  compensations(d) = List(DROP(consumedType))
              }
            }
          }
        }
      }
      compensations
    }

    private def withClosed[a](bb: BasicBlock)(f: => a): a = {
      if (bb.size > 0) bb.close
      val res = f
      if (bb.size > 0) bb.open
      res
    }

    private def findInstruction(bb: BasicBlock, i: Instruction): (BasicBlock, Int) = {
      def find(bb: BasicBlock): Option[(BasicBlock, Int)] = {
        var xs = bb.toList
        xs.zipWithIndex find { hd => hd._1 eq i } match {
          case Some((_, idx)) => Some(bb, idx)
          case None => None
        }
      }

      for (b <- linearizer.linearizeAt(method, bb))
        find(b) match {
          case Some(p) => return p
          case None => ()
        }
      abort("could not find init in: " + method)
    }

    lazy val RuntimePackage = definitions.getModule("scala.runtime")
    /** Is 'sym' a side-effecting method? TODO: proper analysis.  */
    private def isSideEffecting(sym: Symbol): Boolean = {
      !((sym.isGetter && !sym.hasFlag(Flags.LAZY))
       || (sym.isConstructor
           && !(sym.owner == method.symbol.owner && method.symbol.isConstructor) // a call to another constructor
           && sym.owner.owner == RuntimePackage.moduleClass)
       || (sym.isConstructor && inliner.isClosureClass(sym.owner))
/*       || definitions.isBox(sym)
       || definitions.isUnbox(sym)*/)
    }
  } /* DeadCode */
}
