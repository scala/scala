/* NSC -- new scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Iulian Dragos
 */

// $Id$

package scala.tools.nsc.backend.opt

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

    override def apply(c: IClass): Unit =
      if (settings.Xdce.value)
        dce.analyzeClass(c)
  }

  /** Remove dead code.
   */
  class DeadCode {

    def analyzeClass(cls: IClass): Unit = {
      cls.methods.foreach { m =>
        this.method = m
//        analyzeMethod(m);
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

    def dieCodeDie(m: IMethod): Unit = if (m.code ne null) {
      log("dead code elimination on " + m);
//      (new DepthFirstLinerizer).linearize(m)
      m.code.blocks.clear
      accessedLocals = m.params.reverse
      m.code.blocks ++= linearizer.linearize(m)
      collectRDef(m)
      mark
      sweep(m)
      if (m.locals.diff(accessedLocals).length > 0) {
        log("Removed dead locals: " + m.locals.diff(accessedLocals))
        m.locals = accessedLocals.reverse
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
              defs = defs + ((bb, idx)) -> rd._1
//              Console.println(i + ": " + (bb, idx) + " rd: " + rd + " and having: " + defs)
            case RETURN(_) | JUMP(_) | CJUMP(_, _, _, _) | CZJUMP(_, _, _, _) | STORE_FIELD(_, _) |
                 DROP(_) | THROW()   | STORE_ARRAY_ITEM(_) | SCOPE_ENTER(_) | SCOPE_EXIT(_) |
                 LOAD_EXCEPTION() | SWITCH(_, _) | MONITOR_ENTER() | MONITOR_EXIT() => worklist += ((bb, idx))
            case CALL_METHOD(m1, _) if isSideEffecting(m1) => worklist += ((bb, idx))
            case CALL_METHOD(m1, SuperCall(_)) =>
              worklist += ((bb, idx)) // super calls to constructor
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
//      log("Starting with worklist: " + worklist)
      while (!worklist.isEmpty) {
        val (bb, idx) = worklist.elements.next
        worklist -= ((bb, idx))
        if (settings.debug.value)
          log("Marking instr: \tBB_" + bb + ": " + idx + " " + bb(idx))

        val instr = bb(idx)
        if (!useful(bb)(idx)) {
          useful(bb) += idx
          instr match {
            case LOAD_LOCAL(l1) =>
              for ((l2, bb1, idx1) <- defs((bb, idx)) if l1 == l2; if !useful(bb1)(idx1))
                worklist += ((bb1, idx1))

            case nw @ NEW(_) =>
              assert(nw.init ne null, "null new.init at: " + bb + ": " + idx + "(" + instr + ")")
              worklist += findInstruction(bb, nw.init)

            case LOAD_EXCEPTION() =>
              ()

            case _ =>
              for ((bb1, idx1) <- findDefs(bb, idx, instr.consumed) if !useful(bb1)(idx1))
                worklist += ((bb1, idx1))
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
                log("skipped object creation: " + sym)
                //Console.println("skipping class file altogether: " + sym.fullNameString)
                if (inliner.isClosureClass(sym))
                  icodes.classes -= sym
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
        assert(bb.isClosed, "Open block in computeCompensations")
        for ((i, idx) <- bb.toList.zipWithIndex) {
          if (!useful(bb)(idx)) {
            val defs = findDefs(bb, idx, i.consumed)
            for (d <- defs) {
              if (!compensations.isDefinedAt(d))
                compensations(d) = i.consumedTypes map DROP
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
          case Some(_, idx) => Some(bb, idx)
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

    def findDefs(bb: BasicBlock, idx: Int, m: Int): List[(BasicBlock, Int)] = if (idx > 0) {
      assert(bb.isClosed)
      var instrs = bb.getArray
      var res: List[(BasicBlock, Int)] = Nil
      var i = idx
      var n = m
      var d = 0
      // "I look for who produced the 'n' elements below the 'd' topmost slots of the stack"
      while (n > 0 && i > 0) {
        i -= 1
        val prod = instrs(i).produced
        if (prod > d) {
          res = (bb, i) :: res
          n   = n - (prod - d)
          if (bb(i) != LOAD_EXCEPTION)
            d = instrs(i).consumed
        } else {
          d -= prod
          d += instrs(i).consumed
        }
      }

      if (n > 0) {
        val stack = rdef.in(bb)._2
        assert(stack.length >= n, "entry stack is too small, expected: " + n + " found: " + stack)
        stack.drop(d).take(n) foreach { defs =>
          res = defs.toList ::: res
        }
      }
      res
    } else {
      val stack = rdef.in(bb)._2
      assert(stack.length >= m, "entry stack is too small, expected: " + m + " found: " + stack)
      stack.take(m) flatMap (_.toList)
    }

    /** Is 'sym' a side-effecting method? TODO: proper analysis.  */
    private def isSideEffecting(sym: Symbol): Boolean = {
      !(sym.isGetter // for testing only
       || (sym.isConstructor
           && sym.owner.owner == definitions.getModule("scala.runtime").moduleClass)
       || (sym.isConstructor && inliner.isClosureClass(sym.owner)))
    }
  } /* DeadCode */
}
